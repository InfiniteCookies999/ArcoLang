#include "Compiler.h"

#include <filesystem>
#include <fstream>
#include <iostream>
#include <llvm/IR/Verifier.h>

#include "Context.h"
#include "Parser.h"
#include "SemAnalysis.h"
#include "IRGen.h"
#include "CodeGen.h"
#include "FloatConversions.h"
#include "EmitDebugInfo.h"

static bool ReadFile(const std::string& Path, char*& Buffer, u64& Size) {
    std::ifstream Stream(Path, std::ios::binary | std::ios::in);
    if (!Stream.good()) {
        return false;
    }
    Stream.seekg(0, std::ios::end);
    Size = Stream.tellg();
    Buffer = new char[Size + 1];
    Stream.seekg(0, std::ios::beg);
    Stream.read(Buffer, Size);
    Buffer[Size] = 0; // Null termination
    return true;
}


static i64 GetTimeInMilliseconds() {
    using std::chrono::duration_cast;
    using std::chrono::milliseconds;
    using std::chrono::seconds;
    using std::chrono::system_clock;	

    return duration_cast<milliseconds>(system_clock::now().time_since_epoch()).count();
}

// Creating a global instance because for some reason LLVM
// does not clean up target machines properly when deleting
// them
static llvm::TargetMachine* LLMachineTarget = nullptr;


arco::Compiler::Compiler()
    : Context(*new ArcoContext)
{
}

arco::Compiler::~Compiler() {
    delete &Context;
}

void arco::Compiler::PreInitContext() {
    Context.Initialize();
    FD::InitializeCache();
    ContextInitialized = true;
}

int arco::Compiler::Compile(llvm::SmallVector<Source>& Sources) {
    
    i64 ParseTimeBegin = GetTimeInMilliseconds();

    if (!OutputDirectory.empty()) {
        std::error_code EC;
        if (!std::filesystem::exists(OutputDirectory, EC)) {
            if (!std::filesystem::create_directories(OutputDirectory, EC) || EC) {
                Logger::GlobalError(llvm::errs(), "Failed to create the output directory: '%s'",
                    OutputDirectory);
                return 1;
            }
        } else if (EC) {
            Logger::GlobalError(llvm::errs(), "Failed to check if the output directory exists");
            return 1;
        }
        if (OutputDirectory.ends_with('/')) {
            OutputDirectory = OutputDirectory.substr(0, OutputDirectory.size() - 2);
        }
    }

    if (!ContextInitialized) {
        Context.Initialize();
        FD::InitializeCache();
        ContextInitialized = true;
    }

    Context.EmitDebugInfo = EmitDebugInfo;
    Context.StandAlone    = StandAlone;

    if (!StandAlone) {
        if (auto StdLibPath = GetStdLibPath()) {
            Sources.push_back(Source{ false, "std", StdLibPath });
        } else {
            Logger::GlobalError(llvm::errs(),
                "Environment variable missing for arco's standard library. Please set variable 'ArcoStdLibPath' to point towards the standard library");
            return 1;
        }
    }

    // Create a module for every source.
    for (const Source& Source : Sources) {
        if (Context.ModNamesToMods.find(Source.ModName) == Context.ModNamesToMods.end()) {
            Module* Mod = new Module();
            Mod->Name = Source.ModName;
            Mod->DefaultNamespace = new Namespace;
            Context.ModNamesToMods.insert({ Source.ModName, Mod });
            Modules.push_back(Mod);
        }
    }

    ParseAllFiles(Sources);
    if (Stage == PARSE_ONLY) {
        return 1;
    }

    i64 ParsedIn = GetTimeInMilliseconds() - ParseTimeBegin;
    i64 SemCheckIn = 0, IRGenIn = 0;
    CheckAndGenIR(SemCheckIn, IRGenIn);

    i64 EmiteMachineCodeTimeBegin = GetTimeInMilliseconds();

    // -- DEBUG
    // llvm::verifyModule(Context.LLArcoModule, &llvm::errs());

    if (FoundCompileError) {
        return 1;
    }

    if (DisplayLLVMIR) {
        Context.LLArcoModule.dump();
        llvm::outs() << "\n\n";
    }

    if (OutputName.ends_with(".exe")) {
        OutputName = OutputName.substr(0, OutputName.length() - 4);
    }
    std::string ObjFileName = OutputName + ".o";

    std::string ExecutableName = OutputName;
#ifdef _WIN32
    ExecutableName += ".exe";
#endif

    std::string AbsOutputDirectory, AbsoluteExePath, AbsoluteObjPath;
    if (!OutputDirectory.empty()) {
        AbsOutputDirectory = std::filesystem::absolute(std::filesystem::path(OutputDirectory)).generic_string();
    } else {
        AbsOutputDirectory = std::filesystem::absolute(std::filesystem::current_path()).generic_string();
    }
    if (!AbsOutputDirectory.ends_with("/")) {
        AbsOutputDirectory += "/";
    }
    AbsoluteExePath = AbsOutputDirectory + ExecutableName;
    AbsoluteObjPath = AbsOutputDirectory + ObjFileName;

    GenCode(AbsoluteObjPath);

    if (Stage == PARSE_SEMCHECK_COMPILE_ONLY) {
        return 1;
    }

    i64 EmiteMachineCodeIn = GetTimeInMilliseconds() - EmiteMachineCodeTimeBegin;
    i64 LinkTimeBegin = GetTimeInMilliseconds();

    Linking(AbsoluteObjPath, AbsoluteExePath);
    if (FoundCompileError) {
        return 1;
    }

    if (ShouldDeleteObjectFiles) {
        std::remove(AbsoluteObjPath.c_str());
    }

    i64 LinkedIn = GetTimeInMilliseconds() - LinkTimeBegin;

    if (DisplayTimes) {
        auto CountDigits = [](i64 Value) -> std::streamsize {
            std::streamsize Count = 0;
            if (Value == 0) return 1;
            while (Value > 0) {
                Value /= 10;
                ++Count;
            }
            return Count;
        };

        i64 TotalTime = ParsedIn + SemCheckIn + IRGenIn + EmiteMachineCodeIn + LinkedIn;

        llvm::SmallVector<std::streamsize> Counts;
        Counts.push_back(CountDigits(ParsedIn));
        Counts.push_back(CountDigits(SemCheckIn));
        Counts.push_back(CountDigits(IRGenIn));
        Counts.push_back(CountDigits(EmiteMachineCodeIn));
        Counts.push_back(CountDigits(LinkedIn));
        Counts.push_back(CountDigits(TotalTime));
        std::streamsize MaxCount = *std::max_element(std::begin(Counts), std::end(Counts));

        std::cout << "Total Lines Parsed:  " << TotalLinesParsed << '.' << '\n';
        std::cout << "Parsed Time:         " << std::setw(MaxCount) << std::left << ParsedIn           << " ms.  |  " << std::fixed << std::setprecision(3) << (ParsedIn           / 1000.0f) << " s." << '\n';
        std::cout << "Semantic Check Time: " << std::setw(MaxCount) << std::left << SemCheckIn         << " ms.  |  " << std::fixed << std::setprecision(3) << (SemCheckIn         / 1000.0f) << " s." << '\n';
        std::cout << "LLVM IR Gen Time:    " << std::setw(MaxCount) << std::left << IRGenIn            << " ms.  |  " << std::fixed << std::setprecision(3) << (IRGenIn            / 1000.0f) << " s." << '\n';
        std::cout << "Code Emit Time:      " << std::setw(MaxCount) << std::left << EmiteMachineCodeIn << " ms.  |  " << std::fixed << std::setprecision(3) << (EmiteMachineCodeIn / 1000.0f) << " s." << '\n';
        std::cout << "Link Time:           " << std::setw(MaxCount) << std::left << LinkedIn           << " ms.  |  " << std::fixed << std::setprecision(3) << (LinkedIn           / 1000.0f) << " s." << '\n';
        std::cout << "---------------------" << std::string(MaxCount, '-') << "------|--" << std::string(MaxCount+2, '-') << "---" << '\n';
        std::cout << "Total time:          " << std::setw(MaxCount) << std::left << TotalTime << " ms.  |  " << std::fixed << std::setprecision(3) << (TotalTime / 1000.0f) << " s." << '\n';
        std::cout << '\n';
    }

    if (!NoWroteToDispaly) {
        llvm::outs() << "Wrote program to: " << AbsoluteExePath << '\n';
    }

    if (RunProgram || RunInSeperateWindow) {
        return ExeProcess(AbsoluteExePath.c_str(), AbsOutputDirectory.c_str(), RunInSeperateWindow);
    }
    return 0;
}

void arco::Compiler::ParseAllFiles(llvm::SmallVector<Source>& Sources) {
    namespace fs = std::filesystem;
    for (const Source& Source : Sources) {
        const char* FLPath = Source.Path.data();
        fs::path Path = fs::path(FLPath);

        std::error_code EC;
        if (!fs::exists(Path, EC) || EC) {
            if (EC) {
                Logger::GlobalError(llvm::errs(),
                    "Could not check if source \"%s\" exist. Please check permissions", FLPath);
            } else {
                Logger::GlobalError(llvm::errs(),
                    "Source \"%s\" does not exist", FLPath);
            }
            continue;
        }

        Module* Mod = Context.ModNamesToMods[Source.ModName];

        if (fs::is_directory(FLPath)) {
            if (Source.PartOfMainProject) {
                std::string PathS = Path.generic_string();
                ParseDirectoryFiles(Mod, FLPath, PathS.length() + (PathS.back() == '/' ? 0 : 1));
            } else {
                std::string PathS = Path.has_parent_path() ? Path.parent_path().generic_string()
                                                           : Path.generic_string();
                ParseDirectoryFiles(Mod, FLPath, PathS.length() + (PathS.back() == '/' ? 0 : 1));
            }
        } else {
            // The user specified an absolute path to a file.
            if (Path.extension() != ".arco") {
                Logger::GlobalError(llvm::errs(),
                        "Expected source file with extension type .arco for file: \"%s\"", FLPath);
                continue;
            }

            ParseFile(Mod,
                      Path.filename().generic_string(),
                      fs::absolute(Path).generic_string());
        }
    }
}

void arco::Compiler::CheckAndGenIR(i64& SemCheckIn, i64& IRGenIn) {

    i64 SemCheckBegin, IRGenBegin;

    SemCheckBegin = GetTimeInMilliseconds();
    if (!StandAlone) {
        if (!FindStdLibStructs()) {
            return;
        }
    }
    
    // Mapping the imports to the structs within different files.
    for (FileScope* FScope : FileScopes) {
        SemAnalyzer::ResolveImports(FScope, Context);
    }

    SemCheckIn += GetTimeInMilliseconds() - SemCheckBegin;


    IRGenBegin = GetTimeInMilliseconds();
    // Must do this early so that LLVM can correctly determine information for types
    // during generating.
    if (!InitLLVMNativeTarget()) {
        Logger::GlobalError(llvm::errs(), "Failed to initialized LLVM native target");
        return;
    }

    if (!LLMachineTarget) {
        LLMachineTarget = CreateLLVMTargetMache();
    }
    
    SetTargetToModule(Context.LLArcoModule, LLMachineTarget);
    IRGenIn += GetTimeInMilliseconds() - IRGenBegin;

    SemCheckBegin = GetTimeInMilliseconds();
    if (Context.MainEntryFunc) {
        Context.RequestGen(Context.MainEntryFunc);
    } else {
        Logger::GlobalError(llvm::errs(), "Could not find entry point function");
    }
    SemCheckIn += GetTimeInMilliseconds() - SemCheckBegin;

    IRGenBegin = GetTimeInMilliseconds();
    // Creating debug units for each file.
    if (EmitDebugInfo) {
        for (FileScope* FScope : FileScopes) {
            FScope->DIEmitter = new DebugInfoEmitter(Context);
            // NOTE: We do not generate the compilation units here anymore
            //       they are generated on demand as needed.
        }
    }
    IRGenIn += GetTimeInMilliseconds() - IRGenBegin;

    while (!Context.QueuedDeclsToGen.empty()) {
        SemCheckBegin = GetTimeInMilliseconds();
        auto DToGen = Context.QueuedDeclsToGen.front();
        Context.QueuedDeclsToGen.pop();

        SemAnalyzer Analyzer(Context, DToGen.D);
        if (DToGen.D->Is(AstKind::FUNC_DECL)) {
            Analyzer.CheckFuncDecl(static_cast<FuncDecl*>(DToGen.D), DToGen.Binding);
        }
        SemCheckIn += GetTimeInMilliseconds() - SemCheckBegin;
    
        IRGenBegin = GetTimeInMilliseconds();
        if (FoundCompileError || Stage == PARSE_SEMCHECK_ONLY) {
            continue;
        }

        IRGenerator IRGen(Context);	
        if (DToGen.D->Is(AstKind::FUNC_DECL)) {
            IRGen.GenFunc(static_cast<FuncDecl*>(DToGen.D), DToGen.Binding);
        } else if (DToGen.D->Is(AstKind::VAR_DECL)) {
            IRGen.GenGlobalVar(static_cast<VarDecl*>(DToGen.D));
        }
        IRGenIn += GetTimeInMilliseconds() - IRGenBegin;
    }

    IRGenBegin = GetTimeInMilliseconds();
    
    if (!FoundCompileError && Stage != PARSE_SEMCHECK_ONLY) {
        IRGenerator IRGen(Context);
        IRGen.GenGlobalInitFuncBody();

        while (!Context.DefaultConstrucorsNeedingCreated.empty()) {
            StructDecl* Struct = Context.DefaultConstrucorsNeedingCreated.front();
            Context.DefaultConstrucorsNeedingCreated.pop();
            IRGen.GenImplicitDefaultConstructorBody(Struct);
        }

        IRGen.GenGlobalDestroyFuncBody();
    }
    
    IRGenIn += GetTimeInMilliseconds() - IRGenBegin;

    SemCheckBegin = GetTimeInMilliseconds();
    // Checking any code that was not generated.
    while (!Context.UncheckedDecls.empty()) {
        Decl* D = *Context.UncheckedDecls.begin();
        SemAnalyzer Analyzer(Context, D);
        if (D->Is(AstKind::FUNC_DECL)) {
            FuncDecl* Func = static_cast<FuncDecl*>(D);
            if (!Func->IsGeneric()) {
                Analyzer.CheckFuncDecl(Func, nullptr);
            } else {
                Context.UncheckedDecls.erase(Func);
            }
        } else if (D->Is(AstKind::VAR_DECL)) {
            Analyzer.CheckVarDecl(static_cast<VarDecl*>(D));
        } else if (D->Is(AstKind::STRUCT_DECL)) {
            Analyzer.CheckStructDecl(static_cast<StructDecl*>(D));
        } else if (D->Is(AstKind::ENUM_DECL)) {
            Analyzer.CheckEnumDecl(static_cast<EnumDecl*>(D));
        } else if (D->Is(AstKind::INTERFACE_DECL)) {
            Analyzer.CheckInterfaceDecl(static_cast<InterfaceDecl*>(D));
        }
    }

    for (Module* Mod : Modules) {
        SemAnalyzer::CheckForDuplicateFuncDeclarations(Mod);
    }
    SemCheckIn += GetTimeInMilliseconds() - SemCheckBegin;

    if (FoundCompileError) {
        return;
    }

    IRGenBegin = GetTimeInMilliseconds();
    if (EmitDebugInfo && Stage != PARSE_SEMCHECK_ONLY) {
#ifdef _WIN32
        Context.LLArcoModule.addModuleFlag(llvm::Module::Warning, "CodeView", 1);
#endif
        Context.LLArcoModule.addModuleFlag(llvm::Module::Warning, "Debug Info Version", llvm::DEBUG_METADATA_VERSION);
        llvm::NamedMDNode* LLVMIdentMD = Context.LLArcoModule.getOrInsertNamedMetadata("llvm.ident");
		LLVMIdentMD->addOperand(llvm::MDNode::get(Context.LLContext, { llvm::MDString::get(Context.LLContext, "Arco Compiler")}));
    
        // Finalizing all files for debug.
        for (FileScope* FScope : FileScopes) {
            FScope->DIEmitter->Finalize();
        }
    }
    IRGenIn += GetTimeInMilliseconds() - IRGenBegin;
}

void arco::Compiler::GenCode(const std::string& ObjFileName) {
    if (!WriteObjFile(ObjFileName.c_str(), Context.LLArcoModule, LLMachineTarget)) {
        FoundCompileError = true;
    }
}

void arco::Compiler::Linking(const std::string& AbsoluteObjPath, const std::string& AbsoluteExePath) {
    std::string Libs = "";
    for (const char* Lib : Libraries) {
        Libs += std::string("-l") + std::string(Lib) + " ";
    }
    std::string LibPaths = "";
    for (const char* LibPath : LibrarySearchPaths) {
        LibPaths += std::string("-L") + std::string(LibPath) + " ";
    }

    std::string ClangCommand = "clang -O0 "; // TODO: -O0 should not be needed.
    if (EmitDebugInfo)
        ClangCommand += "-g ";
    ClangCommand += LibPaths + Libs + AbsoluteObjPath;
    ClangCommand += " -o ";
    ClangCommand +=  AbsoluteExePath;
    
    if (ShowLinkCommand) {
        llvm::outs() << ClangCommand << "\n";
    }

    std::string Ignored;
    i32 ExitCode = ExeProcess(ClangCommand.c_str(), NULL, false);
    if (ExitCode) {
        // Failed to link
        FoundCompileError = true;
        return;
    }
}

void arco::Compiler::ParseDirectoryFiles(Module* Mod, const std::filesystem::path& DirectoryPath, ulen PrimaryPathLen) {
    for (const auto& Entry : std::filesystem::directory_iterator(DirectoryPath)) {
        if (Entry.is_regular_file()) {
            const std::string& Path = Entry.path().generic_string();

            if (Path.substr(Path.find_last_of('.') + 1) == "arco") {
                std::string RelativePath = Path.substr(PrimaryPathLen);
                std::string AbsolutePath = std::filesystem::absolute(Entry.path()).generic_string();

                ParseFile(Mod, std::move(RelativePath), std::move(AbsolutePath));
            }
        } else if (Entry.is_directory()) {
            ParseDirectoryFiles(Mod, Entry.path(), PrimaryPathLen);
        }
    }
}

void arco::Compiler::ParseFile(Module* Mod, std::string RelativePath, std::string AbsolutePath) {
    SourceBuf Buffer;
    if (!ReadFile(AbsolutePath.c_str(), Buffer.Memory, Buffer.length)) {
        Logger::GlobalError(llvm::errs(), "Failed to read file: %s. Check permissions", AbsolutePath.c_str());	
        return;
    }

    FileScope* FScope = new FileScope(std::move(RelativePath), std::move(AbsolutePath), Buffer);
    Parser Parser(Context, FScope, Mod, Buffer);
    Parser.Parse();
    TotalLinesParsed += Parser.GetLinesParsed();

    if (!FScope->ParsingErrors) {
        SemAnalyzer::ReportStatementsInInvalidContext(FScope);
    }

    FileScopes.push_back(FScope);
}

bool arco::Compiler::FindStdLibStructs() {
    Module* StdModule = Context.ModNamesToMods.find("std")->second;
    auto NSpaceItr = StdModule->Namespaces.find(Identifier("reflect"));
    if (NSpaceItr == StdModule->Namespaces.end()) {
        Logger::GlobalError(llvm::errs(), "Standard library is missing the 'reflect' namespace");
        return false;
    }
    Namespace* ReflectNamespace = NSpaceItr->second;
    ulen NumErrs = TotalAccumulatedErrors;

    Context.StdStringStruct      = FindStdLibStruct(StdModule->DefaultNamespace, Context.StringIdentifier);
    Context.StdAnyStruct         = FindStdLibStruct(ReflectNamespace, Context.AnyIdentifier);
    Context.StdTypeStruct        = FindStdLibStruct(ReflectNamespace, Context.TypeIdentifier);
    Context.StdArrayTypeStruct   = FindStdLibStruct(ReflectNamespace, Context.ArrayTypeIdentifier);
    Context.StdStructTypeStruct  = FindStdLibStruct(ReflectNamespace, Context.StructTypeIdentifier);
    Context.StdFieldTypeStruct   = FindStdLibStruct(ReflectNamespace, Context.FieldTypeIdentifier);
    Context.StdEnumTypeStruct    = FindStdLibStruct(ReflectNamespace, Context.EnumTypeIdentifier);
    Context.StdErrorInterface    = FindStdLibInterface(StdModule->DefaultNamespace, Context.ErrorInterfaceIdentifier);
    Context.AnyType              = StructType::Create(Context.StdAnyStruct, Context);
    
    if (Context.StdErrorInterface) {
        StructType* ErrorInterfaceTy = StructType::Create(Context.StdErrorInterface, Context);
        Context.ErrorInterfacePtrType = PointerType::Create(ErrorInterfaceTy, Context);
    }

    return NumErrs == TotalAccumulatedErrors;
}

arco::StructDecl* arco::Compiler::FindStdLibStruct(Namespace* Namespace, Identifier Name) {
    auto Itr = Namespace->Decls.find(Name);
    if (Itr == Namespace->Decls.end() || Itr->second->IsNot(AstKind::STRUCT_DECL)) {
        Logger::GlobalError(llvm::errs(), "Standard library is missing '%s' struct", Name);
        return nullptr;
    }
    return static_cast<StructDecl*>(Itr->second);
}

arco::InterfaceDecl* arco::Compiler::FindStdLibInterface(Namespace* Namespace, Identifier Name) {
    auto Itr = Namespace->Decls.find(Name);
    if (Itr == Namespace->Decls.end() || Itr->second->IsNot(AstKind::INTERFACE_DECL)) {
        Logger::GlobalError(llvm::errs(), "Standard library is missing '%s' interface", Name);
        return nullptr;
    }
    return static_cast<InterfaceDecl*>(Itr->second);
}

const char* arco::Compiler::GetStdLibPath() {
    return std::getenv("ArcoStdLibPath");
}
