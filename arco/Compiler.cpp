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
#include "Generics.h"
#include "Files.h"
#include "Process.h"

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
        if (!CreateDirectories(OutputDirectory, "output")) {
            return 1;
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
        if (!TestsStdLibPath.empty()) {
            Sources.push_back(Source{ false, "std", TestsStdLibPath });
        } else if (auto StdLibPath = GetStdLibPath()) {
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
    if (FailedToFindSources) {
        return 1;
    }

    if (Stage == PARSE_ONLY) {
        return 0;
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
        Context.LLArcoModule.print(llvm::outs(), nullptr);
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

    // First checking that the sources exists and if they don't then just stopping compilation
    // since references and the like will be wrong.
    for (const Source& Source : Sources) {
        const char* FLPath = Source.Path.data();
        fs::path Path = fs::path(std::string_view(FLPath, Source.Path.size()));

        std::error_code EC;
        if (!fs::exists(Path, EC) || EC) {
            if (EC) {
                Logger::GlobalError(llvm::errs(),
                    "Could not check if source \"%s\" exist. Please check permissions", Source.Path);
            } else {
                Logger::GlobalError(llvm::errs(),
                    "Source \"%s\" does not exist", Source.Path);
            }
            FailedToFindSources = true;
        }
    }
    if (FailedToFindSources) {
        return;
    }


    for (const Source& Source : Sources) {
        const char* FLPath = Source.Path.data();
        fs::path Path = fs::path(std::string_view(FLPath, Source.Path.size()));

        Module* Mod = Context.ModNamesToMods[Source.ModName];
        if (fs::is_directory(Path)) {
            if (Source.PartOfMainProject) {
                std::string PathS = Path.generic_string();
                ParseDirectoryFiles(Mod, Path, PathS.length() + (PathS.back() == '/' ? 0 : 1));
            } else {
                std::string PathS = Path.has_parent_path() ? Path.parent_path().generic_string()
                                                           : Path.generic_string();
                ParseDirectoryFiles(Mod, Path, PathS.length() + (PathS.back() == '/' ? 0 : 1));
            }
        } else {
            // The user specified an absolute path to a file.
            if (Path.extension() != ".arco") {
                Logger::GlobalError(llvm::errs(),
                        "Expected source file with extension type .arco for file: \"%s\"", Source.Path);
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
        if (!FoundCompileError) {
            IRGenerator IRGen(Context);
            IRGen.GenFuncDecl(Context.MainEntryFunc);
        }
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

        if (DToGen.D->Is(AstKind::FUNC_DECL)) {
            FuncDecl* Func = static_cast<FuncDecl*>(DToGen.D);
            if (Func->IsGeneric()) {
                // NOTE: It is not actually needed to bind the types of
                // the generic struct because the binding information store
                // the binding's of the struct and will also bind those generic
                // types.
                BindTypes(Func, DToGen.Binding);
                Func->LLFunction = DToGen.Binding->FuncInfo->LLFunction;
            }
            SemAnalyzer Analyzer(Context, DToGen.D);
            Analyzer.CheckFuncDecl(Func);
        }
        SemCheckIn += GetTimeInMilliseconds() - SemCheckBegin;

        IRGenBegin = GetTimeInMilliseconds();
        if (FoundCompileError || Stage == PARSE_SEMCHECK_ONLY) {
            if (DToGen.D->Is(AstKind::FUNC_DECL)) {
                FuncDecl* Func = static_cast<FuncDecl*>(DToGen.D);
                if (Func->IsGeneric()) {
                    UnbindTypes(Func);
                }
            }
            continue;
        }

        IRGenerator IRGen(Context);
        if (DToGen.D->Is(AstKind::FUNC_DECL)) {
            FuncDecl* Func = static_cast<FuncDecl*>(DToGen.D);
            IRGen.GenFuncBody(Func);
            if (Func->IsGeneric()) {
                UnbindTypes(Func);
            }
        } else if (DToGen.D->Is(AstKind::VAR_DECL)) {
            IRGen.GenGlobalVar(static_cast<VarDecl*>(DToGen.D));
        }
        IRGenIn += GetTimeInMilliseconds() - IRGenBegin;
    }

    IRGenBegin = GetTimeInMilliseconds();

    if (!FoundCompileError && Stage != PARSE_SEMCHECK_ONLY) {
        IRGenerator IRGen(Context);
        IRGen.GenGlobalInitFuncBody();
        IRGen.GenGlobalDestroyFuncBody();
    }

    for (llvm::Function* LLDiscardFunc : Context.LLDiscardFuncs) {
        LLDiscardFunc->eraseFromParent();
    }

    IRGenIn += GetTimeInMilliseconds() - IRGenBegin;

    SemCheckBegin = GetTimeInMilliseconds();
    // Checking any code that was not generated.
    Context.CheckingUnhcecked = true;
    while (!Context.UncheckedDecls.empty()) {
        Decl* D = *Context.UncheckedDecls.begin();
        SemAnalyzer Analyzer(Context, D);
        if (D->Is(AstKind::FUNC_DECL)) {
            FuncDecl* Func = static_cast<FuncDecl*>(D);
            if (!Func->IsGeneric()) {
                Analyzer.CheckFuncDecl(Func);
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
    Context.StdTypeIdEnum        = FindStdLibEnum(ReflectNamespace, Context.TypeIdIdentifier);
    if (Context.StdAnyStruct) {
        Context.AnyType = StructType::Create(Context.StdAnyStruct, {}, Context);
        SemAnalyzer::FinishNonGenericStructType(Context, Context.AnyType);
    }

    if (Context.StdErrorInterface) {
        StructType* ErrorInterfaceTy = StructType::Create(Context.StdErrorInterface, Context);
        Context.ErrorInterfacePtrType = PointerType::Create(ErrorInterfaceTy, Context);
    }

    auto Itr = StdModule->DefaultNamespace->Funcs.find(Identifier("initialize_error_handling"));
    if (Itr == StdModule->DefaultNamespace->Funcs.end()) {
        Logger::GlobalError(llvm::errs(), "Standard library is missing 'initialize_error_handling' function");
    } else {
        FuncsList& Funcs = Itr->second;
        if (Funcs.size() > 1) {
            Logger::GlobalError(llvm::errs(), "Standard library cannot have more than one initialize_error_handling function");
        }
        Context.InitializeErrorHandlingFunc = Funcs[0];
        IRGenerator IRGen(Context);
        IRGen.GenFuncDecl(Context.InitializeErrorHandlingFunc);
        Context.RequestGen(Context.InitializeErrorHandlingFunc);
    }

    if (NumErrs == TotalAccumulatedErrors) {
        Context.StdStringStructType     = StructType::Create(Context.StdStringStruct    , {}, Context);
        Context.StdAnyStructType        = StructType::Create(Context.StdAnyStruct       , {}, Context);
        Context.StdTypeStructType       = StructType::Create(Context.StdTypeStruct      , {}, Context);
        Context.StdArrayTypeStructType  = StructType::Create(Context.StdArrayTypeStruct , {}, Context);
        Context.StdStructTypeStructType = StructType::Create(Context.StdStructTypeStruct, {}, Context);
        Context.StdFieldTypeStructType  = StructType::Create(Context.StdFieldTypeStruct , {}, Context);
        Context.StdEnumTypeStructType   = StructType::Create(Context.StdEnumTypeStruct  , {}, Context);

        SemAnalyzer::FinishNonGenericStructType(Context, Context.StdStringStructType);
        SemAnalyzer::FinishNonGenericStructType(Context, Context.StdAnyStructType);
        SemAnalyzer::FinishNonGenericStructType(Context, Context.StdTypeStructType);
        SemAnalyzer::FinishNonGenericStructType(Context, Context.StdArrayTypeStructType);
        SemAnalyzer::FinishNonGenericStructType(Context, Context.StdStructTypeStructType);
        SemAnalyzer::FinishNonGenericStructType(Context, Context.StdFieldTypeStructType);
        SemAnalyzer::FinishNonGenericStructType(Context, Context.StdEnumTypeStructType);

        // Finding the constructor of StdStringStruct which takes the cstr as an argument.
        for (FuncDecl* Constructor : Context.StdStringStruct->Constructors) {
            if (Constructor->Params.size() != 1) continue;
            if (Constructor->Params[0]->Ty->Equals(Context.CStrType)) {
                Context.StdStringCStrConstructor = Constructor;
                SemAnalyzer::RequestGenNonGenericFunc(Context, Context.StdStringCStrConstructor);
                break;
            }
        }
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

arco::EnumDecl* arco::Compiler::FindStdLibEnum(Namespace* Namespace, Identifier Name) {
    auto Itr = Namespace->Decls.find(Name);
    if (Itr == Namespace->Decls.end() || Itr->second->IsNot(AstKind::ENUM_DECL)) {
        Logger::GlobalError(llvm::errs(), "Standard library is missing '%s' enum", Name);
        return nullptr;
    }
    return static_cast<EnumDecl*>(Itr->second);
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
