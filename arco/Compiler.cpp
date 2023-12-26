#include "Compiler.h"

#include <filesystem>
#include <fstream>
#include <iostream>

#include "Context.h"
#include "Parser.h"
#include "SemAnalysis.h"
#include "IRGen.h"
#include "CodeGen.h"

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

void arco::Compiler::Compile(llvm::SmallVector<Source>& Sources) {
	
	i64 ParseTimeBegin = GetTimeInMilliseconds();

	Context.Initialize();

	if (!StandAlone) {
		if (auto StdLibPath = GetStdLibPath()) {
			Sources.push_back(Source{ "std", StdLibPath });
		} else {
			Logger::GlobalError(llvm::errs(),
				"Environment variable missing for arco's standard library. Please set variable 'ArcoStdLibPath' to point towards the standard library");
			return;
		}
	}

	// Create a module for every source.
	for (const Source& Source : Sources) {
		if (Context.ModNamesToMods.find(Source.ModName) == Context.ModNamesToMods.end()) {
			Context.ModNamesToMods.insert({ Source.ModName, new Module() });
		}
	}

	// Creating FileUnits for the .eris files
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
			std::string PathS = Path.generic_string();
			std::string RootDir = PathS.empty() ? "." : PathS;
			
			ParseDirectoryFiles(Mod, FLPath, PathS.length() + (PathS.back() == '/' ? 0 : 1));
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

	i64 ParsedIn = GetTimeInMilliseconds() - ParseTimeBegin;
	i64 CheckAndIRGenTimeBegin = GetTimeInMilliseconds();

	// Mapping the imports to the structs within different files.
	for (FileScope* FScope : FileScopes) {
		SemAnalyzer::ResolveStructImports(FScope);
	}

	if (Context.MainEntryFunc) {
		Context.RequestGen(Context.MainEntryFunc);
	} else {
		Logger::GlobalError(llvm::errs(), "Could not find entry point function");
		return;
	}

	while (!Context.QueuedDeclsToGen.empty()) {
		Decl* D = Context.QueuedDeclsToGen.front();
		Context.QueuedDeclsToGen.pop();

		SemAnalyzer Analyzer(Context, D);
		if (D->Is(AstKind::FUNC_DECL)) {
			Analyzer.CheckFuncDecl(static_cast<FuncDecl*>(D));
		}
	
		if (FoundCompileError) {
			continue;
		}

		IRGenerator IRGen(Context);	
		if (D->Is(AstKind::FUNC_DECL)) {
			IRGen.GenFunc(static_cast<FuncDecl*>(D));
		} else if (D->Is(AstKind::VAR_DECL)) {
			IRGen.GenGlobalVar(static_cast<VarDecl*>(D));
		}
	}

	// Checking any code that was not generated.
	while (!Context.UncheckedDecls.empty()) {
		Decl* D = *Context.UncheckedDecls.begin();
		SemAnalyzer Analyzer(Context, D);
		if (D->Is(AstKind::FUNC_DECL)) {
			Analyzer.CheckFuncDecl(static_cast<FuncDecl*>(D));
		} else if (D->Is(AstKind::VAR_DECL)) {
			Analyzer.CheckVarDecl(static_cast<VarDecl*>(D));
		} else if (D->Is(AstKind::STRUCT_DECL)) {
			Analyzer.CheckStructDecl(static_cast<StructDecl*>(D));
		}
	}

	if (FoundCompileError) {
		return;
	}

	IRGenerator IRGen(Context);
	while (!Context.DefaultConstrucorsNeedingCreated.empty()) {
		StructDecl* Struct = Context.DefaultConstrucorsNeedingCreated.front();
		Context.DefaultConstrucorsNeedingCreated.pop();
		IRGen.GenImplicitDefaultConstructorBody(Struct);
	}

	IRGen.GenGlobalInitFuncBody();

	i64 CheckAndIRGenIn = GetTimeInMilliseconds() - CheckAndIRGenTimeBegin;
	i64 EmiteMachineCodeTimeBegin = GetTimeInMilliseconds();

	if (FoundCompileError) {
		return;
	}

	if (DisplayLLVMIR) {
		Context.LLArcoModule.dump();
		llvm::outs() << "\n\n";
	}

	if (!InitLLVMNativeTarget()) {
		Logger::GlobalError(llvm::errs(), "Failed to initialized LLVM native target");
		return;
	}

	i64 EmiteMachineCodeIn = GetTimeInMilliseconds() - EmiteMachineCodeTimeBegin;
	i64 LinkTimeBegin = GetTimeInMilliseconds();

	if (!LLMachineTarget) {
		LLMachineTarget = CreateLLVMTargetMache();
	}
	
	SetTargetToModule(Context.LLArcoModule, LLMachineTarget);

	if (OutputName.ends_with(".exe")) {
		OutputName = OutputName.substr(0, OutputName.length() - 4);
	}

	std::string ObjFileName = OutputName + ".o";
	if (!WriteObjFile(ObjFileName.c_str(), Context.LLArcoModule, LLMachineTarget)) {
		FoundCompileError = true;
		return;
	}

	std::string ExecutableName = OutputName;
#ifdef _WIN32
	ExecutableName += ".exe";
#endif

	std::string ClangCommand = "clang ";
	ClangCommand += ObjFileName;
	ClangCommand += " -o ";
	ClangCommand += ExecutableName;

	llvm::outs() << ClangCommand << "\n";

	i32 ExitCode = system(ClangCommand.c_str());
	if (ExitCode) {
		// Failed to link
		FoundCompileError = true;
		return;
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

		i64 TotalTime = ParsedIn + CheckAndIRGenIn + EmiteMachineCodeIn + LinkedIn;

		llvm::SmallVector<std::streamsize> Counts;
		Counts.push_back(CountDigits(ParsedIn));
		Counts.push_back(CountDigits(CheckAndIRGenIn));
		Counts.push_back(CountDigits(EmiteMachineCodeIn));
		Counts.push_back(CountDigits(LinkedIn));
		Counts.push_back(CountDigits(TotalTime));
		std::streamsize MaxCount = *std::max_element(std::begin(Counts), std::end(Counts));

		std::cout << "Parsed Time:       "  << std::setw(MaxCount) << std::left << ParsedIn           << " ms.  |  " << std::fixed << std::setprecision(3) << (ParsedIn           / 1000.0f) << " s." << '\n';
		std::cout << "Checks/IRGen Time: "  << std::setw(MaxCount) << std::left << CheckAndIRGenIn    << " ms.  |  " << std::fixed << std::setprecision(3) << (CheckAndIRGenIn    / 1000.0f) << " s." << '\n';
		std::cout << "Code Emit Time:    "  << std::setw(MaxCount) << std::left << EmiteMachineCodeIn << " ms.  |  " << std::fixed << std::setprecision(3) << (EmiteMachineCodeIn / 1000.0f) << " s." << '\n';
		std::cout << "Link Time:         "  << std::setw(MaxCount) << std::left << LinkedIn           << " ms.  |  " << std::fixed << std::setprecision(3) << (LinkedIn           / 1000.0f) << " s." << '\n';
		std::cout << "-------------------" << std::string(MaxCount, '-') << "------|--" << std::string(MaxCount+2, '-') << "---" << '\n';
		std::cout << "Total time:        " << std::setw(MaxCount) << std::left << TotalTime << " ms.  |  " << std::fixed << std::setprecision(3) << (TotalTime / 1000.0f) << " s." << '\n';
		std::cout << '\n';
	}

	if (ShouldDeleteObjectFiles) {
		std::remove(ObjFileName.c_str());
	}

	llvm::outs() << "Wrote program to: "
		<< std::filesystem::absolute(std::filesystem::current_path()).generic_string().c_str()
		<< '/' << ExecutableName << '\n';

	if (RunProgram) {
#ifdef _WIN32
		system(ExecutableName.c_str());
#endif
	}
}

void arco::Compiler::ParseDirectoryFiles(Module* Mod, const std::filesystem::path& DirectoryPath, ulen PrimaryPathLen) {
	for (const auto& Entry : std::filesystem::directory_iterator(DirectoryPath)) {
		if (Entry.is_regular_file()) {
			const std::string& Path = Entry.path().generic_string();

			if (Path.substr(Path.find_last_of('.') + 1) == "arco") {
				const std::string& RelativePath = Path.substr(PrimaryPathLen);
				const std::string& AbsolutePath = std::filesystem::absolute(Entry.path()).generic_string();

				ParseFile(Mod, RelativePath, AbsolutePath);
			}
		} else if (Entry.is_directory()) {
			ParseDirectoryFiles(Mod, Entry.path(), PrimaryPathLen);
		}
	}
}

void arco::Compiler::ParseFile(Module* Mod, const std::string& RelativePath, const std::string& AbsolutePath) {
	SourceBuf Buffer;
	if (!ReadFile(AbsolutePath.c_str(), Buffer.Memory, Buffer.length)) {
		Logger::GlobalError(llvm::errs(), "Failed to read file: %s. Check permissions", AbsolutePath.c_str());	
		return;
	}

	Parser Parser(Context, Mod, RelativePath.c_str(), Buffer);
	FileScope* FScope = Parser.Parse();

	if (!FScope->ParsingErrors) {
		SemAnalyzer::ReportStatementsInInvalidContext(FScope);
	}

	FileScopes.push_back(FScope);
}

const char* arco::Compiler::GetStdLibPath() {
	return std::getenv("ArcoStdLibPath");
}
