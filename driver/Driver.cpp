#include "Compiler.h"
#include "Logger.h"
#include "TermColors.h"

#include "Options.h"

const char* HELP_MESSAGE =
R"(
Usage: arco <options> <sources>
Basic Options (for more options use -more-options):

    -out-name=<name>
        Sets the name of the executable.

    -run
        Executes the program after it is compiled and linked.
        Or use -run-seperate to run in a seperate window.

    -more-options
        Show more options that can be ran with arco.

Sources:
    Sources may either be absolute paths to a file or
    a directory in which case all .arco files within the
    directory and sub-directories will be included.

Examples:
    arco .          # Compile files in current directory
    arco dir        # Compile all the files under dir
    arco main.arco  # Compile the file main.arco
)";

const char* EXTENDED_HELP_MESSAGE =
R"(
Usage: arco <options> <sources>
Possible options:

    -out-name=<name>
        Sets the name of the executable.

    -run
        Executes the program after it is compiled and linked.

    -run-seperate
        Executes the program after it is compiled and linked
        within a seperate window.

    -display-times
        Display how long different stages took.

    -stand-alone
        Excludes compiling with the standard library.

    -display-llvm
        Displays the LLVM IR generated from source code.
        !! Warning !! This can generate a huge amount of
        output depending on what is compiled.

    -keep-obj-files
        By default the object files created by the compiler are
        deleted after linking. When this option is turned on the
        compiler does not delete the object files.

    -l<Library>
        Adds a library to the linker.

    -L<libpath>
        Adds a path to for the linker to find libraries in.

    -disable-colors
        Disables displaying information with colors.

    -max-errors=<count>
        Sets how many errors will be displayed to the user before
        giving up and exiting compilation.


)";


int    ArgC;
char** UserArgs;

void DriverUnderlineError(int ArgNum) {
	llvm::errs() <<          "       |\n";
	const char* CommandMsg = "       | Command: ./arco ";
	llvm::errs() << CommandMsg;
	ulen Offset = 0, ArgOffset = 0;
	for (ulen i = 1; i < ArgC; i++) {
		llvm::errs() << UserArgs[i] << " ";
		if (i == ArgNum) {
			ArgOffset = Offset;
		}
		Offset += strlen(UserArgs[i]) + 1;
	}
	llvm::errs() << "\n";
	ulen NumUnderline = strlen(UserArgs[ArgNum]);
	llvm::errs() << std::string(strlen(CommandMsg) + ArgOffset, ' ');
	arco::SetTerminalColor(arco::TerminalColorRed);
	llvm::errs() << "^" << std::string(NumUnderline - 1, '~');
	arco::SetTerminalColor(arco::TerminalColorDefault);
	llvm::errs() << "\n";
}

void DriverError(int ArgNum, const char* Msg) {
	if (arco::FoundCompileError) {
		llvm::errs() << "\n";
	}
	arco::Logger::GlobalError(llvm::errs(), Msg);
	DriverUnderlineError(ArgNum);
}

template<typename... TArgs>
void DriverError(int ArgNum, const char* Fmt, TArgs&&... Args) {
	if (arco::FoundCompileError) {
		llvm::errs() << "\n";
	}
	arco::Logger::GlobalError(llvm::errs(), Fmt, std::forward<TArgs>(Args)...);
	DriverUnderlineError(ArgNum);
}


llvm::StringRef GetOptValue(int ArgNum, llvm::StringRef ValPart, const char* OptName) {
	if (ValPart.empty() || ValPart[0] != '=') {
		DriverError(ArgNum, "Expected '=' for %s option", OptName);
		return "";
	}
	ValPart = ValPart.substr(1);
	if (ValPart.empty()) {
		DriverError(ArgNum, "Missing value for %s option", OptName);
		return "";
	}
	return ValPart;
}


i32 ParseIntegerValue(llvm::StringRef ValPart, bool& ParseError) {
	ulen Idx = 0;
	ulen Len = ValPart.size();
	i32 IntValue = 0, PrevValue = 0;
	
	if (Len == 0) {
		ParseError = true;
		return 0;
	}

	bool IsNeg = false;
	if (ValPart[Idx] == '+' || ValPart[Idx] == '-') {
		IsNeg = ValPart[Idx] == '-';
		++Idx;
	}

	while (Idx < Len) {
		char C = ValPart[Idx];
		if (C == arco::NUMBER_SEPERATOR) {
			++Idx;
			continue;
		}
		if (!arco::IsDigit(C)) {
			ParseError = true;
			return 0;
		}
		++Idx;
		
		PrevValue = IntValue;
		IntValue  = IntValue * 10 + ((i32)C - '0');
	
		// Check for overflow
		if (IntValue / 10 < PrevValue) {
			ParseError = true;
			return 0;
		}
	}

	ParseError = false;
	return IsNeg ? -IntValue : +IntValue;
}

int main(int argc, char* argv[]) {
	ArgC     = argc;
	UserArgs = argv;

	arco::Compiler Compiler;

	OptionManager OptManager;
	OptManager.AddOption("display-times" , &Compiler.DisplayTimes);
	OptManager.AddOption("display-llvm"  , &Compiler.DisplayLLVMIR);
	OptManager.AddOption("stand-alone"   , &Compiler.StandAlone);
	OptManager.AddOption("run"           , &Compiler.RunProgram);
	OptManager.AddOption("run-seperate"  , &Compiler.RunInSeperateWindow);
	OptManager.AddOption("keep-obj-files", &Compiler.ShouldDeleteObjectFiles);
	OptManager.AddOption("keep-obj-files", &Compiler.ShouldDeleteObjectFiles);
	OptManager.AddOption("disable-colors", &arco::DisableTerminalColors);
	OptManager.AddOption("max-errors", [](int ArgNum, llvm::StringRef ValPart) {
		ValPart = GetOptValue(ArgNum, ValPart, "max-errors");
		if (ValPart.empty()) return;
		bool ParseError;
		i32 ErrorCount = ParseIntegerValue(ValPart, ParseError);
		if (ParseError) {
			DriverError(ArgNum, "Expected an integer for the error count");
			return;
		}
		if (ErrorCount <= 0) {
			DriverError(ArgNum, "The error count must be > 0");
			return;
		}

		arco::TOTAL_ALLOWED_ERRORS = ErrorCount;
	});
	OptManager.AddOption("out-name", [&Compiler](int ArgNum, llvm::StringRef ValPart) {
		ValPart = GetOptValue(ArgNum, ValPart, "out-name");
		if (ValPart.empty()) return;

		// TODO: Will want to validate the user provided a valid name for an executable!
		Compiler.SetOutputName(ValPart.str());
	});
	OptManager.AddOption("l", [&Compiler](int ArgNum, llvm::StringRef LibName) {
		if (LibName.empty()) {
			DriverError(ArgNum, "Empty library name");
			return;
		}
		Compiler.AddLibrary(LibName.data());
	});
	OptManager.AddOption("L", [&Compiler](int ArgNum, llvm::StringRef LibPath) {
		if (LibPath.empty()) {
			DriverError(ArgNum, "Empty library path");
			return;
		}
		Compiler.AddLibraryPath(LibPath.data());
	});

	llvm::SmallVector<arco::Source> Sources;
	for (int i = 1; i < argc; i++) {
		if (argv[i][0] == '-') {
			llvm::StringRef Opt = llvm::StringRef(argv[i] + 1);

			if (Opt == "help" || Opt == "-help") {
				llvm::outs() << HELP_MESSAGE;
				return 0;
			} else if (Opt == "more-options" || Opt == "more-help") {
				llvm::outs() << EXTENDED_HELP_MESSAGE;
				return 0;
			} else if (Opt.empty()) {
				DriverError(i, "Empty option");
			} else if (!OptManager.ProcessOption(i, Opt)) {
				DriverError(i, "Unknown option");
			}
		} else {
			arco::Source Source = {
				true,
				"default.program.module",
				argv[i]
			};
			Sources.push_back(Source);
		}
	}

	if (Sources.empty()) {
		if (arco::FoundCompileError) {
			llvm::errs() << "\n";
		}
		arco::Logger::GlobalError(llvm::errs(), "No sources provided");
		return 1;
	}

	if (arco::FoundCompileError) {
		return 1;
	}

	return Compiler.Compile(Sources);
}