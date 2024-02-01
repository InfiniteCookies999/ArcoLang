#include "Compiler.h"
#include "Logger.h"

#include "Options.h"

const char* HELP_MESSAGE =
R"(
Usage: arco <options> <sources>
Basic Options (for more options use -more-options):

    -out-name=<name>
        Sets the name of the executable.

    -run
        Executes the program after it is compiled and linked.

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

)";

llvm::StringRef GetOptValue(llvm::StringRef ValPart, const char* OptName) {
	if (ValPart.empty() || ValPart[0] != '=') {
		arco::Logger::GlobalError(llvm::errs(), "Expected '=' for %s option", OptName);
		return "";
	}
	ValPart = ValPart.substr(1);
	if (ValPart.empty()) {
		arco::Logger::GlobalError(llvm::errs(), "Missing value for %s option", OptName);
		return "";
	}
	return ValPart;
}

int main(int argc, char* argv[]) {

	arco::Compiler Compiler;

	OptionManager OptManager;
	OptManager.AddOption("display-times" , &Compiler.DisplayTimes);
	OptManager.AddOption("display-llvm"  , &Compiler.DisplayLLVMIR);
	OptManager.AddOption("stand-alone"   , &Compiler.StandAlone);
	OptManager.AddOption("run"           , &Compiler.RunProgram);
	OptManager.AddOption("keep-obj-files", &Compiler.ShouldDeleteObjectFiles);
	OptManager.AddOption("out-name", [&Compiler](llvm::StringRef ValPart) {
		ValPart = GetOptValue(ValPart, "out-name");
		if (ValPart.empty()) return;

		// TODO: Will want to validate the user provided a valid name for an executable!
		Compiler.SetOutputName(ValPart.str());
	});
	OptManager.AddOption("l", [&Compiler](llvm::StringRef LibName) {
		if (LibName.empty()) {
			arco::Logger::GlobalError(llvm::errs(), "Empty library name");
			return;
		}
		Compiler.AddLibrary(LibName.data());
	});
	OptManager.AddOption("L", [&Compiler](llvm::StringRef LibPath) {
		if (LibPath.empty()) {
			arco::Logger::GlobalError(llvm::errs(), "Empty library path");
			return;
		}
		Compiler.AddLibraryPath(LibPath.data());
	});

	// TODO: Underline the part of the command that has the error.
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
				arco::Logger::GlobalError(llvm::errs(), "Empty option");
			} else if (!OptManager.ProcessOption(Opt)) {
				arco::Logger::GlobalError(llvm::errs(), "Unknown option");
			}
		} else {
			arco::Source Source = {
				"default.program.module",
				argv[i]
			};
			Sources.push_back(Source);
		}
	}

	if (Sources.empty()) {
		arco::Logger::GlobalError(llvm::errs(), "No sources provided");
		return 1;
	}

	Compiler.Compile(Sources);

	return 0;
}