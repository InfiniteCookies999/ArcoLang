#include "Compiler.h"
#include "Logger.h"
#include "TermColors.h"
#include "SpellChecking.h"

#include "Options.h"
#include "Process.h"
#include "picosha3.h"

#include "Files.h"
#include <filesystem>

const char* HELP_MESSAGE =
R"(
Usage: arco <options> <sources>
Basic Options (for more options use -more-options):

    -out=<name>
        Sets the name of the executable. -out-dir for directories.

    -emit-debug
        Emits debug information.

    -proj <project dir>
        Creates a new project using arco's build system.

    -build
        Builds an existing arco project.

    -more-options
        Show more options that can be ran with arco.

Sources:
    Sources may either be a path to a file or a directory
    in which case all .arco files within the directory
    and sub-directories will be included.

Examples:
    arco .          # Compile files in current directory
    arco dir        # Compile all the files under dir
    arco main.arco  # Compile the file main.arco
)";

const char* EXTENDED_HELP_MESSAGE =
R"(
Usage: arco <options> <sources>
Possible options:

    -out=<name>
        Sets the name of the executable.

    -emit-debug
        Emits debug information.

    -out-dir=<name>
        The directory to put the output files such as
        the executable. Will create the needed directories
        if they do not exists.

    -proj <project dir>
        Creates a new project using arco's build system.

    -build
        Builds an existing arco project.

    -mod=(<name>, <source>)
        Adds a source (file or directory + sub-directories) under the
        given module name.

    -run
        Executes the program after it is compiled and linked.

    -run-seperate
        Executes the program after it is compiled and linked
        within a seperate terminal.

    -show-link-command
        Displays the command executed for linking the program.

    -no-wroteto-display
        Turns off showing the message for where the program wrote
        files to.

    -show-times
        Display how long different stages took.

    -stand-alone
        Excludes compiling with the standard library.

    -show-llvm
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

    -short-errors
        If set to true it will not display a visual indication of
        where the error occured but instead only show the message.

    -full-paths
        Error messages will display full paths of the file locations
        where errors occured. Normally it will only show the path
        information relative to the modules' folders.

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

void CreateNewProject(const char* ProjDir) {
    if (std::filesystem::exists(ProjDir)) {
        arco::Logger::GlobalError(llvm::errs(),
            "Could not create project because the provided directory '%s' already exists",
            ProjDir);
        return;
    }

    std::string ProjDirStr = std::string(ProjDir);
    if (ProjDirStr.ends_with("/")) {
        ProjDirStr = ProjDirStr.substr(0, ProjDirStr.size() - 1);
    }

    if (!arco::CreateDirectories(ProjDirStr, "project")) {
        return;
    }
    if (!arco::CreateDirectories(ProjDirStr + "/.arco_build", "hidden project")) {
        return;
    }
    if (!arco::CreateDirectories(ProjDirStr + "/src", "project src")) {
        return;
    }

    const char* MainFileContents =
R"(import std;

fn main() {
    std.println("Hello from arco!");
})";

    std::string MainFilePath = ProjDirStr + "/src/main.arco";
    std::ofstream ProjMainFileStream(MainFilePath);
    if (ProjMainFileStream.fail()) {
        arco::Logger::GlobalError(llvm::errs(),
            "Failed to create project because creating file 'main.arco' failed");
        return;
    }
    ProjMainFileStream << "import std;\n";
    ProjMainFileStream << "\n";
    ProjMainFileStream << "fn main() {\n";
    ProjMainFileStream << "    std.println(\"Hello from arco!\");\n";
    ProjMainFileStream << "}";

    std::string BuildFilePath = ProjDirStr + "/build.arco";
    std::ofstream ProjBuildFileStream(BuildFilePath);
    if (ProjBuildFileStream.fail()) {
        arco::Logger::GlobalError(llvm::errs(),
            "Failed to create project because creating file '%s' failed", BuildFilePath);
        return;
    }

    const char* BuildFileContents =
R"(import std;
import std.build;

OUTPUT_NAME :: "program";

fn main(argc int, argv char**) {

    build.output_name = OUTPUT_NAME;

    build.add_source("src");

    build.execute(argc, argv);

})";

    ProjBuildFileStream << BuildFileContents;

    std::string HashLockPath = ProjDirStr + "/.arco_build/build_hash.txt";
    std::ofstream HashLockFileStream(HashLockPath);
    if (HashLockFileStream.fail()) {
        arco::Logger::GlobalError(llvm::errs(),
            "Failed to create project because creating file '%s' failed", HashLockPath);
        return;
    }

    llvm::outs() << "Created a new project at: " << ProjDir << "\n";

}

std::string CreateBuildHash(llvm::StringRef Contents) {
    std::vector<unsigned char> HashBytes(picosha2::k_digest_size);
    picosha2::hash256(Contents.begin() , Contents.end(),
                      HashBytes.begin(), HashBytes.end());
    return picosha2::bytes_to_hex_string(HashBytes.begin(), HashBytes.end());
}

void RunBuildCommand(int argc, char* argv[], int i) {

    if (!std::filesystem::exists("build.arco")) {
        arco::Logger::GlobalError(llvm::errs(),
            "This directory is not a valid build directory since it is missing 'build.arco' file");
        return;
    }

    if (!std::filesystem::exists(".arco_build")) {
        // TODO: Create the directory. The user might have deleted it in an attempt
        // to clear the cache or something.
        if (!arco::CreateDirectories(".arco_build", "hidden project")) {
            return;
        }
    }

    std::string OriginalHash;
#ifdef _WIN32
    std::string BuildExePath = ".arco_build/build.exe";
#else
    std::string BuildExePath = ".arco_build/build";
#endif

    if (std::filesystem::exists(".arco_build/build_hash.txt") &&
        std::filesystem::exists(BuildExePath)) {

        std::ifstream HashFileStream(".arco_build/build_hash.txt");
        if (HashFileStream.fail()) {
            arco::Logger::GlobalError(llvm::errs(),
                "Failed to build because the '.arco_build/build_hash.txt' file failed to open");
            return;
        }
        HashFileStream >> OriginalHash;
    } // else just let it be empty and it will recreate the hash.

    char* BuildFileBuffer;
    ulen  BuildFileSize;
    if (!arco::ReadFile("build.arco", BuildFileBuffer, BuildFileSize)) {
        arco::Logger::GlobalError(llvm::errs(),
            "Failed to build because the 'build.arco' file failed to open");
        return;
    }

    llvm::StringRef BuildFileContents = llvm::StringRef(BuildFileBuffer, BuildFileSize);
    std::string Hash = CreateBuildHash(BuildFileContents);

    if (Hash != OriginalHash) {
        // Different hashes so have to recompile!
        llvm::outs() << ">> Building build.arco\n";

        std::string Result;
        arco::ExeHiddenProcess("arco build.arco -out=build -out-dir=.arco_build", Result);
        if (Result.find("error") != std::string::npos) {
            // NOTE: Hacky, just rerunning the process but as non-hidden
            // to get the result of the error :)
            arco::ExeProcess("arco build.arco -out=build -out-dir=.arco_build", nullptr, false);
            return;
        }

        std::ofstream OutHashFileStream(".arco_build/build_hash.txt");
        if (OutHashFileStream.fail()) {
            arco::Logger::GlobalError(llvm::errs(),
                "Failed to build because the '.arco_build/build_hash.txt' file failed to open to write the hash!");
            return;
        }
        OutHashFileStream << Hash;

        llvm::outs() << ">> Finished building build.arco\n\n";
    }

    std::string Cmd = ".arco_build/build ";
    ++i;
    for (; i < argc; i++) {
        Cmd += argv[i] + std::string(" ");
    }
    arco::ExeProcess(Cmd.c_str(), nullptr, false);
}

int main(int argc, char* argv[]) {
    ArgC     = argc;
    UserArgs = argv;

    arco::Compiler Compiler;

    OptionManager OptManager;
    OptManager.AddOption("show-times"        , &Compiler.DisplayTimes);
    OptManager.AddOption("show-llvm"         , &Compiler.DisplayLLVMIR);
    OptManager.AddOption("stand-alone"       , &Compiler.StandAlone);
    OptManager.AddOption("run"               , &Compiler.RunProgram);
    OptManager.AddOption("run-seperate"      , &Compiler.RunInSeperateWindow);
    OptManager.AddOption("keep-obj-files"    , &Compiler.ShouldDeleteObjectFiles);
    OptManager.AddOption("keep-obj-files"    , &Compiler.ShouldDeleteObjectFiles);
    OptManager.AddOption("short-errors"      , &arco::ShortErrors);
    OptManager.AddOption("disable-colors"    , &arco::DisableTerminalColors);
    OptManager.AddOption("emit-debug"        , &Compiler.EmitDebugInfo);
    OptManager.AddOption("show-link-command" , &Compiler.ShowLinkCommand);
    OptManager.AddOption("no-wroteto-display", &Compiler.NoWroteToDispaly);
    OptManager.AddOption("-full-paths"       , &arco::FullPaths);
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
    OptManager.AddOption("out", [&Compiler](int ArgNum, llvm::StringRef ValPart) {
        ValPart = GetOptValue(ArgNum, ValPart, "out");
        if (ValPart.empty()) return;

        if (ValPart.find_first_of('/')  != llvm::StringRef::npos ||
            ValPart.find_first_of('\\') != llvm::StringRef::npos) {
            DriverError(ArgNum, "The output name cannot contain directory paths. If you wish to set the output directory use: -out-dir=<dir>");
            return;
        }
        // TODO: validate furher that the name is valid?
        Compiler.SetOutputName(ValPart.str());
    });
    OptManager.AddOption("out-dir", [&Compiler](int ArgNum, llvm::StringRef ValPart) {
        ValPart = GetOptValue(ArgNum, ValPart, "out-dir");
        if (ValPart.empty()) return;

        // TODO: Validate it is a valid path?
        Compiler.SetOutputDirectory(ValPart.str());
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
    OptManager.AddOption("mod", [&Compiler, &Sources](int ArgNum, llvm::StringRef ValPart) {
        llvm::StringRef ModPair = GetOptValue(ArgNum, ValPart, "mod pair");
        if (ModPair.empty()) return;

        const char* Ptr = ModPair.data();
        if (*Ptr != '(') {
            DriverError(ArgNum, "Expected '(' character for module pair");
            return;
        }
        ++Ptr;

        const char* NameStartPtr = Ptr;
#define IsIdentChar (arco::IsAlpha(*Ptr))
        if (!IsIdentChar) {
            DriverError(ArgNum, "Expected alphabetic character to start mod name");
            return;
        }

        while (IsIdentChar) {
            ++Ptr;
        }

        llvm::StringRef ModName =
            llvm::StringRef(NameStartPtr, Ptr - NameStartPtr);
        if (*Ptr != ',') {
            DriverError(ArgNum, "Expected ',' character to seperate mod name from source");
            return;
        }
        ++Ptr;

        const char* PathStartPtr = Ptr;
        const char* End = ModPair.end();
        while (*Ptr != ')' && Ptr != End) {
            ++Ptr;
        }

        if (PathStartPtr == Ptr) {
            DriverError(ArgNum, "Path cannot be empty");
            return;
        }

        llvm::StringRef Path =
            llvm::StringRef(PathStartPtr, Ptr - PathStartPtr);
        if (*Ptr != ')') {
            DriverError(ArgNum, "Expected ')' character for module pair");
            return;
        }

#undef IsIdentChar
        Sources.push_back(arco::Source{
            false,
            ModName,
            Path
        });
    });

    for (int i = 1; i < argc; i++) {
        if (argv[i][0] == '-') {
            llvm::StringRef Opt = llvm::StringRef(argv[i] + 1);

            if (Opt == "help" || Opt == "-help") {
                llvm::outs() << HELP_MESSAGE;
                return 0;
            } else if (Opt == "more-options" || Opt == "more-help") {
                llvm::outs() << EXTENDED_HELP_MESSAGE;
                return 0;
            } else if (Opt == "proj" || Opt == "project") {
                if (i+1 == argc) {
                    DriverError(i, "Expected directory for new project");
                    return 0;
                }
                const char* ProjDir = argv[i+1];
                CreateNewProject(ProjDir);
                return 0;
            } else if (Opt == "build") {
                RunBuildCommand(argc, argv, i);
                return 0;
            } else if (Opt.empty()) {
                DriverError(i, "Empty option");
            } else if (!OptManager.ProcessOption(i, Opt)) {
                auto AllOpts = OptManager.GetAllOptionsAsStrings();
                std::string Found = arco::FindClosestSpellingMatch(AllOpts, argv[i]);
                // TODO: picky: there is an extra period after the question mark
                std::string DidYouMean = !Found.empty() ? ",  Did you mean: " + std::string(Found) + "?" : "";
                DriverError(i, "Unknown option: %s%s", argv[i], DidYouMean);
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
