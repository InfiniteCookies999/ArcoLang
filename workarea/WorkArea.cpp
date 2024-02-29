
#include <Process.h>
#include <Logger.h>
#include <TermColors.h>
#include <Compiler.h>

#include <llvm/Support/raw_ostream.h>
#include <fstream>

#include <FloatConversions.h>

#define SRC(x) ARCO_TEST_SOURCE_DIR x

double ToDouble(llvm::StringRef Text) {
    bool IsSigned = Text[0] == '-';
    if (Text[0] == '-') { // Algorithm doesn't know how to handle signed characters !
        Text = Text.substr(1);
    }
    
    arco::FD::FloatParseError Error;
    double result = arco::FD::ToIEEEDouble(Text, Error);
    if (Error != arco::FD::FloatParseError::NONE) {
        llvm::outs() << "Ok we really fucked up!";
        exit(1);
    }
    
    if (IsSigned) {
        result = -result;
    }

    return result;
}

float ToSingle(llvm::StringRef Text) {
    bool IsSigned = Text[0] == '-';
    if (Text[0] == '-') { // Algorithm doesn't know how to handle signed characters !
        Text = Text.substr(1);
    }

    arco::FD::FloatParseError Error;
    float result = arco::FD::ToIEEESingle(Text, Error);
    if (Error != arco::FD::FloatParseError::NONE) {
        llvm::outs() << "Ok we really fucked up!";
        exit(1);
    }

    if (IsSigned) {
        result = -result;
    }

    return result;
}

int main() {
    
    llvm::SmallVector<arco::Source> Sources;
    Sources.push_back(arco::Source{ true, "default.program.module", SRC("test_utils.arco") });
    Sources.push_back(arco::Source{ true, "default.program.module", SRC("workpad") });
    
    arco::Compiler Compiler;
    Compiler.StandAlone = true;
    Compiler.EmitDebugInfo = true;
    Compiler.DisplayLLVMIR = true;
    //Compiler.SetOutputDirectory("abc");
    //Compiler.DisplayTimes = true;
    Compiler.Compile(Sources);
    
    if (!arco::FoundCompileError) {
        std::string StdOutResult;
        int Ok = arco::ExeHiddenProcess("program", StdOutResult);
        
        if (!Ok) {
            llvm::outs() << "Failed to run the compiled program\n";
            return 1;
        } else {
            llvm::outs() << '\n';
            llvm::outs() << "Program Standard Output: \"";
            llvm::outs() << StdOutResult << "\"\n";
        }
    }
    
    return 0;
}