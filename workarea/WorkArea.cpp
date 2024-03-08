
#include <Process.h>
#include <Logger.h>
#include <TermColors.h>
#include <Compiler.h>

#include <llvm/Support/raw_ostream.h>

#define SRC(x) ARCO_TEST_SOURCE_DIR x

int main() {
    
    llvm::SmallVector<arco::Source> Sources;
    //Sources.push_back(arco::Source{ true, "default.program.module", SRC("test_utils.arco") });
    Sources.push_back(arco::Source{ true, "default.program.module", SRC("workpad") });
    
    arco::Compiler Compiler;
    //Compiler.StandAlone = true;
    //Compiler.EmitDebugInfo = true;
    Compiler.DisplayLLVMIR = true;
    //Compiler.SetOutputDirectory("abc");
    Compiler.DisplayTimes = true;
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