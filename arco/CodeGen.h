#ifndef ARCO_CODE_GEN_H
#define ARCO_CODE_GEN_H

#include "Prelude.h"

namespace llvm {
    class TargetMachine;
    class Module;
    class TargetMachine;
}

namespace arco {

    bool InitLLVMNativeTarget();

    llvm::TargetMachine* CreateLLVMTargetMache();

    void SetTargetToModule(llvm::Module& LLModule, llvm::TargetMachine* LLTargetMachine);

    bool WriteObjFile(const char* FilePath, llvm::Module& LLModule, llvm::TargetMachine* LLTargetMachine);

}

#endif // ARCO_CODE_GEN_H