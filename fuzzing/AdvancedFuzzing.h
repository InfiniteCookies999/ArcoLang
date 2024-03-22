#ifndef ARCO_ADVANCED_FUZZING_H
#define ARCO_ADVANCED_FUZZING_H

#include <Compiler.h>
#include <Context.h>
#include <llvm/ADT/DenseSet.h>

enum AdvFuzzSemConsiders {
    NO_UNREACHABLE,
    RESTRICT_SHIFT_SIZE
};

void GenAdvancedFuzz(arco::ArcoContext& Context, llvm::DenseSet<u32> Considers);

#endif // ARCO_ADVANCED_FUZZING_H