#ifndef ARCO_TYPE_BINDING_H
#define ARCO_TYPE_BINDING_H

#include "AST.h"
#include <llvm/ADT/SmallVector.h>

namespace arco {

    void BindTypes(FuncDecl* Func, GenericBinding* Binding);
    
    void UnbindTypes(FuncDecl* Func);
    
    // The types must be bound before calling this function. Will check if there
    // exists an instance of the function for the given bound types.
    GenericBinding* GetExistingBinding(FuncDecl* Func, const llvm::SmallVector<Type*, 8>& BindableTypes);

    GenericBinding* CreateNewBinding(FuncDecl* Func, llvm::SmallVector<Type*, 8> BindableTypes);

}

#endif // ARCO_TYPE_BINDING_H