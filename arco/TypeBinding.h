#ifndef ARCO_TYPE_BINDING_H
#define ARCO_TYPE_BINDING_H

#include "AST.h"

namespace arco {

    //void BindTypes(FuncDecl* Func, const llvm::SmallVector<Type*>& Tys);

    void BindTypes(FuncDecl* Func, GenericBind* Binding);

    void UnbindTypes(FuncDecl* Func);

    // The types must be bound before calling this function. Will check if there
    // exists an instance of the function for the given bound types.
    GenericBind* GetExistingBinding(FuncDecl* Func);

    GenericBind* CreateNewBinding(FuncDecl* Func);

}

#endif // ARCO_TYPE_BINDING_H