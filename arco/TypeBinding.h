#ifndef ARCO_TYPE_BINDING_H
#define ARCO_TYPE_BINDING_H

#include "AST.h"
#include <llvm/ADT/SmallVector.h>

namespace arco {

    void BindTypes(Decl* D, GenericBinding* Binding);
    
    void UnbindTypes(Decl* D);
    
    GenericBinding* GetExistingBinding(Decl* D, const llvm::SmallVector<Type*, 8>& BindableTypes);

    GenericBinding* CreateNewBinding(Decl* D, llvm::SmallVector<Type*, 8> BindableTypes);

}

#endif // ARCO_TYPE_BINDING_H