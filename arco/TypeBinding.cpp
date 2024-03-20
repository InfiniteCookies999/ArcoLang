#include "TypeBinding.h"

void arco::BindTypes(Decl* D, GenericBinding* Binding) {
    
    auto& GenTys = D->GenData->GenTys;

    // -- DEBUG
    //llvm::outs() << "BINDING TYPES:\n";
    //for (ulen i = 0; i < GenTys.size(); i++) {
    //    Type* TyToBind = Binding->BindableTypes[i];
    //    llvm::outs() << TyToBind->ToString() << "\n";
    //}
    //llvm::outs() << "BINDING TYPES END\n";
    
    GenericBinding* PreviousBinding = D->GenData->CurBinding;
    if (PreviousBinding) {
        D->GenData->BindingStack.push(PreviousBinding);
    }
    D->GenData->CurBinding = Binding;
    for (ulen i = 0; i < GenTys.size(); i++) {
        GenericType* GenTy = GenTys[i];
        Type* TyToBind = Binding->BindableTypes[i];
        GenTy->BindType(TyToBind);
    }
}

void arco::UnbindTypes(Decl* D) {

    auto& GenTys = D->GenData->GenTys;

    // -- DEBUG
    //llvm::outs() << "REMOVING BINDING TYPES:\n";
    //for (GenericType* GenTy : GenTys) {
    //    llvm::outs() << GenTy->GetBoundTy()->ToString() << "\n";
    //}
    //llvm::outs() << "REMOVING BINDING TYPES END\n";

    D->GenData->CurBinding = nullptr;
    for (GenericType* GenTy : GenTys) {
        GenTy->UnbindType();
    }


    if (!D->GenData->BindingStack.empty()) {
        // Restoring previous qualification.
        GenericBinding* PreviousBinding = D->GenData->BindingStack.top();
        D->GenData->BindingStack.pop();
        BindTypes(D, PreviousBinding);
    }
}

arco::GenericBinding* arco::GetExistingBinding(Decl* D, const llvm::SmallVector<Type*, 8>& BindableTypes) {

    auto* GenData = D->GenData;
    for (GenericBinding* Binding : GenData->Bindings) {
        bool TysMatch = true;
        for (ulen i = 0; i < Binding->BindableTypes.size(); i++) {
            Type* ExistingBindTy = Binding->BindableTypes[i];
            Type* BoundTy        = BindableTypes[i];
            if (!ExistingBindTy->Equals(BoundTy)) {
                TysMatch = false;
                break;
            }
        }
        if (TysMatch) {
            // There exists a binding with thoses types.
            return Binding;
        }
    }

    return nullptr;
}

arco::GenericBinding* arco::CreateNewBinding(Decl* D, llvm::SmallVector<Type*, 8> BindableTypes) {

    // TODO: use of new
    GenericBinding* Binding = new GenericBinding;
    Binding->BindableTypes = std::move(BindableTypes);

    D->GenData->Bindings.push_back(Binding);
    return Binding;
}
