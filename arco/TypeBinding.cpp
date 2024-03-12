#include "TypeBinding.h"

void arco::BindTypes(FuncDecl* Func, GenericBinding* Binding) {
    
    auto& GenTys = Func->GenData->GenTys;

    // -- DEBUG
    //llvm::outs() << "BINDING TYPES:\n";
    //for (ulen i = 0; i < GenTys.size(); i++) {
    //    Type* TyToBind = Binding->BindableTypes[i];
    //    llvm::outs() << TyToBind->ToString() << "\n";
    //}
    //llvm::outs() << "BINDING TYPES END\n";

    GenericBinding* PreviousBinding = Func->GenData->CurBinding;
    if (PreviousBinding) {
        Func->GenData->BindingStack.push(PreviousBinding);
    }
    Func->GenData->CurBinding = Binding;
    for (ulen i = 0; i < GenTys.size(); i++) {
        GenericType* GenTy = GenTys[i];
        Type* TyToBind = Binding->BindableTypes[i];
        GenTy->BindType(TyToBind);
    }
}

void arco::UnbindTypes(FuncDecl* Func) {

    auto& GenTys = Func->GenData->GenTys;

    // -- DEBUG
    //llvm::outs() << "REMOVING BINDING TYPES:\n";
    //for (GenericType* GenTy : GenTys) {
    //    llvm::outs() << GenTy->GetBoundTy()->ToString() << "\n";
    //}
    //llvm::outs() << "REMOVING BINDING TYPES END\n";

    Func->GenData->CurBinding = nullptr;
    for (GenericType* GenTy : GenTys) {
        GenTy->UnbindType();
    }


    if (!Func->GenData->BindingStack.empty()) {
        // Restoring previous qualification.
        GenericBinding* PreviousBinding = Func->GenData->BindingStack.top();
        Func->GenData->BindingStack.pop();
        BindTypes(Func, PreviousBinding);
    }
}

arco::GenericBinding* arco::GetExistingBinding(FuncDecl* Func, const llvm::SmallVector<Type*, 8>& BindableTypes) {

    auto* GenData = Func->GenData;
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

arco::GenericBinding* arco::CreateNewBinding(FuncDecl* Func, llvm::SmallVector<Type*, 8> BindableTypes) {

    // TODO: use of new
    GenericBinding* Binding = new GenericBinding;
    Binding->BindableTypes = std::move(BindableTypes);

    // -- DEBUG
    //llvm::outs() << "NEW TYPE BINDING:\n";
    //for (Type* QualTy : Binding->BindableTypes) {
    //    llvm::outs() << QualTy->ToString() << "\n";
    //}
    //llvm::outs() << "NEW TYPE BINDING END\n";

    Func->GenData->Bindings.push_back(Binding);
    return Binding;
}
