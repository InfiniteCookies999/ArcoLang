#include "Generics.h"

#include "AST.h"

void arco::BindTypes(Decl* D, GenericBinding* Binding) {

    auto& GenTys = D->GenData->GenTys;

    // -- DEBUG
    //llvm::outs() << "BINDING TYPES: (for ";
    //if (D->Is(AstKind::FUNC_DECL)) {
    //    llvm::outs() << "function " << D->Name;
    //} else if (D->Is(AstKind::STRUCT_DECL)) {
    //    llvm::outs() << "struct " << D->Name;
    //}
    //llvm::outs() << ")\n";
    //for (ulen i = 0; i < GenTys.size(); i++) {
    //    Type* TyToBind = Binding->BindableTypes[i];
    //    GenericType* GenTy = GenTys[i];
    //    llvm::outs() << (void*)GenTy << " -> " << TyToBind->ToString() << "\n";
    //}
    //llvm::outs() << "BINDING TYPES END\n";

    GenericBinding* PreviousBinding = D->GenData->CurBinding;
    if (PreviousBinding) {
        D->GenData->BindingStack.push(PreviousBinding);
    }
    D->GenData->CurBinding = Binding;
    if (Binding->ParentBinding) {
        BindTypes(Binding->ParentBinding->Dec, Binding->ParentBinding);
    }
    for (ulen i = 0; i < GenTys.size(); i++) {
        GenericType* GenTy = GenTys[i];
        Type* TyToBind = Binding->BindableTypes[i]->UnboxGeneric();
        assert(TyToBind->GetRealKind() != TypeKind::Generic);
        GenTy->BindType(TyToBind);
    }
}

void arco::UnbindTypes(Decl* D) {

    auto& GenTys = D->GenData->GenTys;

    // -- DEBUG
    //llvm::outs() << "REMOVING BINDING TYPES: (for ";
    //if (D->Is(AstKind::FUNC_DECL)) {
    //    llvm::outs() << "function " << D->Name;
    //} else if (D->Is(AstKind::STRUCT_DECL)) {
    //    llvm::outs() << "struct " << D->Name;
    //}
    //llvm::outs() << ")\n";
    //for (GenericType* GenTy : GenTys) {
    //    llvm::outs() << (void*)GenTy << " -> " << GenTy->GetBoundTy()->ToString() << "\n";
    //}
    //llvm::outs() << "REMOVING BINDING TYPES END\n";

    if (D->GenData->CurBinding->ParentBinding) {
        UnbindTypes(D->GenData->CurBinding->ParentBinding->Dec);
    }

    D->GenData->CurBinding = nullptr;
    for (GenericType* GenTy : GenTys) {
        GenTy->UnbindType();
    }

    if (!D->GenData->BindingStack.empty()) {
        // Restoring previous binding.
        GenericBinding* PreviousBinding = D->GenData->BindingStack.top();
        D->GenData->BindingStack.pop();
        BindTypes(D, PreviousBinding);
    }
}

arco::GenericBinding* arco::GetExistingBinding(Decl* D, const llvm::SmallVector<Type*, 8>& BindableTypes, GenericBinding* ParentBinding) {

    auto* GenData = D->GenData;
    for (GenericBinding* Binding : GenData->Bindings) {
        if (Binding->ParentBinding != ParentBinding) {
            continue;
        }
        bool TysMatch = true;
        for (ulen i = 0; i < Binding->BindableTypes.size(); i++) {
            Type* ExistingBindTy = Binding->BindableTypes[i]->UnboxGeneric();
            Type* BoundTy = BindableTypes[i]->UnboxGeneric();
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

arco::GenericBinding* arco::CreateNewBinding(Decl* D, llvm::SmallVector<Type*, 8> BindableTypes, GenericBinding* ParentBinding) {

    // TODO: use of new
    GenericBinding* Binding = new GenericBinding;
    Binding->BindableTypes = std::move(BindableTypes);
    Binding->ParentBinding = ParentBinding;
    Binding->Dec = D;

    D->GenData->Bindings.push_back(Binding);
    return Binding;
}
