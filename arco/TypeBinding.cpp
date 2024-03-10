#include "TypeBinding.h"

void arco::BindTypes(FuncDecl* Func, GenericBind* Binding) {
    // TODO: This will have to store previous binding stack in order to
    // take into account really weird cases of the user recursively calling
    // a generic function with different types. Or disallow this since that
    // is a very odd thing to do.
    Func->CurBinding = Binding;
    auto& GenTys = Func->GenData->GenTys;
    for (ulen i = 0; i < GenTys.size(); i++) {
        GenericType* GenTy    = GenTys[i];
        Type*        TyToBind = Binding->BindableTypes[i];
        GenTy->BindType(TyToBind);
    }
}

void arco::UnbindTypes(FuncDecl* Func) {
    auto& GenTys = Func->GenData->GenTys;
    for (GenericType* GenTy : GenTys) {
        GenTy->UnbindType();
    }
}

arco::GenericBind* arco::GetExistingBinding(FuncDecl* Func) {

    auto* GenData = Func->GenData;
    for (GenericBind* Binding : GenData->GenBindings) {
        bool TysMatch = true;
        for (ulen i = 0; i < Binding->BindableTypes.size(); i++) {
            Type* ExistingBindTy = Binding->BindableTypes[i];
            Type* BoundTy        = GenData->GenTys[i];
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

arco::GenericBind* arco::CreateNewBinding(FuncDecl* Func) {

    // TODO: use of new
    GenericBind* Binding = new GenericBind;
    auto* GenData = Func->GenData;
    for (GenericType* GenTy : GenData->GenTys) {
        Binding->BindableTypes.push_back(GenTy->GetBoundTy());
    }

    Func->GenData->GenBindings.push_back(Binding);
    return Binding;
}
