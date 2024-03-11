#include "TypeBinding.h"

void arco::BindTypes(FuncDecl* Func, GenericBind* Binding) {
    // TODO: This will have to store previous binding stack in order to
    // take into account really weird cases of the user recursively calling
    // a generic function with different types. Or disallow this since that
    // is a very odd thing to do.
    Func->CurBinding = Binding;
    ulen Count = 0;
    for (VarDecl* Param : Func->Params) {
        if (Param->Ty->ContainsGenerics) {
            Param->Ty->QualifiedType = Binding->BindableTypes[Count++];
        }
    }
}

void arco::UnbindTypes(FuncDecl* Func) {
    // TODO: Does this need to be recursive and dequalify more than just
    // the root?
    auto& GenTys = Func->GenData->GenTys;
    for (VarDecl* Param : Func->Params) {
        if (Param->Ty->ContainsGenerics) {
            Param->Ty->QualifiedType = nullptr;
        }
    }
}

arco::GenericBind* arco::GetExistingBinding(FuncDecl* Func) {

    auto* GenData = Func->GenData;
    for (GenericBind* Binding : GenData->GenBindings) {
        ulen Count = 0;
        bool TysMatch = true;
        for (VarDecl* Param : Func->Params) {
            if (Param->Ty->ContainsGenerics) {
                Type* ExistingBindTy = Binding->BindableTypes[Count++];
                Type* BoundTy        = Param->Ty->QualifiedType;
                if (!ExistingBindTy->Equals(BoundTy)) {
                    TysMatch = false;
                    break;
                }
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
    for (VarDecl* Param : Func->Params) {
        if (Param->Ty->ContainsGenerics) {
            Binding->BindableTypes.push_back(Param->Ty->QualifiedType);
        }
    }

    Func->GenData->GenBindings.push_back(Binding);
    return Binding;
}
