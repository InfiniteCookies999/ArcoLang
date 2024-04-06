#include "AST.h"

#include "Context.h"
#include "Generics.h"

// TODO: These predicate find_if functions may be slower than just iterating
// normally.

arco::Expr* arco::FuncDecl::GetInitializerValue(VarDecl* Field) {
    auto Itr = std::find_if(InitializerValues.begin(),
                            InitializerValues.end(),
    [Field](const FuncDecl::InitializerValue& InitValue) {
        return InitValue.FieldName == Field->Name;
    });
    if (Itr == InitializerValues.end()) {
        return nullptr;
    }
    return Itr->Assignment;
}

llvm::StructType* arco::FuncDecl::GetLLStructType() {
    // NOTE: Call Our own binding to get the generic
    // struct type because the struct is not always
    // bound when the generic function is bound.
    if (Struct->IsGeneric()) {
        assert(GenData->CurBinding && "Not bound!");
        return GenData->CurBinding->StructInfo->LLStructType;
    } else {
        return Struct->LLStructTy;
    }
}

arco::StructType* arco::FuncDecl::GetParentStructType() {
    if (Struct->IsGeneric()) {
        return GenData->CurBinding->StructInfo->QualStructTy;
    } else {
        return Struct->StructTy;
    }
}

bool arco::StructDecl::ImplementsInterface(InterfaceDecl* Interface) {
    auto Itr = std::find(Interfaces.begin(), Interfaces.end(), Interface);
    return Itr != Interfaces.end();
}

arco::VarDecl* arco::StructDecl::FindField(Identifier Name) {
    auto Itr = std::find_if(Fields.begin(), Fields.end(), [=](VarDecl* Field) {
        return Field->Name == Name;
    });
    if (Itr == Fields.end()) {
        return nullptr;
    }
    return *Itr;
        
}

const arco::EnumDecl::EnumValue* arco::EnumDecl::FindValue(Identifier Name) const {
    auto Itr = std::find_if(Values.begin(), Values.end(), [Name](const EnumValue& Value) {
        return Value.Name == Name;
    });
    if (Itr == Values.end()) {
        return nullptr;
    }
    return Itr;
}

arco::Decl* arco::FileScope::FindDecl(Identifier Name) {
    auto Itr = std::find_if(PrivateDecls.begin(), PrivateDecls.end(),
        [=](Decl* Dec) {
            return Dec->Name == Name;
        });
    if (Itr == PrivateDecls.end()) {
        return nullptr;
    }
    return *Itr;
}

arco::GenericBinding* arco::Decl::GetCurBinding() {
    return GenData->CurBinding;
}

#define SET_STATE_INFO(Name, Val)                        \
        if (IsGeneric())                                 \
            GenData->CurBinding->StructInfo->Name = Val; \
        else                                             \
            Name = Val;

void arco::StructDecl::SetFieldsHaveAssignment(bool Tof) {
    SET_STATE_INFO(FieldsHaveAssignment, Tof);
}

void arco::StructDecl::SetNeedsDestruction(bool Tof) {
    SET_STATE_INFO(NeedsDestruction, Tof);
}

void arco::StructDecl::SetMustForceRaise(bool Tof) {
    SET_STATE_INFO(MustForceRaise, Tof);
}

#undef SET_STATE_INFO
