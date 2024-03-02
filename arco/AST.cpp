#include "AST.h"

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

arco::FuncsList* arco::FileScope::FindFuncsList(Identifier Name) {
    auto Itr = std::find_if(PrivateFuncs.begin(), PrivateFuncs.end(),
        [=](const FuncsList& List) {
            return List[0]->Name == Name;
        });
    if (Itr == PrivateFuncs.end()) {
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
