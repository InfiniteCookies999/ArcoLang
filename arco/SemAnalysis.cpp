#include "SemAnalysis.h"

#include "Context.h"
#include "IRGen.h"
#include "SpellChecking.h"
#include "TermColors.h"
#include "Generics.h"
#include "ExpandedLoc.h"

#include <sstream>

#define YIELD_ERROR(N)     \
N->Ty = Context.ErrorType; \
return;

#define YIELD_IF_ERROR(CH)       \
if (CH->Ty == Context.ErrorType) \
return;

#define YIELD_ERROR_WHEN(N, CH)    \
if (CH->Ty == Context.ErrorType) { \
    YIELD_ERROR(N); }

static inline ulen max(ulen a, ulen b) {
    return a > b ? a : b;
}

static inline ulen min(ulen a, ulen b) {
    return a < b ? a : b;
}

arco::SemAnalyzer::SemAnalyzer(ArcoContext& Context, Decl* D)
    : Context(Context), Mod(D->Mod), Log(D->FScope, D->FScope->Buffer)
{
}

void arco::SemAnalyzer::ReportStatementsInInvalidContext(FileScope* FScope) {
    for (auto [Kind, InvalidStmt] : FScope->InvalidStmts) {
        std::string ScopeKind;
        if (Kind == FileScope::InvalidScopeKind::GLOBAL) {
            ScopeKind = "global";
        } else if (Kind == FileScope::InvalidScopeKind::STRUCT) {
            ScopeKind = "struct";
        } else {
            assert(!"Unhandled case");
        }
        Logger Log(FScope, FScope->Buffer);
        Log.BeginError(InvalidStmt->Loc, "Statement does not belong at %s scope",
            ScopeKind);
        Log.EndError();
    }
}

void arco::SemAnalyzer::ResolveImports(FileScope* FScope, ArcoContext& Context) {
    if (FScope->ImportsResolved) return;
    FScope->ImportsResolved = true;

#define BERROR(Fmt, ...)                          \
Logger Log(FScope, FScope->Buffer); \
Log.BeginError(ErrorLoc, Fmt, __VA_ARGS__);

    ErrorSpellChecker SpellChecker;

    for (auto& [LookupIdent, StructOrNamespaceImport] : FScope->Imports) {
        auto ModItr = Context.ModNamesToMods.find(StructOrNamespaceImport.ModOrNamespace.Text);
        bool ModExists = ModItr != Context.ModNamesToMods.end();

        SourceLoc ErrorLoc = StructOrNamespaceImport.ErrorLoc;
        Module* ImportMod = ModExists ? ModItr->second : FScope->Mod;

        if (StructOrNamespaceImport.StructOrNamespace.IsNull()) {
            // import mod;
            // import namespace;
            if (ModExists) {
                StructOrNamespaceImport.NSpace = ImportMod->DefaultNamespace;
            } else {
                auto Itr = ImportMod->Namespaces.find(StructOrNamespaceImport.ModOrNamespace);
                if (Itr != ImportMod->Namespaces.end()) {
                    StructOrNamespaceImport.NSpace = Itr->second;
                } else {
                    BERROR("Could not find module or namespace '%s'",
                        StructOrNamespaceImport.ModOrNamespace);
                    SpellChecker.AddSearches(ImportMod->Namespaces);
                    SpellChecker.AddSearches(Context.ModNamesToMods);
                    SpellChecker.SearchAndEndError(Log, StructOrNamespaceImport.ModOrNamespace);
                }
            }
        } else if (StructOrNamespaceImport.StructName.IsNull()) {
            // import mod.struct;
            // import mod.namespace;
            // import namespace.struct;

            if (ModExists) {
                auto Itr = ImportMod->DefaultNamespace->Decls.find(StructOrNamespaceImport.StructOrNamespace);
                if (Itr != ImportMod->DefaultNamespace->Decls.end() && Itr->second->IsStructLike()) {
                    StructOrNamespaceImport.Decl = Itr->second;
                } else {
                    auto Itr2 = ImportMod->Namespaces.find(StructOrNamespaceImport.StructOrNamespace);
                    if (Itr2 != ImportMod->Namespaces.end()) {
                        StructOrNamespaceImport.NSpace = Itr2->second;
                    } else {
                        BERROR("Could not find type or namespace '%s' in module '%s'",
                             StructOrNamespaceImport.StructOrNamespace, StructOrNamespaceImport.ModOrNamespace);
                        SpellChecker.AddSearches(ImportMod->Namespaces);
                        SpellChecker.SearchAndEndError(Log, StructOrNamespaceImport.StructOrNamespace);
                    }
                }
            } else {
                auto Itr = ImportMod->Namespaces.find(StructOrNamespaceImport.ModOrNamespace);
                if (Itr != ImportMod->Namespaces.end()) {
                    Namespace* LookupNamespace = Itr->second;
                    auto Itr2 = LookupNamespace->Decls.find(StructOrNamespaceImport.StructOrNamespace);
                    if (Itr2 != LookupNamespace->Decls.end() && Itr2->second->IsStructLike()) {
                        StructOrNamespaceImport.Decl = Itr2->second;
                    } else {
                        BERROR("Could not find type '%s' in namespace '%s'",
                            StructOrNamespaceImport.StructOrNamespace, StructOrNamespaceImport.ModOrNamespace);
                        SpellChecker.AddSearches(LookupNamespace->Decls);
                        SpellChecker.SearchAndEndError(Log, StructOrNamespaceImport.StructOrNamespace);
                    }
                } else {
                    BERROR("Could not find module or namespace '%s'", StructOrNamespaceImport.ModOrNamespace);
                    SpellChecker.AddSearches(ImportMod->Namespaces);
                    SpellChecker.AddSearches(Context.ModNamesToMods);
                    SpellChecker.SearchAndEndError(Log, StructOrNamespaceImport.ModOrNamespace);
                }
            }
        } else {
            // import mod.namespace.struct;

            if (ModExists) {
                auto Itr = ImportMod->Namespaces.find(StructOrNamespaceImport.StructOrNamespace);
                if (Itr != ImportMod->Namespaces.end()) {
                    Namespace* LookupNamespace = Itr->second;
                    auto Itr2 = LookupNamespace->Decls.find(StructOrNamespaceImport.StructName);
                    if (Itr2 != LookupNamespace->Decls.end() && Itr2->second->IsStructLike()) {
                        StructOrNamespaceImport.Decl = Itr2->second;
                    } else {
                        BERROR("Could not find type '%s' in namespace '%s'",
                            StructOrNamespaceImport.StructName, StructOrNamespaceImport.StructOrNamespace);
                        SpellChecker.AddSearches(LookupNamespace->Decls);
                        SpellChecker.SearchAndEndError(Log, StructOrNamespaceImport.StructName);
                    }
                } else {
                    BERROR("Could not find namespace '%s' in module '%s'",
                        StructOrNamespaceImport.StructOrNamespace, StructOrNamespaceImport.ModOrNamespace);
                    SpellChecker.AddSearches(ImportMod->Namespaces);
                    SpellChecker.SearchAndEndError(Log, StructOrNamespaceImport.StructOrNamespace);
                }
            } else {
                BERROR("Could not find module '%s'", StructOrNamespaceImport.ModOrNamespace);
                SpellChecker.AddSearches(Context.ModNamesToMods);
                SpellChecker.SearchAndEndError(Log, StructOrNamespaceImport.ModOrNamespace);
            }
        }
    }

    for (auto& Import : FScope->StaticImports) {

        SourceLoc ErrorLoc = Import.ErrorLoc;

        // static import
        if (Import.IsNamespaceUnderMod && !Import.Mod) {
            BERROR("Could not find module '%s'", Import.NamespaceName);
            SpellChecker.AddSearches(Context.ModNamesToMods);
            SpellChecker.SearchAndEndError(Log, Import.NamespaceName);
            continue;
        }

        if (Import.Mod && Import.NamespaceName.IsNull()) {
            // Importing module's default namespace.
            Import.NSpace = Import.Mod->DefaultNamespace;
        } else if (Import.Mod) {
            // Statically importing namespace from module.
            auto Itr = Import.Mod->Namespaces.find(Import.NamespaceName);
            if (Itr == Import.Mod->Namespaces.end()) {
                BERROR("Could not find namespace '%s' in module '%s'",
                    Import.NamespaceName, Import.Mod->Name);
                SpellChecker.AddSearches(Import.Mod->Namespaces);
                SpellChecker.SearchAndEndError(Log, Import.NamespaceName);
                continue;
            }
            Import.NSpace = Itr->second;
        } else {
            // Statically importing namespace from the source file's module.
            auto Itr = FScope->Mod->Namespaces.find(Import.NamespaceName);
            if (Itr == FScope->Mod->Namespaces.end()) {
                BERROR("Could not find namespace or module '%s'", Import.NamespaceName);
                SpellChecker.AddSearches(Context.ModNamesToMods);
                SpellChecker.AddSearches(FScope->Mod->Namespaces);
                SpellChecker.SearchAndEndError(Log, Import.NamespaceName);
                continue;
            }
            Import.NSpace = Itr->second;
        }
    }

    // Auto importing important structures.
    if (Context.StdStringStruct) {
        auto Itr = FScope->Imports.find(Context.StringIdentifier);
        if (Itr == FScope->Imports.end()) {
            FileScope::StructOrNamespaceImport Import;
            Import.Decl = Context.StdStringStruct;
            FScope->Imports.insert({ Context.StringIdentifier, Import });
        }
        Itr = FScope->Imports.find(Context.ErrorInterfaceIdentifier);
        if (Itr == FScope->Imports.end()) {
            FileScope::StructOrNamespaceImport Import;
            Import.Decl = Context.StdErrorInterface;
            FScope->Imports.insert({ Context.ErrorInterfaceIdentifier, Import });
        }
    }
#undef ERROR
}

void arco::SemAnalyzer::CheckForDuplicateFuncDeclarations(Module* Mod) {
    CheckForDuplicateFuncDeclarations(Mod->DefaultNamespace);
    for (auto [NamespaceName, NSpace] : Mod->Namespaces) {
        CheckForDuplicateFuncDeclarations(NSpace);
    }
}

void arco::SemAnalyzer::CheckForDuplicateFuncDeclarations(Namespace* NSpace) {
    for (const auto& [Name, FuncList] : NSpace->Funcs) {
        CheckForDuplicateFuncs(FuncList);
    }
    for (const auto& [Name, Dec] : NSpace->Decls) {
        if (Dec->Is(AstKind::STRUCT_DECL)) {
            StructDecl* Struct = static_cast<StructDecl*>(Dec);
            CheckForDuplicateFuncs(Struct->Constructors);
            for (const auto& [Name, FuncList] : Struct->Funcs) {
                CheckForDuplicateFuncs(FuncList);
            }
        } else if (Dec->Is(AstKind::INTERFACE_DECL)) {
            InterfaceDecl* Interface = static_cast<InterfaceDecl*>(Dec);
            CheckForDuplicateFuncs(Interface->Funcs);
        }
    }
}

void arco::SemAnalyzer::CheckFuncDecl(FuncDecl* Func) {
    if (Func->HasBeenChecked && !Func->IsGeneric()) return;
    Func->HasBeenChecked = true;
    Context.UncheckedDecls.erase(Func);
    if (Func->ParsingError) return;

    //if (Func->IsGeneric()) {
    //    llvm::outs() << "CURRENTLY BOUND TYPES:\n";
    //    for (ulen i = 0; i < Func->GenData->GenTys.size(); i++) {
    //        GenericType* GenTy = Func->GenData->GenTys[i];
    //        llvm::outs() << GenTy->ToString() << "\n";
    //    }
    //}

    // -- DEBUG
    // llvm::outs() << "Checking function: "
    //              << (Func->Struct ? Func->Struct->Name.Text.str() + "." : "")
    //              << Func->Name
    //              << " : " << Func->FScope->Path
    //              << "\n";

    if (Func->IsGeneric()) {
        CreateQualifications(Func);
    }

    CheckFuncParams(Func);
    // Need to check for parsing errors again in case CheckFuncParams
    // set the function as having parsing errors.
    if (Func->ParsingError) return;

    CFunc   = Func;
    CStruct = Func->Struct;
    FScope  = Func->FScope;

    if (Func->IsConstructor) {
        if (Func->RetTy->GetKind() != TypeKind::Void) {
            Error(Func, "Constructors cannot return values");
        }
    } else {
        if (!Func->InitializerValues.empty()) {
            Error(Func, "Only constructors can have initializer values");
        }
        if (Func->IsCopyConstructor) {
            Error(Func, "Only constructors can use keyword copyobj to indicate it is a copy constructor");
        }
        if (Func->IsMoveConstructor) {
            Error(Func, "Only constructors can use keyword moveobj to indicate it is a move constructor");
        }
        if (Func->Struct && Func->Name == Func->Struct->Name) {
            Error(Func, "Cannot name member functions the same name as a constructor");
        }
    }

    if (Func->RetTy->GetKind() == TypeKind::Array) {
        Error(Func, "Functions cannot return arrays");
    }
    if (Func->IsDestructor && !Func->RaisedErrors.empty()) {
        Error(Func, "Destructors cannot raise errors");
    }

    if (Func->IsConstructor) {
        StructDecl* Struct = Func->Struct;
        // TODO: I do not think this is needed but may need to come back to.
        // doesn't take into account generics.
        //if (!Struct->HasBeenChecked) {
        //    SemAnalyzer Analyzer(Context, Struct);
        //    Analyzer.CheckStructDecl(Struct);
        //}

        if (!Struct->ParsingError) {
            for (const FuncDecl::InitializerValue& InitValue : Func->InitializerValues) {
                CheckNode(InitValue.Assignment);
                if (!Struct->FindField(InitValue.FieldName)) {
                    Error(InitValue.ErrorLoc, "No field '%s' for initializer value", InitValue.FieldName);
                }
            }

            for (VarDecl* Field : Struct->Fields) {
                if (Field->IsBeingChecked) {
                    // TODO: may be good to provide a better error message but honestly this
                    // should very rarely occure.
                    Error(Field, "Field of constructor has circular dependency");
                    continue;
                }
                if (Field->Ty == Context.ErrorType) {
                    continue;
                }


                Expr* InitValue = Func->GetInitializerValue(Field);
                if (InitValue && InitValue->Ty != Context.ErrorType) {
                    if (!IsAssignableTo(Field->Ty, InitValue)) {
                        DisplayErrorForTypeMismatch(
                            "Cannot assign value of type '%s' to field of type '%s'",
                            InitValue->Loc,
                            InitValue,
                            Field->Ty);
                    }

                    if (ViolatesConstAssignment(Field, InitValue)) {
                        Error(InitValue, "Cannot assign const memory to non-const field");
                    }
                }

                if (!Field->Assignment && Field->Ty->GetKind() == TypeKind::Struct) {
                    StructDecl* StructForTy = Field->Ty->AsStructType()->GetStruct();

                    if (!StructForTy->Constructors.empty() && !StructForTy->DefaultConstructor) {
                        if (!InitValue) {
                            Error(Func, "No default constructor to initialize the '%s' field", Field->Name);
                        }
                    }
                }
            }
        }
    }

    if (Func->Mods & ModKinds::NATIVE) {

        if (Func->Struct) {
            Error(Func, "native functions cannot be member functions");
        }

        Identifier Name = Func->Name;
        if (!Func->LinkageName.empty()) {
            Name = Identifier(Func->LinkageName);
        }

        auto Itr = Context.LLVMIntrinsicsTable.find(Name);
        if (Itr != Context.LLVMIntrinsicsTable.end()) {
            // Validating that the parameters are correct.

            bool FoundIntrinsic = false;
            for (const ArcoContext::LLIntrinsicDef& IntrinsicDef : Context.LLVMValidIntrinsicArgs) {
                if (IntrinsicDef.Name != Name) continue;
                if (IntrinsicDef.ParamTypes.size() != Func->Params.size()) {
                    continue;
                }
                if (!IntrinsicDef.RetType->Equals(Func->RetTy)) {
                    continue;
                }
                bool ParamsCorrect = true;
                for (ulen i = 0; i < IntrinsicDef.ParamTypes.size(); i++) {
                    if (!IntrinsicDef.ParamTypes[i]->Equals(Func->Params[i]->Ty)) {
                        ParamsCorrect = false;
                        break;
                    }
                }
                if (!ParamsCorrect) {
                    continue;
                }
                FoundIntrinsic = true;
                break;
            }

            if (!FoundIntrinsic) {
                Error(Func, "Invalid parameter types/return type for intrinsic function declaration");
            }
        }

        return;
    }

    Scope FuncScope;
    CheckScopeStmts(Func->Scope, FuncScope);
    if (!FuncScope.AllPathsReturn && !Func->RetTy->Equals(Context.VoidType)) {
        Error(Func, "Not all function paths return");
    }
}

void arco::SemAnalyzer::CheckStructDecl(StructDecl* Struct) {
    if (Struct->HasBeenChecked && !Struct->IsGeneric()) return;
    Struct->HasBeenChecked = true;
    Context.UncheckedDecls.erase(Struct);
    if (Struct->ParsingError) return;

    if (Struct->IsGeneric()) {
        CreateQualifications(Struct);
        auto& StructInfo = Struct->GetCurBinding()->StructInfo;

        // Qualifying all the field types because bindings are temporary but
        // references to the field types may exist longer than the bindings.
        for (VarDecl* Field : Struct->Fields) {
            if (Field->Ty->ContainsGenerics) {
                StructInfo->QualifiedFieldTypes.push_back(Field->Ty->GetQualType());
                bool HasConstAddress = Field->Ty->GetKind() == TypeKind::CStr || Field->ExplicitlyMarkedConst;
                StructInfo->FieldsConstAddressInfo.push_back(HasConstAddress);
            }
        }
    }

    Struct->IsBeingChecked = true;
    FScope  = Struct->FScope;
    CStruct = Struct;

    bool NeedsDestruction     = false;
    bool NeedsMove            = false;
    bool NeedsCopy            = false;
    bool FieldsHaveAssignment = false;

    if (Struct->Destructor) {
        NeedsDestruction = true;
    }
    if (Struct->MoveConstructor) {
        NeedsMove = true;
    }
    if (Struct->CopyConstructor) {
        NeedsCopy = true;
    }

    // Skip over pointers to vtable.
    ulen LLFieldIdx = Struct->InterfaceHooks.size();  //   Struct->Interfaces.size();  -- this struct is not filled out yet!
    for (VarDecl* Field : Struct->Fields) {
        if (Field->TyIsInfered) {
            NotFullyQualifiedVarState = true;
            CheckVarDecl(Field, false, Struct->IsGeneric());
        } else {
            NotFullyQualifiedVarState = false;
            CheckVarDecl(Field, false, Struct->IsGeneric());
        }

        if (!Field->IsComptime()) {
            if (Field->Assignment) {
                FieldsHaveAssignment = true;
            }

            Field->LLFieldIdx = LLFieldIdx;
            ++LLFieldIdx;
        }

        if (Field->Ty->GetKind() == TypeKind::Struct) {
            StructType* StructTy = Field->Ty->AsStructType();
            StructDecl* OStruct = StructTy->GetStruct();
            if (OStruct) {
                FieldsHaveAssignment |= StructTy->DoesFieldsHaveAssignment();
                NeedsDestruction     |= StructTy->DoesNeedsDestruction();
                NeedsMove            |= StructTy->DoesNeedsMove();
                NeedsCopy            |= StructTy->DoesNeedsCopy();
            }
        } else if (Field->Ty->GetKind() == TypeKind::Array) {
            ArrayType* ArrayTy = Field->Ty->AsArrayTy();

            Type* BaseTy = ArrayTy->GetBaseType();
            if (BaseTy->GetKind() == TypeKind::Struct) {
                StructType* StructTy = BaseTy->AsStructType();
                StructDecl* OStruct  = StructTy->GetStruct();
                if (OStruct) {
                    FieldsHaveAssignment |= StructTy->DoesFieldsHaveAssignment();
                    NeedsDestruction     |= StructTy->DoesNeedsDestruction();
                    NeedsMove            |= StructTy->DoesNeedsMove();
                    NeedsCopy            |= StructTy->DoesNeedsCopy();
                }
            }
        }
    }

    Struct->SetFieldsHaveAssignment(FieldsHaveAssignment);
    Struct->SetNeedsDestruction(NeedsDestruction);
    Struct->SetNeedsCopy(NeedsCopy);
    Struct->SetNeedsMove(NeedsMove);

    // Keep this below checking fields because the the function may end up
    // being part of the function signature.
    FixupInterfaces(Struct);

    // Want to set this to false before checking member functions or it will
    // complain about the type not being complete but all the relavent
    // type information has been determined already.
    Struct->IsBeingChecked = false;

    if (Struct->IsGeneric()) {
        if (!Struct->GetCurBinding()->StructInfo->QualStructTy) {
            Struct->GetCurBinding()->StructInfo->QualStructTy =
                StructType::Create(Struct, Struct->GetCurBinding()->BindableTypes, Context);
            FinishGenericStructType(Struct->GetCurBinding()->StructInfo->QualStructTy);
        }
    } else {
        if (!Struct->StructTy) {
            Struct->StructTy = StructType::Create(Struct, {}, Context);
            FinishNonGenericStructType(Context, Struct->StructTy);
        }
    }

    StructType* StructTy = Struct->IsGeneric() ? Struct->GetCurBinding()->StructInfo->QualStructTy
                                               : Struct->StructTy;

    if (Struct->DefaultConstructor) {
        if (Struct->IsGeneric()) {
            // Actually have to see if there already exists because a
            // function call may have created one.
            GenericBinding* ExistingBinding =
                GetExistingBinding(Struct->DefaultConstructor, {}, Struct->GenData->CurBinding);
            if (ExistingBinding) {
                Struct->GetCurBinding()->StructInfo->LLDefaultConstructor = ExistingBinding->FuncInfo->LLFunction;
            } else {
                GenericBinding* Binding =
                    CreateNewBinding(Struct->DefaultConstructor, {}, Struct->GenData->CurBinding);
                Binding->FuncInfo = new GenericFuncInfo; // TODO: Use of new
                Binding->StructInfo = Struct->GetCurBinding()->StructInfo;
                if (!FoundCompileError && !Context.CheckingUnhcecked) {
                    IRGenerator IRGen(Context);
                    Struct->DefaultConstructor->GenData->CurBinding = Binding;
                    IRGen.GenFuncDecl(Struct->DefaultConstructor);
                    Struct->GetCurBinding()->StructInfo->LLDefaultConstructor = Binding->FuncInfo->LLFunction;
                }
                Context.RequestGen(Struct->DefaultConstructor, Binding);
            }
        } else {
            RequestGenNonGenericFunc(Context, Struct->DefaultConstructor);
            Struct->LLDefaultConstructor = Struct->DefaultConstructor->LLFunction;
        }
    }
    if (Struct->Destructor) {
        if (Struct->IsGeneric()) {
            GenericBinding* Binding =
                CreateNewBinding(Struct->Destructor, {}, Struct->GenData->CurBinding);
            Binding->FuncInfo = new GenericFuncInfo; // TODO: Use of new
            Binding->StructInfo = Struct->GenData->CurBinding->StructInfo;
            if (!FoundCompileError && !Context.CheckingUnhcecked) {
                IRGenerator IRGen(Context);
                Struct->Destructor->GenData->CurBinding = Binding;
                IRGen.GenFuncDecl(Struct->Destructor);
                Struct->GetCurBinding()->StructInfo->LLDestructor = Binding->FuncInfo->LLFunction;
            }
            Context.RequestGen(Struct->Destructor, Binding);
        } else {
            RequestGenNonGenericFunc(Context, Struct->Destructor);
            Struct->LLDestructor = Struct->Destructor->LLFunction;
        }
    }

    auto CheckMoveOrCopyConstructor = [=](FuncDecl* Func, const char* ConstructorType) {
        if (Func->ExplicitlyHasGenerics) {
            // Let the CheckFuncDecl deal with the error.
            return;
        }
        if (Func->IsVariadic) {
            Error(Func->Loc, "%s constructors cannot be variadic", ConstructorType);
        }
        if (Func->Params.empty()) {
            Error(Func->Loc, "%s constructor must have argument for struct to copy", ConstructorType);
        } else if (Func->Params.size() > 1) {
            Error(Func->Loc, "%s constructor must have only one argument for the struct to copy", ConstructorType);
        } else {

            Type* ParamTy = Func->Params[0]->Ty;
            if (ParamTy != Context.ErrorType) {
                Type* ExpectedType = PointerType::Create(StructTy, Context);
                if (!ParamTy->Equals(ExpectedType)) {
                    Error(Func->Loc, "%s constructor must have type '%s' but found type '%s'",
                        ConstructorType, ExpectedType, ParamTy);
                }
            }
        }
    };

    auto CheckNGenMoveOrCopyConstructor = [=](FuncDecl* Func, bool IsCopy) {
        SemAnalyzer A(Context, Func);
        if (Struct->IsGeneric()) {
            GenericBinding* Binding = CreateNewBinding(Func, {}, Struct->GenData->CurBinding);
            Binding->FuncInfo = new GenericFuncInfo; // TODO: Use of new
            Binding->StructInfo = Struct->GenData->CurBinding->StructInfo;


            // Must first check that it doesn't have generics itself because
            // if it does then an attempt to bind types will try and access
            // a bindable list that doesn't exist.
            if (!Func->ExplicitlyHasGenerics) {
                IRGenerator IRGen(Context);
                Func->GenData->CurBinding = Binding;

                BindTypes(Func, Binding);
                A.CheckFuncParams(Func, true);

                CreateQualifications(Func);
                CheckMoveOrCopyConstructor(Func, IsCopy ? "Copy" : "Move");

                if (!FoundCompileError && !Context.CheckingUnhcecked) {
                    IRGen.GenFuncDecl(Func);
                    if (IsCopy) {
                        Struct->GetCurBinding()->StructInfo->LLCopyConstructor = Binding->FuncInfo->LLFunction;
                    } else {
                        Struct->GetCurBinding()->StructInfo->LLMoveConstructor = Binding->FuncInfo->LLFunction;
                    }
                    UnbindTypes(Func);
                }
            }


            Context.RequestGen(Func, Binding);
        } else {
            A.CheckFuncParams(Func);
            CheckMoveOrCopyConstructor(Func, IsCopy ? "Copy" : "Move");
            RequestGenNonGenericFunc(Context, Func);
            if (IsCopy) {
                Struct->LLCopyConstructor = Func->LLFunction;
            } else {
                Struct->LLMoveConstructor = Func->LLFunction;
            }
        }
    };

    if (Struct->CopyConstructor) {
        CheckNGenMoveOrCopyConstructor(Struct->CopyConstructor, true);
    }
    if (Struct->MoveConstructor) {
        CheckNGenMoveOrCopyConstructor(Struct->MoveConstructor, false);
    }
}

void arco::SemAnalyzer::CheckFuncParams(FuncDecl* Func, bool NotFullyQualified) {

    // This must go before checking everything else because the
    // struct may require that a function has complete type information
    // and by placing it here the struct may then recall this function,
    // this function can completely finished, and then the struct will have
    // complete information about the function.
    if (Func->Struct && !Func->Struct->IsGeneric()) {
        CheckStructDecl(Func->Struct);
    }

    if (Func->ParamTypesChecked && !Func->IsGeneric()) return;
    Func->ParamTypesChecked = true;
    if (Func->ParsingError) return;

    assert(!CFunc && "Must create a new semantic analyzer when checking function parameters");

    // -- DEBUG
    // llvm::outs() << "Checking function params of: " << (Func->Struct ? (Func->Struct->Name.Text.str()+".") : "") << Func->Name << "\n";

    CFunc   = Func;
    CStruct = Func->Struct;
    FScope  = Func->FScope;

    if (!FixupType(Func->RetTy, false, true)) {
        Func->RetTy = Context.ErrorType;
        Func->ParsingError = true;
    }

    if (Func->IsGeneric()) {
        for (GenericType* GenTy : Func->GenData->GenTys) {
            IdentRef* ConstraintRef = GenTy->GetConstraintRef();
            if (!ConstraintRef) continue;
            // TODO: Fix: this shows wrong information about original bind locations.
            CheckIdentRef<false>(ConstraintRef, false, nullptr, nullptr, true);
            if (!ConstraintRef->FoundBinding) {
                Func->ParsingError = true;
            }
        }

        if (Func->IsDefaultConstructor) {
            if (Func->ExplicitlyHasGenerics) {
                Error(Func, "Default constructor cannot have generic types");
                Func->ParsingError = true;
                return;
            }
        } else if (Func->IsCopyConstructor) {
            if (Func->ExplicitlyHasGenerics) {
                Error(Func, "Copy constructor cannot have generic types");
                Func->ParsingError = true;
                return;
            }
        } else if (Func->IsMoveConstructor) {
            if (Func->ExplicitlyHasGenerics) {
                Error(Func, "Move constructor cannot have generic types");
                Func->ParsingError = true;
                return;
            }
        } else if (Func->IsDestructor) {
            if (Func->ExplicitlyHasGenerics) {
                Error(Func, "Destructors cannot have generic types");
                Func->ParsingError = true;
                return;
            }
        }
    }

    NotFullyQualifiedVarState = true;

    llvm::SmallVector<VarDecl*, 2> Params = Func->Params;
    bool ParamsHaveAssignment = false, ParamAssignmentNotLast = false;
    for (VarDecl* Param : Params) {
        if (Param->TyIsInfered || !NotFullyQualified || !Func->IsGeneric()) {
            // Uses type inference so have to check the variable but need
            // to specify that generic types cannot be used to reduce the
            // assignment because the generic type is not known.
            CheckVarDecl(Param, true);

            if (Param->Ty == Context.ErrorType) {
                // TODO: Hacky so that it stops showing errors later on.
                Func->ParsingError = true;
                return;
            }
        } else {
            if (!FixupType(Param->Ty, false, true)) {
                Param->Ty = Context.ErrorType;
                Func->ParsingError = true;
                return;
            }

            if (!Param->Ty->ContainsGenerics) {
                if (Param->Ty->GetKind() == TypeKind::CStr) {
                    Param->HasConstAddress = true;
                }
            }
        }

        if (Param->Assignment) {
            ++Func->NumDefaultArgs;
            ParamsHaveAssignment = true;
        } else if (ParamsHaveAssignment) {
            ParamAssignmentNotLast = true;
        }
    }
    if (Func->IsVariadic) {

        if (ParamsHaveAssignment) {
            for (VarDecl* Param : Params) {
                if (Param->Assignment) {
                    Error(Param, "Variadic functions cannot have default arguments");
                    Func->ParsingError = true; // Hacky but reduces error messages later on.
                }
            }
        }

        VarDecl* Last = Func->Params[Func->Params.size() - 1];
        if (Last->Ty->GetKind() == TypeKind::Array) {
            Error(Last, "Cannot have variadic arguments of type '%s'", Last->Ty);
            Func->ParsingError = true; // Hacky but reduces error messages later on.
        }
        // TODO: can there be circular dependency here?
        Last->Ty = SliceType::Create(Last->Ty, Context);
    }
    NotFullyQualifiedVarState = false;

    if (ParamAssignmentNotLast) {
        Error(Func, "Parameter default arguments must come last in the parameter list");
        Func->ParsingError = true; // Hacky but reduces error messages later on.
    }

    if (!Func->RaisedErrors.empty()) {
        if (Context.StandAlone) {
            Error(Func, "Cannot raise errors when the -stand-alone flag is set");
        } else {
            for (auto& RaisedError : Func->RaisedErrors) {
                Decl* Decl = FindStructLikeTypeByName(RaisedError.Name);
                if (!Decl) {
                    Error(RaisedError.ErrorLoc, "Failed to find error struct for name '%s'",
                        RaisedError.Name);
                    Func->ParsingError = true;
                    continue;
                }
                if (!Decl->Is(AstKind::STRUCT_DECL)) {
                    Error(RaisedError.ErrorLoc, "Expected error type to be a struct");
                    Func->ParsingError = true;
                    continue;
                }
                StructDecl* ErrorStruct = static_cast<StructDecl*>(Decl);
                if (!ErrorStruct->HasBeenChecked) {
                    SemAnalyzer A(Context, ErrorStruct);
                    A.CheckStructDecl(ErrorStruct);
                }

                if (ErrorStruct->ParsingError) {
                    continue;
                }

                bool ImplementsErrorInterface = false;
                for (InterfaceDecl* Interface : ErrorStruct->Interfaces) {
                    if (Interface == Context.StdErrorInterface) {
                        ImplementsErrorInterface = true;
                        break;
                    }
                }
                if (!ImplementsErrorInterface) {
                    Error(RaisedError.ErrorLoc, "Raised error must implement error interface");
                    Func->ParsingError = true;
                    continue;
                }
                RaisedError.ErrorStruct = ErrorStruct;
            }
        }
    }

}

void arco::SemAnalyzer::CheckNode(AstNode* Node) {
    switch (Node->Kind) {
    case AstKind::VAR_DECL:
        CheckVarDecl(static_cast<VarDecl*>(Node));
        break;
    case AstKind::RETURN:
        CheckReturn(static_cast<ReturnStmt*>(Node));
        break;
    case AstKind::CONTINUE:
    case AstKind::BREAK:
        CheckLoopControl(static_cast<LoopControlStmt*>(Node));
        break;
    case AstKind::PREDICATE_LOOP:
        CheckPredicateLoop(static_cast<PredicateLoopStmt*>(Node));
        break;
    case AstKind::RANGE_LOOP:
        CheckRangeLoop(static_cast<RangeLoopStmt*>(Node));
        break;
    case AstKind::ITERATOR_LOOP:
        CheckIteratorLoop(static_cast<IteratorLoopStmt*>(Node));
        break;
    case AstKind::DELETE:
        CheckDeleteStmt(static_cast<DeleteStmt*>(Node));
        break;
    case AstKind::RAISE:
        CheckRaiseStmt(static_cast<RaiseStmt*>(Node));
        break;
    case AstKind::INITOBJ:
        CheckInitObj(static_cast<InitObjStmt*>(Node));
        break;
    case AstKind::IF: {
        bool Ignore1, Ignore2;
        CheckIf(static_cast<IfStmt*>(Node), Ignore1, Ignore2);
        break;
    }
    case AstKind::NESTED_SCOPE: {
        bool Ignore1, Ignore2;
        CheckNestedScope(static_cast<NestedScopeStmt*>(Node), Ignore1, Ignore2);
        break;
    }
    case AstKind::BINARY_OP:
        CheckBinaryOp(static_cast<BinaryOp*>(Node));
        break;
    case AstKind::UNARY_OP:
        CheckUnaryOp(static_cast<UnaryOp*>(Node));
        break;
    case AstKind::IDENT_REF:
        CheckIdentRef<false>(static_cast<IdentRef*>(Node), false, nullptr);
        break;
    case AstKind::FIELD_ACCESSOR:
        CheckFieldAccessor(static_cast<FieldAccessor*>(Node), false);
        break;
    case AstKind::THIS_REF:
        CheckThisRef(static_cast<ThisRef*>(Node));
        break;
    case AstKind::FUNC_CALL:
        CheckFuncCall(static_cast<FuncCall*>(Node));
        break;
    case AstKind::ARRAY:
        CheckArray(static_cast<Array*>(Node));
        break;
    case AstKind::ARRAY_ACCESS:
        CheckArrayAccess(static_cast<ArrayAccess*>(Node));
        break;
    case AstKind::TYPE_CAST:
        CheckTypeCast(static_cast<TypeCast*>(Node));
        break;
    case AstKind::TYPE_BITCAST:
        CheckTypeBitCast(static_cast<TypeBitCast*>(Node));
        break;
    case AstKind::STRUCT_INITIALIZER:
        CheckStructInitializer(static_cast<StructInitializer*>(Node));
        break;
    case AstKind::HEAP_ALLOC:
        CheckHeapAlloc(static_cast<HeapAlloc*>(Node), nullptr);
        break;
    case AstKind::SIZEOF:
        CheckSizeOf(static_cast<SizeOf*>(Node));
        break;
    case AstKind::TYPEOF:
        CheckTypeOf(static_cast<TypeOf*>(Node));
        break;
    case AstKind::TYPEID:
        CheckTypeId(static_cast<TypeId*>(Node));
        break;
    case AstKind::MOVEOBJ:
        CheckMoveObj(static_cast<MoveObj*>(Node));
        break;
    case AstKind::TERNARY:
        CheckTernary(static_cast<Ternary*>(Node));
        break;
    case AstKind::VAR_DECL_LIST:
        CheckVarDeclList(static_cast<VarDeclList*>(Node));
        break;
    case AstKind::TRY_ERROR:
        CheckTryError(static_cast<TryError*>(Node));
        break;
    case AstKind::CATCH_ERROR:
        CheckCatchError(static_cast<CatchError*>(Node), nullptr);
        break;
    case AstKind::NUMBER_LITERAL:
    case AstKind::STRING_LITERAL:
    case AstKind::NULLPTR:
    case AstKind::BOOL_LITERAL:
        break;
    default:
        assert(!"Failed to implement CheckNode case!");
        break;
    }
}

void arco::SemAnalyzer::FixupInterfaces(StructDecl* Struct) {
    for (const auto& InterfaceHook : Struct->InterfaceHooks) {
        FixupInterface(Struct, InterfaceHook);
    }
}

void arco::SemAnalyzer::FixupInterface(StructDecl* Struct, const StructDecl::InterfaceHook& InterfaceHook) {
    // TODO: May want to check for duplicate interface functions prior to this
    // and remove them prior to this to prevent uneeded error messages.
    Decl* FoundDecl = FindStructLikeTypeByName(InterfaceHook.Name);

    if (!FoundDecl) {
        Error(InterfaceHook.ErrorLoc, "Could not find interface by name '%s'", InterfaceHook.Name);
        return;
    }

    if (FoundDecl->IsNot(AstKind::INTERFACE_DECL)) {
        if (FoundDecl->Is(AstKind::STRUCT_DECL)) {
            // TODO: Explain why?
            Error(InterfaceHook.ErrorLoc, "Cannot extend from another struct");
        } else if (FoundDecl->Is(AstKind::ENUM_DECL)) {
            Error(InterfaceHook.ErrorLoc, "Cannot extend from enum");
        }
        return;
    }


    InterfaceDecl* Interface = static_cast<InterfaceDecl*>(FoundDecl);
    SemAnalyzer A(Context, Interface);
    A.CheckInterfaceDecl(Interface);

    if (Interface == Context.StdErrorInterface) {
        for (VarDecl* Field : Struct->Fields) {

            if (Field->Name != Context.ForcesRaiseIdentifier) continue;
            if (!Field->IsComptime()) {
                Error(Field, "Expected field '%s' to but compile time computable",
                    Context.ForcesRaiseIdentifier);
                break;
            }
            if (!Field->Ty->Equals(Context.BoolType)) {
                Error(Field, "Expected field '%s' to be of type bool",
                    Context.ForcesRaiseIdentifier);
                break;
            }
            if (!Field->Assignment) {
                Error(Field, "Expected field '%s' to have assignment",
                    Context.ForcesRaiseIdentifier);
                break;
            }

            llvm::Value* LLBool = GenFoldable(Field->Assignment->Loc, Field->Assignment);
            if (LLBool) {
                llvm::ConstantInt* LLInt = llvm::cast<llvm::ConstantInt>(LLBool);
                Struct->SetMustForceRaise(LLInt->getZExtValue() != 0);
            }

            break;
        }
    }

    Struct->Interfaces.push_back(Interface);
    // Finding the matching functions for the interface.
    for (FuncDecl* InterfaceFunc : Interface->Funcs) {

        if (InterfaceFunc->ParsingError) continue;

        auto Itr = std::find_if(Struct->Funcs.begin(), Struct->Funcs.end(),
            [InterfaceFunc](const auto& KeyValue) {
                return KeyValue.first == InterfaceFunc->Name;
            });
        if (Itr != Struct->Funcs.end()) {
            // Validating that the functions are a perfect match.
            // Unlike when checking for duplicate functions this must check
            // to make sure that everything is identical.
            FuncsList& FuncList = Itr->second;
            FuncDecl* FoundFunc = nullptr;
            bool FuncsHaveErrors = false;
            for (FuncDecl* Func : FuncList) {
                if (!Func->ParamTypesChecked) {
                    SemAnalyzer A(Context, Func);
                    A.CheckFuncParams(Func);
                }
                if (Func->ParsingError) {
                    FuncsHaveErrors = true;
                    break;
                }

                if (InterfaceFunc->Params.size() != Func->Params.size()) continue;
                if (!InterfaceFunc->RetTy->Equals(Func->RetTy)) continue;
                bool ParamsMatch = true;
                for (ulen i = 0; i < Func->Params.size(); i++) {
                    VarDecl* FuncParam = Func->Params[i];
                    VarDecl* InterfaceParam = InterfaceFunc->Params[i];
                    if (!FuncParam->Ty->Equals(InterfaceParam->Ty)) {
                        ParamsMatch = false;
                        break;
                    }
                    if (FuncParam->HasConstAddress != InterfaceParam->HasConstAddress) {
                        ParamsMatch = false;
                        break;
                    }
                }
                if (!ParamsMatch) continue;
                if (InterfaceFunc->IsVariadic != Func->IsVariadic) continue;
                if (InterfaceFunc->RaisedErrors.size() != Func->RaisedErrors.size()) continue;
                bool RaisedErrorsMatch = true;
                for (ulen i = 0; i < Func->RaisedErrors.size(); i++) {
                    const auto& FuncRaisedError      = Func->RaisedErrors[i];
                    const auto& InterfaceRaisedError = InterfaceFunc->RaisedErrors[i];
                    if (FuncRaisedError.ErrorStruct != InterfaceRaisedError.ErrorStruct) {
                        RaisedErrorsMatch = false;
                        break;
                    }
                }
                if (!RaisedErrorsMatch) continue;
                FoundFunc = Func;
                break;
            }
            if (FuncsHaveErrors) {
                // Do not report when there are functions with errors.
                continue;
            }

            if (!FoundFunc) {
                ReportAboutNoOverloadedInterfaceFunc(InterfaceHook.ErrorLoc, Interface, InterfaceFunc, &FuncList);
                continue;
            }
            if (FoundFunc->MappedInterfaceFunc) {
                // There is a conflict with the interfaces. Two interfaces must be requiring the
                // implementation of the same function definition.
                Error(InterfaceHook.ErrorLoc,
                        "Function conflict with interfaces '%s' and '%s'. Function '%s' is defined in both interfaces",
                        Interface->Name,
                        FoundFunc->MappedInterfaceFunc->Interface->Name,
                        GetFuncDefForError(ParamsToTypeInfo(FoundFunc), FoundFunc));
            }

            FoundFunc->MappedInterfaceFunc = InterfaceFunc;

            RequestGenNonGenericFunc(Context, FoundFunc);
        } else {
            Error(InterfaceHook.ErrorLoc, "Struct must implement function '%s' of interface '%s'",
                GetFuncDefForError(ParamsToTypeInfo(InterfaceFunc), InterfaceFunc), Interface->Name);
        }
    }
}


void arco::SemAnalyzer::ReportAboutNoOverloadedInterfaceFunc(SourceLoc ErrorLoc,
                                                             InterfaceDecl* Interface,
                                                             FuncDecl* InterfaceFunc,
                                                             FuncsList* Canidates) {

    std::string InterfaceFuncDef = GetFuncDefForError(ParamsToTypeInfo(InterfaceFunc), InterfaceFunc);
    if (Canidates->size() == 1) {
        FuncDecl* Canidate = (*Canidates)[0];

        std::string ErrorMsg = "Function '" + InterfaceFuncDef
                             + "' for interface '" + Interface->Name.Text.str() + "' does not match.";
        ErrorMsg += "\n\n";
        std::string ExtMsg = GetMismatchInfoForInterfaceFunc(InterfaceFunc, Canidate);
        Log.SetMsgToShowAbovePrimaryLocAligned(ExtMsg.c_str());
        Log.BeginError(Canidate->Loc, ErrorMsg.c_str(), false);
        Log.EndError();
        return;
    }


    std::string ErrorMsg = "Could not find overloaded function '" + InterfaceFuncDef
                         + "' for interface '" + Interface->Name.Text.str() + "'";
    ErrorMsg += "\n\n  Possible Canidates:";
    ulen LongestDefLength = 0;
    for (FuncDecl* Canidate : *Canidates) {
        ulen Len = GetFuncDefForError(ParamsToTypeInfo(Canidate), Canidate).length();
        if (Len > LongestDefLength) {
            LongestDefLength = Len;
        }
    }

    // TODO: Since this limits the amount of functions that are shown to the user it should select the
    // closest matches and show those to increase finding the right function.

    ulen Count = 0;
    for (FuncDecl* Canidate : *Canidates) {
        if (Count == Context.BailCountForShowingOverloadedFuncs) {
            ErrorMsg += "\n\n  And " + std::to_string(Canidates->size() - Count) + " more...";
            break;
        }
        llvm::SmallVector<TypeInfo> ParamTypes = ParamsToTypeInfo(Canidate);
        ErrorMsg += "\n\n     ";
        std::string Def = GetFuncDefForError(ParamTypes, Canidate);
        ErrorMsg += Def;
        ErrorMsg += std::string(LongestDefLength - Def.length(), ' ') + "   - declared at: "
            + Canidate->FScope->Path + std::string(":") + std::to_string(Canidate->Loc.LineNumber);
        ErrorMsg += "\n";
        std::string MismatchInfo = GetMismatchInfoForInterfaceFunc(InterfaceFunc, Canidate);
        std::stringstream StrStream(MismatchInfo.c_str());
        std::string Line;
        bool First = true;
        while (std::getline(StrStream, Line, '\n')) {
            if (!First) {
                ErrorMsg += "\n";
            }
            First = false;
            ErrorMsg += "        " + Line;
        }
        ++Count;
    }

    ErrorMsg += "\n";
    Log.BeginError(ErrorLoc, ErrorMsg.c_str(), false);
    Log.EndError();
}

std::string arco::SemAnalyzer::GetMismatchInfoForInterfaceFunc(FuncDecl* InterfaceFunc, FuncDecl* Canidate) {
    if (InterfaceFunc->Params.size() != Canidate->Params.size()) {
        return "- Incorrect number of parameters. Expected "
            + std::to_string(InterfaceFunc->Params.size())
            + ". Got " + std::to_string(Canidate->Params.size()) + ".";
    }

    bool EncounteredError = false;
    std::string MismatchInfo = "";
    if (!InterfaceFunc->RetTy->Equals(Canidate->RetTy)) {
        if (EncounteredError)  MismatchInfo += "\n";
        MismatchInfo += "- Return types do not match. Expected '"
                     + InterfaceFunc->RetTy->ToString() + "'. Got '"
                     + Canidate->RetTy->ToString() + "'.";
        EncounteredError = true;
    }
    for (ulen i = 0; i < Canidate->Params.size(); i++) {
        VarDecl* FuncParam = Canidate->Params[i];
        VarDecl* InterfaceParam = InterfaceFunc->Params[i];
        if (!FuncParam->Ty->Equals(InterfaceParam->Ty)) {
            if (EncounteredError)  MismatchInfo += "\n";
            MismatchInfo += "- Parameter " + std::to_string(i + 1) + " type does not match. Expected '"
                + InterfaceParam->Ty->ToString() + "'. Got '"
                + FuncParam->Ty->ToString() + "'.";
            EncounteredError = true;
        }
        if (FuncParam->HasConstAddress != InterfaceParam->HasConstAddress) {
            // TODO: Should this complain about constness for cstr if the types mismatch?
            if (EncounteredError)  MismatchInfo += "\n";
            if (FuncParam->HasConstAddress) {
                MismatchInfo += "- Parameter " + std::to_string(i + 1) + " is not meant to be const.";
            } else {
                MismatchInfo += "- Parameter " + std::to_string(i + 1) + " was expected to be const.";
            }
            EncounteredError = true;
        }
    }
    if (InterfaceFunc->IsVariadic != Canidate->IsVariadic) {
        if (EncounteredError)  MismatchInfo += "\n";
        if (InterfaceFunc->IsVariadic) {
            MismatchInfo += "- Expected variadic parameters.";
        } else {
            MismatchInfo += "- Does not take variadic parameters.";
        }
        EncounteredError = true;
    }
    bool MissingRaisedError = false;
    for (const auto& InterfaceRaisedError : InterfaceFunc->RaisedErrors) {
        auto Itr = std::find_if(Canidate->RaisedErrors.begin(),
                                Canidate->RaisedErrors.end(),
            [&InterfaceRaisedError](const auto& CanidateRaisedError) {
                return InterfaceRaisedError.ErrorStruct == CanidateRaisedError.ErrorStruct;
            });
        if (Itr == Canidate->RaisedErrors.end()) {
            if (EncounteredError)  MismatchInfo += "\n";
            MismatchInfo += "- Raises error '" + InterfaceRaisedError.Name.Text.str() + "' not raised by function.";
            MissingRaisedError = true;
        }
    }
    if (!MissingRaisedError) {
        if (InterfaceFunc->RaisedErrors.size() < Canidate->RaisedErrors.size()) {
            // May have raised an error that the interface does not raise.
            for (const auto& CanidateRaisedError : Canidate->RaisedErrors) {
                auto Itr = std::find_if(InterfaceFunc->RaisedErrors.begin(),
                                        InterfaceFunc->RaisedErrors.end(),
                    [&CanidateRaisedError](const auto& InterfaceRaisedError) {
                        return InterfaceRaisedError.ErrorStruct == CanidateRaisedError.ErrorStruct;
                    });
                if (Itr == InterfaceFunc->RaisedErrors.end()) {
                    if (EncounteredError)  MismatchInfo += "\n";
                    MismatchInfo += "- Raises error '" + CanidateRaisedError.Name.Text.str() + "' which the interface does not raise.";
                    MissingRaisedError = true;
                }
            }
        } else if (InterfaceFunc->RaisedErrors.size() == Canidate->RaisedErrors.size()) {
            // Might have raised errors out of order.
            bool RaisedOutOfError = false;
            for (ulen i = 0; i < Canidate->RaisedErrors.size(); i++) {
                const auto& CanidateRaisedError = Canidate->RaisedErrors[i];
                const auto& InterfaceRaisedError = InterfaceFunc->RaisedErrors[i];
                if (CanidateRaisedError.ErrorStruct != InterfaceRaisedError.ErrorStruct) {
                    RaisedOutOfError = true;
                    break;
                }
            }
            if (RaisedOutOfError) {
                if (EncounteredError)  MismatchInfo += "\n";
                MismatchInfo += "- Must raise all errors in the same order as the interface.";
            }
        }
    }

    return MismatchInfo;
}

void arco::SemAnalyzer::CheckEnumDecl(EnumDecl* Enum) {
    if (Enum->HasBeenChecked) return;
    Enum->HasBeenChecked = true;
    Context.UncheckedDecls.erase(Enum);
    if (Enum->ParsingError) return;

    Enum->IsBeingChecked = true;
    FScope  = Enum->FScope;


    Type* ValuesType = Enum->ValuesType;
    if (ValuesType) {
        FixupType(ValuesType);
        if (ValuesType == Context.ErrorType) {
            return;
        }
    }

    // TODO: could provide language optimization here by reordering the indexes
    // if they are not in order. Or possibly consider it an error and tell the user
    // to reorder their indexes.

    ulen ValueIndex = 0;
    for (ulen i = 0; i < Enum->Values.size(); i++) {
        EnumDecl::EnumValue& Value = Enum->Values[i];
        for (ulen j = i + 1; j < Enum->Values.size(); j++) {
            if (Value.Name == Enum->Values[j].Name) {
                Error(Value.Loc, "Duplicate name in enum");
            }
        }

        // TODO: should the error messages mark the location of the identifier instead?
        if (!Value.Assignment) {
            Value.Index = ValueIndex;
            ++ValueIndex;
            continue;
        }

        CheckNode(Value.Assignment);
        if (!Value.Assignment->IsFoldable) {
            Error(Value.Loc, "Could not compute enum value at compile time");
        }
        if (Value.Assignment->Ty == Context.ErrorType) {
            continue;
        }
        if (!ValuesType) {
            // TODO: should we readjust to the best possible index type?
            // similar to how arrays works.
            ValuesType = Value.Assignment->Ty;
        }


        if (Value.Assignment->Ty != Context.ErrorType) {
            if (!IsAssignableTo(ValuesType, Value.Assignment)) {
                Error(Value.Loc, "Cannot assign enum value of type '%s' to enum's value type '%s'",
                    Value.Assignment->Ty, ValuesType);
            } else {
                CreateCast(Value.Assignment, ValuesType);
                if (Value.Assignment->IsFoldable && Value.Assignment->Ty->IsInt()) {
                    // TODO: Check to make sure that the index is well.. indexable!

                    llvm::Value* LLValue = GenFoldable(Value.Assignment->Loc, Value.Assignment);
                    if (!LLValue)  continue;

                    llvm::ConstantInt* LLValueIndex = llvm::cast<llvm::ConstantInt>(LLValue);
                    ulen NewValueIndex = LLValueIndex->getZExtValue();
                    if (NewValueIndex < ValueIndex) {
                        Enum->IndexingInOrder = false;
                    }
                    ValueIndex = NewValueIndex;

                    // TODO: check to make sure that the value index fit's into the size of
                    // the enum's index type

                } else {
                    Enum->IndexingInOrder = false;
                }
            }
        }

        Value.Index = ValueIndex;
        ++ValueIndex;
    }
    Enum->ValuesType = ValuesType;

    if (!ValuesType) {
        // This can happen if there are no assignments.
        Enum->ValuesType = Context.IntType;
    }

    if (Enum->ValuesType->GetRealKind() == TypeKind::Enum) {
        Error(Enum, "Enums cannot have another enum as a value type");
    }

    if (!Enum->IndexingInOrder) {
        // The user did not provide a valid ordering to the indexes so
        // we must provide our own. Since we want to index into an array
        // the index will simply be the values into that array.
        ValueIndex = 0;
        for (EnumDecl::EnumValue& Value : Enum->Values) {
            Value.Index = ValueIndex++;
        }
    }

    Enum->IsBeingChecked = false;
}

void arco::SemAnalyzer::CheckInterfaceDecl(InterfaceDecl* Interface) {
    if (Interface->HasBeenChecked) return;
    Interface->HasBeenChecked = true;
    Context.UncheckedDecls.erase(Interface);
    if (Interface->ParsingError) return;

    Interface->IsBeingChecked = true;
    FScope = Interface->FScope;

    if (Interface->Funcs.empty()) {
        Error(Interface, "Interfaces must have at least one function declaration");
    }

    for (FuncDecl* Func : Interface->Funcs) {
        SemAnalyzer A(Context, Func);
        A.CheckFuncParams(Func);
    }
}

//===-------------------------------===//
// Statements
//===-------------------------------===//

void arco::SemAnalyzer::CheckScopeStmts(LexScope& LScope, Scope& NewScope) {
    NewScope.Parent = LocScope;
    LocScope = &NewScope;

    for (AstNode* Stmt : LScope.Stmts) {
        if (LocScope->FoundTerminal) {
            Error(Stmt, "Unreachable code");
            break;
        }

        // Ensuring that it is actually a valid statement.
        switch (Stmt->Kind) {
        case AstKind::VAR_DECL:
        case AstKind::RETURN:
        case AstKind::IF:
        case AstKind::PREDICATE_LOOP:
        case AstKind::RANGE_LOOP:
        case AstKind::ITERATOR_LOOP:
        case AstKind::FUNC_CALL:
        case AstKind::BREAK:
        case AstKind::CONTINUE:
        case AstKind::NESTED_SCOPE:
        case AstKind::DELETE:
        case AstKind::VAR_DECL_LIST:
        case AstKind::RAISE:
        case AstKind::TRY_ERROR:
        case AstKind::CATCH_ERROR:
        case AstKind::INITOBJ:
            break;
        case AstKind::BINARY_OP:
            switch (static_cast<BinaryOp*>(Stmt)->Op) {
            case '=':
            case TokenKind::PLUS_EQ:
            case TokenKind::MINUS_EQ:
            case TokenKind::STAR_EQ:
            case TokenKind::SLASH_EQ:
            case TokenKind::MOD_EQ:
            case TokenKind::AMP_EQ:
            case TokenKind::BAR_EQ:
            case TokenKind::CRT_EQ:
            case TokenKind::LT_LT_EQ:
            case TokenKind::GT_GT_EQ:
                break;
            default:
                Error(Stmt, "Incomplete statement");
                continue;
            }
            break;
        case AstKind::UNARY_OP:
            switch (static_cast<UnaryOp*>(Stmt)->Op) {
            case TokenKind::PLUS_PLUS:
            case TokenKind::POST_PLUS_PLUS:
            case TokenKind::MINUS_MINUS:
            case TokenKind::POST_MINUS_MINUS:
            case TokenKind::TLD_DOT:
                break;
            default:
                Error(Stmt, "Incomplete statement");
                continue;
            }
            break;
        case AstKind::FUNC_DECL:
            Error(Stmt, "No support for nested function declarations at this time");
            continue;
        case AstKind::STRUCT_DECL:
            Error(Stmt, "No support for declaring structs within this scope at this time");
            continue;
        case AstKind::ENUM_DECL:
            Error(Stmt, "No support for declaring enums within this scope at this time");
            continue;
        default:
            Error(Stmt, "Incomplete statement");
            continue;
        }

        CheckNode(Stmt);
    }

    LocScope = LocScope->Parent;
}

void arco::SemAnalyzer::CheckVarDecl(VarDecl* Var, bool PartialGenFixup, bool ForceCheck) {

    if (Var->HasBeenChecked && !Var->IsGeneric() && !ForceCheck) {
        if (Var->IsField() || Var->IsGlobal) {
            return;
        }
    }

    Var->HasBeenChecked = true;
    if (Var->IsGlobal) {
        Context.UncheckedDecls.erase(Var);
        CGlobal = Var;
    }
    if (Var->ParsingError) {
        Var->Ty = Context.ErrorType;
        return;
    }
    Var->IsBeingChecked = true;

    FScope = Var->FScope;
    Mod    = Var->Mod;
    CVar   = Var;

    if (Var->IsField()) {
        CField = Var;
    }

    if (Var->IsGeneric()) {
        CreateQualifications(Var);
    }

    if (Var->Mods & ModKinds::NATIVE) {
        if (!Var->IsGlobal) {
            Error(Var, "Only global variables can be marked as native");
        }
        if (Var->Assignment) {
            Error(Var, "Variables marked native cannot have assignment");
        }
    }

    // TODO: There is a problem at the moment with generic structs and error reporting
    // because if the type of the variable is set to an error the future reporting of
    // errors will be worse.

    // TODO: Is it needed to store a previous CGlobal/CField
    //       and set it to that once the function returns?
#define VAR_YIELD(E, TyErr)       \
Var->IsBeingChecked = false;      \
CGlobal = nullptr;                \
CField  = nullptr;                \
CVar    = nullptr;                \
E;                                \
if (Var->IsGeneric())             \
    Var->GenData                  \
       ->CurBinding               \
       ->QualifiedType = Var->Ty; \
if constexpr (TyErr) {            \
    Var->Ty = Context.ErrorType;  \
}                                 \
return;

    if (!Var->TyIsInfered && !FixupType(Var->Ty, false, PartialGenFixup)) {
        VAR_YIELD(, true);
    }

    if (Var->IsGeneric() && !Var->ExplicitlyMarkedConst) {
        VAR_YIELD(Error(Var, "Generic variables must be marked as const"), true);
    }

    if (Var->Assignment) {
        if (Var->Assignment->Is(AstKind::CATCH_ERROR)) {
            CatchError* Catch = static_cast<CatchError*>(Var->Assignment);
            CheckCatchError(Catch, Var);
        } else if (Var->Assignment->Is(AstKind::HEAP_ALLOC)) {
            HeapAlloc* Alloc = static_cast<HeapAlloc*>(Var->Assignment);
            CheckHeapAlloc(Alloc, Var->Ty);
        } else {
            CheckNode(Var->Assignment);
        }

        if (Var->Assignment->Ty == Context.ErrorType) {
            VAR_YIELD(, true);
        }

        if (Var->TyIsInfered) {
            // The type is infered.

            Var->Ty = Var->Assignment->Ty;
            if (Var->Assignment->HasConstAddress &&
                (Var->Assignment->Ty->IsPointer() || Var->Assignment->Ty->GetKind() == TypeKind::Array)) {
                // If the memory is protected from the infered assignment take on that
                // constness.
                Var->HasConstAddress = true;
            }

            switch (Var->Ty->GetKind()) {
            case TypeKind::Null:
                VAR_YIELD(Error(Var, "Cannot infer pointer type from null"), true);
                break;
            case TypeKind::Array: {
                ArrayType* ArrayTy = Var->Ty->AsArrayTy();
                Type* BaseTy = ArrayTy->GetBaseType();
                TypeKind Kind = BaseTy->GetKind();
                if (Kind == TypeKind::EmptyArrayElm) {
                    VAR_YIELD(Error(Var, "Cannot infer array type from an empty array"), true);
                } else if (Kind == TypeKind::Null) {
                    VAR_YIELD(Error(Var, "Cannot infer array pointer type from a null array"), true);
                } else if (Kind == TypeKind::Import || Kind == TypeKind::FuncRef) {
                    VAR_YIELD(Error(Var, "Cannot infer type from an incomplete expression"), true);
                }
                break;
            }
            case TypeKind::Import:
            case TypeKind::FuncRef:
            case TypeKind::StructRef:
            case TypeKind::EnumRef:
            case TypeKind::InterfaceRef:
                VAR_YIELD(Error(Var, "Cannot infer type from an incomplete expression"), true);
                break;
            }
        } else if (Var->Ty->GetKind() == TypeKind::Array &&
            !Var->Ty->AsArrayTy()->GetLengthExpr()) {

            if (Var->Assignment->IsNot(AstKind::ARRAY)) {
                VAR_YIELD(Error(Var, "Expected array declaration for implicit array type"), true);
            }

            ArrayType* ImplicitArrayType = Var->Ty->AsArrayTy();
            ArrayType* FromArrayType     = Var->Assignment->Ty->AsArrayTy();
            if (ImplicitArrayType->GetDepthLevel() != FromArrayType->GetDepthLevel()) {
                VAR_YIELD(Error(Var, "Incompatible depth with initializer array for implicit array type"), true);
            }

            if (!IsAssignableTo(ImplicitArrayType->GetBaseType(),
                                FromArrayType->GetBaseType(),
                                nullptr)) {
                VAR_YIELD(
                    Error(Var,
                        "Cannot assign array with element types '%s' to implicit array element types '%s'",
                        FromArrayType->GetBaseType(),
                        ImplicitArrayType->GetBaseType()),
                    true);
            }

            ImplicitArrayType->AssignLength(FromArrayType->GetLength());
            while (true) {
                Type* ElmType = FromArrayType->GetElementType();

                if (ElmType->GetKind() == TypeKind::Array) {
                    FromArrayType     = FromArrayType->GetElementType()->AsArrayTy();
                    ImplicitArrayType = ImplicitArrayType->GetElementType()->AsArrayTy();
                    ImplicitArrayType->AssignLength(FromArrayType->GetLength());
                } else {
                    break;
                }
            }

        } else if (IsAssignableTo(Var->Ty, Var->Assignment)) {
            CreateCast(Var->Assignment, Var->Ty);
        } else {
            VAR_YIELD(
                DisplayErrorForTypeMismatch(
                    "Cannot assign value of type '%s' to variable of type '%s'",
                    Var->Loc,
                    Var->Assignment,
                    Var->Ty),
                true);
        }

        if (Var->Ty->Equals(Context.CStrType)) {
            Var->HasConstAddress = true;
        }

        if (ViolatesConstAssignment(Var, Var->Assignment)) {
            VAR_YIELD(Error(Var, "Cannot assign const memory to non-const variable"), true);
        }
    } else {
        // No assignment.

        if (Var->TyIsInfered) {
            VAR_YIELD(Error(Var, "Cannot infer type because there is no assignment"), true);
        }

        if (Var->Ty->Equals(Context.CStrType)) {
            Var->HasConstAddress = true;
        }

        StructDecl* StructForTy = nullptr;
        if (Var->Ty->GetKind() == TypeKind::Struct) {
            StructForTy = Var->Ty->AsStructType()->GetStruct();
        } else if (Var->Ty->GetKind() == TypeKind::Array) {
            ArrayType* ArrayTy = Var->Ty->AsArrayTy();
            Type* BaseTy = ArrayTy->GetBaseType();
            if (BaseTy->GetKind() == TypeKind::Struct) {
                StructForTy = BaseTy->AsStructType()->GetStruct();
            }
        }

        if (StructForTy) {
            if (!StructForTy->Constructors.empty() && !StructForTy->DefaultConstructor &&
                !Var->IsField() && !Var->IsParam()) {
                VAR_YIELD(Error(Var, "No default constructor to initialize the variable"), false);
            }
        }
    }

    if (Var->Ty->GetKind() == TypeKind::Struct) {
        StructType* StructTy = Var->Ty->AsStructType();
        if (StructTy->GetStruct()->IsBeingChecked) {
            VAR_YIELD(
                Log.BeginError(
                        Var->Loc,
                        "Cannot declare variable with struct type '%s' because the type is incomplete",
                        StructTy);
                Log.AddNoteLine([](llvm::raw_ostream& OS) {
                    OS << "This often happens due to cyclical struct dependencies.";
                });
                Log.AddNoteLine([](llvm::raw_ostream& OS) {
                    OS << "Example:";
                });
                Log.AddNoteLine([](llvm::raw_ostream& OS) {
                    OS << "\tA struct { b B; } // A depends on B.";
                });
                Log.AddNoteLine([](llvm::raw_ostream& OS) {
                    OS << "\tB struct { a A; } // B depends on A.";
                });
                Log.EndError();
                    , true);
        }
    } else if (Var->Ty->GetKind() == TypeKind::Interface) {
        Error(Var, "Cannot declare a variable type as an interface type. You must use a pointer");
    }

    if (Var->HasConstAddress && !Var->Assignment &&
        !Var->Ty->IsPointer() && !Var->IsParam()) {
        Error(Var, "Must initialize variables marked with const");
    }

    if (Var->Ty->Equals(Context.VoidType)) {
        VAR_YIELD(Error(Var, "Variables cannot have type 'void'"), true);
    }

    // Keep this after having checked the assignment so that they are generated based
    // on dependency order.
    if (Var->IsGlobal && !Var->IsGeneric()) {
        Context.RequestGen(Var);
    }

    if (Var->IsGeneric() && !Var->Assignment->IsFoldable) {
        VAR_YIELD(Error(Var, "Generic variables must be computable at compile time"), true);
    }

    if (Var->IsGeneric()) {
        if (Var->IsComptime()) {
            if (llvm::Value* LLResult = GenFoldable(Var->Assignment->Loc, Var->Assignment)) {
                Var->GenData->CurBinding->LLValue = llvm::cast<llvm::Constant>(LLResult);
            } else {
                Var->Ty = Context.ErrorType;
            }
        }
    } else if (Var->IsComptime()) {
        if (llvm::Value* LLResult = GenFoldable(Var->Assignment->Loc, Var->Assignment)) {
            Var->LLComptimeVal = llvm::cast<llvm::Constant>(LLResult);
        }
    }

    VAR_YIELD(, false);

#undef VAR_YIELD
}

void arco::SemAnalyzer::CheckReturn(ReturnStmt* Return) {
    LocScope->FoundTerminal  = true;
    LocScope->AllPathsReturn = true;
    LocScope->AllPathsBranch = true;

    if (Return->Value) {
        CheckNode(Return->Value);
        YIELD_IF_ERROR(Return->Value);
    }

    if (CFunc->RetTy == Context.ErrorType) {
        return;
    }

    bool ReturnMatched = true;
    if (Return->Value) {
        if (IsAssignableTo(CFunc->RetTy, Return->Value)) {
            CreateCast(Return->Value, CFunc->RetTy);
            if (CFunc->RetTy == Context.VoidType) {
                // returning a void value could happen if the user calls another
                // function that returns void. You cannot return void values.
                Error(Return, "functions that have a return type of void cannot return a value");
            }
        } else {
            ReturnMatched = false;
        }
        if (Return->Value->Is(AstKind::IDENT_REF)) {
            IdentRef* IRef = static_cast<IdentRef*>(Return->Value);
            if (IRef->RefKind == IdentRef::RK::Var) {
                VarDecl* Var = IRef->Var;
                if (!Var->IsParam() && !Var->IsField() && !Var->IsGlobal) {
                    Var->IsLocalRetValue = true;
                }
            }
        }
    } else {
        ReturnMatched = CFunc->RetTy == Context.VoidType;
    }

    if (!ReturnMatched) {
        Error(Return, "Expected return type '%s' but got '%s'",
            CFunc->RetTy,
            Return->Value ? Return->Value->Ty : Context.VoidType);
    }
}

void arco::SemAnalyzer::CheckLoopControl(LoopControlStmt* LoopControl) {
    LocScope->FoundTerminal = true;
    LocScope->AllPathsBranch = true;

    if (LoopDepth == 0) {
        if (LoopControl->Kind == AstKind::BREAK) {
            Error(LoopControl, "break statements may only be used inside of loops");
            return;
        } else {
            Error(LoopControl, "continue statements may only be used inside of loops");
            return;
        }
    }

    if (LoopControl->LoopCount > LoopDepth) {
        if (LoopControl->Kind == AstKind::BREAK) {
            Error(LoopControl, "number of requested breaks exceeds the loop depth");
        } else {
            Error(LoopControl, "number of requested continues exceeds the loop depth");
        }
    }
}

void arco::SemAnalyzer::CheckPredicateLoop(PredicateLoopStmt* Loop) {

    if (Loop->Cond) {
        if (Loop->Cond->Is(AstKind::RANGE)) {
            CheckRange(static_cast<Range*>(Loop->Cond));
        } else {
            CheckCondition(Loop->Cond, "Loop");
        }
    }

    ++LoopDepth;
    Scope LoopScope;
    CheckScopeStmts(Loop->Scope, LoopScope);
    --LoopDepth;
}

void arco::SemAnalyzer::CheckRangeLoop(RangeLoopStmt* Loop) {
    for (AstNode* InitNode : Loop->InitNodes) {
        CheckNode(InitNode);
    }
    if (Loop->Cond) {
        CheckCondition(Loop->Cond, "Loop");
    }
    for (Expr* Inc : Loop->Incs) {
        CheckNode(Inc);
    }

    ++LoopDepth;
    Scope LoopScope;
    CheckScopeStmts(Loop->Scope, LoopScope);
    --LoopDepth;
}

void arco::SemAnalyzer::CheckIteratorLoop(IteratorLoopStmt* Loop) {
    if (Loop->VarVal->Ty) {
        if (!FixupType(Loop->VarVal->Ty, false)) {
            return;
        }
    }

    Type* IterOnType;
    if (Loop->IterOnExpr->Is(AstKind::RANGE)) {
        Range* Rg = static_cast<Range*>(Loop->IterOnExpr);
        CheckRange(Rg);
        YIELD_IF_ERROR(Rg);
        IterOnType = Rg->Ty;
    } else {
        CheckNode(Loop->IterOnExpr);
        YIELD_IF_ERROR(Loop->IterOnExpr);

        Type* IterableType = Loop->IterOnExpr->Ty;
        if (IterableType->GetKind() == TypeKind::Array || IterableType->GetKind() == TypeKind::Slice) {
            IterOnType = IterableType->AsContainerType()->GetElementType();
        } else {
            // TODO: expanded location?
            Error(Loop->IterOnExpr->Loc, "Cannot iterate on type '%s'", IterableType);
            return;
        }
    }

    if (Loop->VarVal->Ty) {
        if (!IsAssignableTo(Loop->VarVal->Ty, IterOnType, nullptr)) {
            // Maybe the iteration may happen by pointers instead.

            if (!(Loop->VarVal->Ty->GetKind() == TypeKind::Pointer &&
                  IterOnType->Equals(Loop->VarVal->Ty->AsPointerTy()->GetElementType())
                )) {
                Error(Loop->VarVal->Loc, "Cannot assign type '%s' to variable of type '%s'",
                    IterOnType, Loop->VarVal->Ty);
                return;
            }
        }
    } else {
        // Type is infered.
        if (Loop->InfersPtrTy) {
            Loop->VarVal->Ty = PointerType::Create(IterOnType, Context);
        } else {
            Loop->VarVal->Ty = IterOnType;
        }
    }

    ++LoopDepth;
    Scope LoopScope;
    CheckScopeStmts(Loop->Scope, LoopScope);
    --LoopDepth;
}

void arco::SemAnalyzer::CheckDeleteStmt(DeleteStmt* Delete) {
    CheckNode(Delete->Value);
    YIELD_IF_ERROR(Delete->Value);

    TypeKind Kind = Delete->Value->Ty->GetKind();
    if (Kind != TypeKind::Pointer &&
        Kind != TypeKind::Slice
        ) {
        Error(Delete, "Cannot delete type '%s'", Delete->Value->Ty);
    }
}

void arco::SemAnalyzer::CheckIf(IfStmt* If, bool& AllPathsReturn, bool& AllPathsBranch) {

    CheckCondition(If->Cond, "If");

    Scope IfBodyScope;
    CheckScopeStmts(If->Scope, IfBodyScope);
    AllPathsReturn = If->Else && IfBodyScope.AllPathsReturn;
    AllPathsBranch = If->Else && IfBodyScope.AllPathsBranch;

    if (If->Else) {
        if (If->Else->Is(AstKind::IF)) {
            // This is an else if case.
            bool AllPathsReturn2, AllPathsBranch2;
            CheckIf(static_cast<IfStmt*>(If->Else), AllPathsReturn2, AllPathsBranch2);
            AllPathsReturn &= AllPathsReturn2;
            AllPathsBranch &= AllPathsBranch2;
        } else {
            bool AllPathsReturn2, AllPathsBranch2;
            CheckNestedScope(static_cast<NestedScopeStmt*>(If->Else), AllPathsReturn2, AllPathsBranch2);
            AllPathsReturn &= AllPathsReturn2;
            AllPathsBranch &= AllPathsBranch2;
        }
    }

    LocScope->AllPathsBranch = AllPathsBranch;
    LocScope->AllPathsReturn = AllPathsReturn;
    LocScope->FoundTerminal |= AllPathsReturn;

}

void arco::SemAnalyzer::CheckNestedScope(NestedScopeStmt* NestedScope, bool& AllPathsReturn, bool& AllPathsBranch) {
    Scope NewScope;
    CheckScopeStmts(NestedScope->Scope, NewScope);
    AllPathsReturn = NewScope.AllPathsReturn;
    AllPathsBranch = NewScope.AllPathsBranch;
}

void arco::SemAnalyzer::CheckRaiseStmt(RaiseStmt* Raise) {
    LocScope->FoundTerminal = true;
    LocScope->AllPathsReturn = true;

    CheckNode(Raise->StructInit);
    YIELD_IF_ERROR(Raise->StructInit);

    if (Context.StandAlone) {
        Error(Raise, "Cannot raise errors when -stand-alone is enabled");
        return;
    }

    StructInitializer* StructInit = static_cast<StructInitializer*>(Raise->StructInit);
    StructType* StructTy = StructInit->Ty->AsStructType();
    StructDecl* Struct = StructTy->GetStruct();

    bool ImplementsErrorInterface = false;
    for (InterfaceDecl* Interface : Struct->Interfaces) {
        if (Interface == Context.StdErrorInterface) {
            ImplementsErrorInterface = true;
            break;
        }
    }
    if (Struct->ParsingError) {
        return;
    }

    if (!ImplementsErrorInterface) {
        Error(Raise, "Struct type '%s' being raised does not implement Error interface",
            Struct->Name);
        return;
    }

    if (StructTy->DoesMustForceRaise()) {
        bool FuncRaisesError = false;
        for (const auto& RaisedError : CFunc->RaisedErrors) {
            if (RaisedError.ErrorStruct == Struct) {
                FuncRaisesError = true;
                break;
            }

        }

        if (!FuncRaisesError) {
            Log.BeginError(Raise->Loc, "Cannot raise error '%s' because the function signature does not raise that error",
                Struct->Name);
            Log.AddNoteLine([=](llvm::raw_ostream& OS) {
                OS << "Your function needs to be declared as:  "
                    << GetFuncDefForError(ParamsToTypeInfo(CFunc), CFunc)
                    << " raises ";
                for (ulen i = 0; i < CFunc->RaisedErrors.size(); i++) {
                    const auto& RaisedError = CFunc->RaisedErrors[i];
                    OS << RaisedError.Name << ", ";
                }
                OS << Struct->Name;
                });
            Log.EndError();
        }
    } else {
        CheckStdPanicFuncExists();
    }
}

//===-------------------------------===//
// Expressions
//===-------------------------------===//

namespace arco {

static Type* DetermineTypeFromIntTypes(ArcoContext& Context, Expr* LHS, Expr* RHS) {
    if (LHS->Is(AstKind::NUMBER_LITERAL)) {
        return RHS->Ty;
    } else if (RHS->Is(AstKind::NUMBER_LITERAL)) {
        return LHS->Ty;
    } else if (RHS->Ty->Equals(LHS->Ty)) {
        return RHS->Ty;
    } else {
        return Context.ErrorType;
    }
}

static Type* DetermineTypeFromNumberTypes(ArcoContext& Context, Expr* LHS, Expr* RHS) {
    if (LHS->Ty->IsInt() && RHS->Ty->IsInt()) {
        return DetermineTypeFromIntTypes(Context, LHS, RHS);
    } else {
        if (LHS->Ty->IsSystemInt()) {
            return RHS->Ty; // Take on the float size
        } else if (RHS->Ty->IsSystemInt()) {
            return LHS->Ty; // Take on the float size
        } else {
            ulen LargerMemSize = max(LHS->Ty->GetTrivialTypeSizeInBytes(), RHS->Ty->GetTrivialTypeSizeInBytes());
            return Type::GetFloatTypeBasedOnByteSize(LargerMemSize, Context);
        }
    }
}

}

void arco::SemAnalyzer::CheckBinaryOp(BinaryOp* BinOp) {

    CheckNode(BinOp->LHS);
    if (BinOp->RHS->Is(AstKind::HEAP_ALLOC) && BinOp->Op == '=') {
        CheckHeapAlloc(static_cast<HeapAlloc*>(BinOp->RHS), BinOp->LHS->Ty);
    } else {
        CheckNode(BinOp->RHS);
    }

    Type* LTy = BinOp->LHS->Ty;
    Type* RTy = BinOp->RHS->Ty;

    if (LTy == Context.ErrorType || RTy == Context.ErrorType) {
        YIELD_ERROR(BinOp);
    }

    if (!BinOp->LHS->IsFoldable || !BinOp->RHS->IsFoldable) {
        BinOp->IsFoldable = false;
    }

#define OPERATOR_CANNOT_APPLY(T)                                         \
Error(BinOp, "Operator '%s' cannot apply to type '%s'   ('%s' %s '%s')", \
    Token::TokenKindToString(BinOp->Op, Context),                        \
    T,                                                                   \
    LTy,                                                                 \
    Token::TokenKindToString(BinOp->Op, Context),		                 \
    RTy                                                                  \
    );                                                                   \
YIELD_ERROR(BinOp)

#define TYPE_MISMATCH()                                                  \
Error(BinOp, "Operator '%s' had mismatched types   ('%s' %s '%s')",      \
    Token::TokenKindToString(BinOp->Op, Context),                        \
    LTy,                                                                 \
    Token::TokenKindToString(BinOp->Op, Context),		                 \
    RTy                                                                  \
    );                                                                   \
YIELD_ERROR(BinOp)

    switch (BinOp->Op) {
    case '=':
    case TokenKind::PLUS_EQ: case TokenKind::MINUS_EQ:
    case TokenKind::STAR_EQ: case TokenKind::SLASH_EQ:
    case TokenKind::MOD_EQ: case TokenKind::AMP_EQ:
    case TokenKind::CRT_EQ: case TokenKind::BAR_EQ:
    case TokenKind::LT_LT_EQ: case TokenKind::GT_GT_EQ: {

        CheckModifibility(BinOp->LHS);

        bool UsesPointerArithmetic = false;
        switch (BinOp->Op) {
        case TokenKind::PLUS_EQ: case TokenKind::MINUS_EQ: {
            if (LTy->IsPointer()) {
                if (!RTy->IsInt()) {
                    Error(BinOp->RHS, "Pointer arithmetic expects integer value");
                    YIELD_ERROR(BinOp);
                }
                UsesPointerArithmetic = true;
            } else {
                if (!RTy->IsNumber()) {
                    OPERATOR_CANNOT_APPLY(RTy);
                }
                if (!LTy->IsNumber()) {
                    OPERATOR_CANNOT_APPLY(LTy);
                }
            }
            break;
        }
        case TokenKind::STAR_EQ: case TokenKind::SLASH_EQ: {
            if (!RTy->IsNumber()) {
                OPERATOR_CANNOT_APPLY(RTy);
            }
            if (!LTy->IsNumber()) {
                OPERATOR_CANNOT_APPLY(LTy);
            }
            break;
        }
        case TokenKind::MOD_EQ:
        case TokenKind::LT_LT_EQ: case TokenKind::GT_GT_EQ: {
            if (!RTy->IsInt()) {
                OPERATOR_CANNOT_APPLY(RTy);
            }
            if (!LTy->IsInt()) {
                OPERATOR_CANNOT_APPLY(LTy);
            }
            break;
        }
        case TokenKind::BAR_EQ: case TokenKind::AMP_EQ:
        case TokenKind::CRT_EQ: {
            // TODO: Support boolean modifications.
            if (!RTy->IsInt()) {
                OPERATOR_CANNOT_APPLY(RTy);
            }
            if (!LTy->IsInt()) {
                OPERATOR_CANNOT_APPLY(LTy);
            }
            break;
        }
        default:
            break;
        }

        if (BinOp->LHS->Is(AstKind::FIELD_ACCESSOR) ||
            BinOp->LHS->Is(AstKind::IDENT_REF)) {
            IdentRef* IRef = static_cast<IdentRef*>(BinOp->LHS);
            if (IRef->RefKind == IdentRef::RK::Var && (IRef->Var->Mods & ModKinds::READONLY)) {
                if (IRef->Var->FScope != FScope) {
                    Error(BinOp, "Cannot modify variable marked readonly");
                }
            }
        }

        if (BinOp->Op == TokenKind::SLASH_EQ || BinOp->Op == TokenKind::MOD_EQ) {
            if (BinOp->RHS->IsFoldable) {
                llvm::Value* LLValue = GenFoldable(BinOp->RHS->Loc, BinOp->RHS);
                if (LLValue) {
                    llvm::Constant* LLInt = llvm::cast<llvm::Constant>(LLValue);
                    if (LLInt->isZeroValue()) {
                        Error(BinOp, "Division by zero");
                    }
                }
            }
        }

        if (!UsesPointerArithmetic) {
            if (LTy->GetKind() == TypeKind::Array) {
                // TODO: May want to change this requirement.
                Error(BinOp, "Cannot reassign the value of an array");
                YIELD_ERROR(BinOp);
            }

            if (!IsAssignableTo(LTy, BinOp->RHS)) {
                // Check for implicit derefencing for function calls.


                DisplayErrorForTypeMismatch(
                    "Cannot assign value of type '%s' to variable of type '%s'",
                    BinOp->Loc,
                    BinOp->RHS,
                    LTy);
                YIELD_ERROR(BinOp);
            }

            CreateCast(BinOp->RHS, LTy);
        }

        BinOp->Ty = LTy;
        break;
    }
    case '+': case '-': {
        // Pointers/arrays are included so that pointer arithmetic
        // can be performed.

        bool LPtrLike = LTy->IsPointer() || LTy->GetKind() == TypeKind::Array;
        bool RPtrLike = RTy->IsPointer() || RTy->GetKind() == TypeKind::Array;

        if (!(LTy->IsNumber() || LPtrLike)) {
            OPERATOR_CANNOT_APPLY(LTy);
        }
        if (!(RTy->IsNumber() || RPtrLike)) {
            OPERATOR_CANNOT_APPLY(RTy);
        }

        if (LPtrLike || RPtrLike) {
            // Pointer arithmetic

            if (BinOp->Op == '-' && LTy->IsNumber()) {
                Error(BinOp, "Cannot subtract a %s from a number",
                    RTy->IsPointer() ? "pointer" : "array");
                YIELD_ERROR(BinOp);
            }

            if (LPtrLike) {
                // LHS has memory

                if (!RTy->IsInt()) {
                    Error(BinOp, "Pointer arithmetic expects integer value");
                    YIELD_ERROR(BinOp);
                }

                BinOp->HasConstAddress = BinOp->LHS->HasConstAddress;

                if (LTy->IsPointer()) {
                    BinOp->Ty = LTy;
                } else {
                    BinOp->Ty = PointerType::Create(LTy->AsArrayTy()->GetElementType(), Context);
                }
            } else {
                // RHS has memory

                if (!LTy->IsInt()) {
                    Error(BinOp, "Pointer arithmetic expects integer value");
                    YIELD_ERROR(BinOp);
                }

                BinOp->HasConstAddress = BinOp->RHS->HasConstAddress;

                if (RTy->IsPointer()) {
                    BinOp->Ty = RTy;
                } else {
                    BinOp->Ty = PointerType::Create(RTy->AsArrayTy()->GetElementType(), Context);
                }
            }

        } else {
            // Not pointer arithmetic
            Type* ToType = DetermineTypeFromNumberTypes(Context, BinOp->LHS, BinOp->RHS);
            if (ToType == Context.ErrorType) {
                TYPE_MISMATCH();
            }

            CreateCast(BinOp->LHS, ToType);
            CreateCast(BinOp->RHS, ToType);
            BinOp->Ty = ToType;
        }

        break;
    }
    case '*': case '/': {
        if (!LTy->IsNumber()) {
            OPERATOR_CANNOT_APPLY(LTy);
        }
        if (!RTy->IsNumber()) {
            OPERATOR_CANNOT_APPLY(LTy);
        }

        if (BinOp->Op == '/' && BinOp->RHS->IsFoldable) {
            llvm::Value* LLValue = GenFoldable(BinOp->RHS->Loc, BinOp->RHS);
            if (LLValue) {
                llvm::Constant* LLInt = llvm::cast<llvm::Constant>(LLValue);
                if (LLInt->isZeroValue()) {
                    Error(BinOp, "Division by zero");
                }
            }
        }

        Type* ToType = DetermineTypeFromNumberTypes(Context, BinOp->LHS, BinOp->RHS);
        if (ToType == Context.ErrorType) {
            TYPE_MISMATCH();
        }

        CreateCast(BinOp->LHS, ToType);
        CreateCast(BinOp->RHS, ToType);
        BinOp->Ty = ToType;
        break;
    }
    case '%': case TokenKind::LT_LT: case TokenKind::GT_GT: {
        if (!LTy->IsInt()) {
            OPERATOR_CANNOT_APPLY(LTy);
        }
        if (!RTy->IsInt()) {
            OPERATOR_CANNOT_APPLY(RTy);
        }

        if (BinOp->Op == '%' && BinOp->RHS->IsFoldable) {
            llvm::Value* LLValue = GenFoldable(BinOp->RHS->Loc, BinOp->RHS);
            if (LLValue) {
                llvm::ConstantInt* LLInt = llvm::cast<llvm::ConstantInt>(LLValue);
                if (LLInt->isZeroValue()) {
                    Error(BinOp, "Division by zero");
                }
            }
        } else if (BinOp->RHS->IsFoldable) { // << or >>
            llvm::Value* LLValue = GenFoldable(BinOp->RHS->Loc, BinOp->RHS);
            if (LLValue) {
                llvm::ConstantInt* LLInt = llvm::cast<llvm::ConstantInt>(LLValue);
                u64 ShiftVal = LLInt->getZExtValue();
                if (ShiftVal == 0) {
                    // TODO: Should this error? Shifting zero is a pointless operation!
                } else if (ShiftVal - 1 > LTy->GetSizeInBytes(Context.LLArcoModule) * 8) {
                    Error(BinOp, "Shifting bits larger than bit size of type '%s'", LTy);
                }
            }
        }

        // Want the LHS to determine the type since it is what gets
        // operated on.

        // TODO: this implies we should check to make sure that the RHS
        // is not a larger size than the LHS!!

        Type* ToType = LTy;

        CreateCast(BinOp->LHS, ToType);
        CreateCast(BinOp->RHS, ToType);
        BinOp->Ty = ToType;
        break;
    }
    case '|': case '&': case '^': {
        bool RIsBool = RTy == Context.BoolType;
        bool LIsBool = LTy == Context.BoolType;
        if (RIsBool || LIsBool) {
            if (!(RIsBool && LIsBool)) {
                Error(BinOp,
                      "Both sides of the operator '%s' must both be a booleans or integers   ('%s' %s '%s')",
                      Token::TokenKindToString(BinOp->Op, Context),
                      LTy,
                      Token::TokenKindToString(BinOp->Op, Context),
                      RTy
                );
                YIELD_ERROR(BinOp);
            }

            BinOp->Ty = Context.BoolType;
        } else {
            if (!LTy->IsInt()) {
                OPERATOR_CANNOT_APPLY(LTy);
            }
            if (!RTy->IsInt()) {
                OPERATOR_CANNOT_APPLY(RTy);
            }

            Type* ToType = DetermineTypeFromIntTypes(Context, BinOp->LHS, BinOp->RHS);
            if (ToType == Context.ErrorType) {
                TYPE_MISMATCH();
            }

            CreateCast(BinOp->LHS, ToType);
            CreateCast(BinOp->RHS, ToType);
            BinOp->Ty = ToType;
        }
        break;
    }
    case TokenKind::EQ_EQ: case TokenKind::EXL_EQ:
    case '<': case '>':
    case TokenKind::LT_EQ: case TokenKind::GT_EQ: {
        if (LTy->IsPointer()) {

            if (!RTy->IsPointer()) {
                if (RTy->GetKind() == TypeKind::StructRef &&
                    (BinOp->Op == TokenKind::EQ_EQ || BinOp->Op == TokenKind::EXL_EQ)
                    ) {
                    Type* ElmType = LTy->GetPointerElementType(Context);
                    if (ElmType->GetKind() == TypeKind::Interface) {
                        // Checking if an interface is of a specific struct type is valid.
                        // Still need that the struct actually implements the interface though.
                        StructDecl* Struct = static_cast<IdentRef*>(BinOp->RHS)->Struct;
                        if (!Struct->ImplementsInterface(ElmType->AsStructType()->GetInterface())) {
                            Error(BinOp, "Cannot compare the interface to struct '%s' because it does not implement it",
                                Struct->Name);
                            YIELD_ERROR(BinOp);
                        }
                        BinOp->IsFoldable = false; // Requires branching to check for null.
                    }
                } else {
                    Error(BinOp->RHS, "Expected to be a pointer");
                }
            }

            if (RTy->GetKind() == TypeKind::Null) {
                BinOp->ResultType = BinOp->LHS->Ty;
                CreateCast(BinOp->RHS, BinOp->LHS->Ty);
            } else if (LTy->GetKind() == TypeKind::Null) {
                CreateCast(BinOp->LHS, BinOp->RHS->Ty);
                BinOp->ResultType = BinOp->RHS->Ty;
            } else {
                BinOp->ResultType = BinOp->RHS->Ty;
            }

            BinOp->Ty = Context.BoolType;
        } else if (RTy->IsPointer()) {

            if (!LTy->IsPointer()) {
                if (LTy->GetKind() == TypeKind::StructRef &&
                    (BinOp->Op == TokenKind::EQ_EQ || BinOp->Op == TokenKind::EXL_EQ)) {
                    Type* ElmType = LTy->GetPointerElementType(Context);
                    if (ElmType->GetKind() == TypeKind::Interface) {
                        // Checking if an interface is of a specific struct type is valid.
                        // Still need that the struct actually implements the interface though.
                        StructDecl* Struct = static_cast<IdentRef*>(BinOp->LHS)->Struct;
                        if (!Struct->ImplementsInterface(ElmType->AsStructType()->GetInterface())) {
                            Error(BinOp, "Cannot compare the interface to struct '%s' because it does not implement it",
                                Struct->Name);
                            YIELD_ERROR(BinOp);
                        }
                    }
                } else {
                    Error(BinOp->LHS, "Expected to be a pointer");
                }
            }

            if (RTy->GetKind() == TypeKind::Null) {
                CreateCast(BinOp->RHS, BinOp->LHS->Ty);
                BinOp->ResultType = BinOp->LHS->Ty;
            } else if (LTy->GetKind() == TypeKind::Null) {
                CreateCast(BinOp->LHS, BinOp->RHS->Ty);
                BinOp->ResultType = BinOp->RHS->Ty;
            } else {
                BinOp->ResultType = BinOp->RHS->Ty;
            }

            BinOp->Ty = Context.BoolType;
        } else if (LTy == Context.BoolType && RTy == Context.BoolType) {
            BinOp->Ty = Context.BoolType;
        } else {
            if (!LTy->IsNumber()) {
                OPERATOR_CANNOT_APPLY(LTy);
            }
            if (!RTy->IsNumber()) {
                OPERATOR_CANNOT_APPLY(RTy);
            }

            Type* ToType = DetermineTypeFromNumberTypes(Context, BinOp->LHS, BinOp->RHS);
            if (ToType == Context.ErrorType) {
                TYPE_MISMATCH();
            }

            CreateCast(BinOp->LHS, ToType);
            CreateCast(BinOp->RHS, ToType);
            BinOp->Ty = Context.BoolType;
            BinOp->ResultType = ToType;
        }
        break;
    }
    case TokenKind::AMP_AMP: case TokenKind::BAR_BAR: {
        if (!IsComparable(LTy)) {
            OPERATOR_CANNOT_APPLY(LTy);
        }
        if (!IsComparable(RTy)) {
            OPERATOR_CANNOT_APPLY(RTy);
        }

        BinOp->Ty = Context.BoolType;
        break;
    }

    default:
        assert(!"Failed to implement binary operator check");
        break;
    }

#undef OPERATOR_CANNOT_APPLY
#undef TYPE_MISMATCH
}

void arco::SemAnalyzer::CheckUnaryOp(UnaryOp* UniOp) {
    CheckNode(UniOp->Value);
    YIELD_ERROR_WHEN(UniOp, UniOp->Value);

    UniOp->IsFoldable = UniOp->Value->IsFoldable;
    Type* ValTy = UniOp->Value->Ty;

#define OPERATOR_CANNOT_APPLY(T)                        \
Error(UniOp, "Operator '%s' cannot apply to type '%s'", \
    Token::TokenKindToString(UniOp->Op, Context), T);   \
YIELD_ERROR(UniOp);

    switch (UniOp->Op) {
    case TokenKind::PLUS_PLUS:   case TokenKind::POST_PLUS_PLUS:
    case TokenKind::MINUS_MINUS: case TokenKind::POST_MINUS_MINUS: {
        if (!(ValTy->IsInt() || ValTy->IsPointer())) {
            OPERATOR_CANNOT_APPLY(ValTy);
        }

        CheckModifibility(UniOp->Value);

        UniOp->HasConstAddress = UniOp->Value->HasConstAddress;
        UniOp->Ty = ValTy;
        break;
    }
    case '&': {
        if (ValTy == Context.FuncRefType) {
            // Retrieving the address of a function!
            IdentRef* IRef = static_cast<IdentRef*>(UniOp->Value);

            FuncDecl* Func = (*IRef->Funcs)[0];
            if (Func->ParsingError) {
                YIELD_ERROR(UniOp);
            }

            // TODO: If we do allow taking addresses of member function then we should
            // also prevent taking addresses of virtual functions because that would
            // still cause bugs.
            if (Func->Struct) {
                Error(UniOp, "Not supporting taking address of member functions yet!");
            }

            // TODO: We should allow taking addresses of functions which raise errors
            if (!Func->RaisedErrors.empty()) {
                Error(UniOp, "Not supporting taking address of function which raises errors yet!");
            }

            SemAnalyzer A(Context, Func);
            A.CheckFuncParams(Func);

            // TODO: eventually take into account calling convention.
            llvm::SmallVector<TypeInfo> ParamTypes;
            ParamTypes.reserve(Func->Params.size());
            for (VarDecl* Param : Func->Params) {
                ParamTypes.push_back(TypeInfo{
                    Param->Ty,
                    Param->HasConstAddress
                    });
            }

            // TODO: Deal with generic bindings.
            RequestGenNonGenericFunc(Context, Func);

            UniOp->HasConstAddress = false;
            UniOp->Ty = FunctionType::Create(TypeInfo{ Func->RetTy, Func->ReturnsConstAddress },
                                             std::move(ParamTypes), Context);
        } else {
            if (!IsLValue(UniOp->Value)) {
                Error(UniOp, "Operator '%s' requires the value to be modifiable",
                    Token::TokenKindToString(UniOp->Op, Context));
            }
            if (UniOp->Value->HasConstAddress) {
                if (!ValTy->IsPointer() && ValTy->GetKind() != TypeKind::Array) {
                    Error(UniOp, "Cannot get address of value because it is folded and has no address");
                }
            }

            UniOp->HasConstAddress = UniOp->Value->HasConstAddress;
            UniOp->Ty = PointerType::Create(ValTy, Context);
        }
        break;
    }
    case '*': {
        if (!ValTy->IsPointer()) {
            OPERATOR_CANNOT_APPLY(ValTy);
        }
        if (ValTy->Equals(Context.VoidPtrType)) {
            Error(UniOp, "Cannot dereference void*");
        }
        if (ValTy->Equals(Context.NullType)) {
            Error(UniOp, "Cannot dereference null");
            YIELD_ERROR(UniOp);
        }

        UniOp->HasConstAddress = UniOp->Value->HasConstAddress;
        UniOp->Ty = ValTy->GetPointerElementType(Context);
        break;
    }
    case '-': case '+': case '~': {
        if (!ValTy->IsNumber()) {
            OPERATOR_CANNOT_APPLY(ValTy);
        }

        // TODO: Handle casting for unsigned?

        UniOp->Ty = ValTy;
        break;
    }
    case '!': {
        if (!IsComparable(ValTy)) {
            OPERATOR_CANNOT_APPLY(ValTy);
        }

        UniOp->Ty = Context.BoolType;
        break;
    }
    case TokenKind::TLD_DOT: {
        CheckModifibility(UniOp->Value);

        UniOp->HasConstAddress = true;
        UniOp->Ty = Context.VoidType;

        break;
    }
    default:
        assert(!"Unhandled unary check");
        break;
    }
}

template<bool SpellChecking>
void arco::SemAnalyzer::CheckIdentRef(IdentRef* IRef,
                                      bool ExpectsFuncCall,
                                      Namespace* NamespaceToLookup,
                                      StructType* StructToLookupTy,
                                      bool ExpectsGenericConstraint) {

    bool LocalNamespace = !NamespaceToLookup;

    using SpellCheckList = std::conditional_t<SpellChecking,
                                                llvm::SmallVector<std::string, 64>,
                                                void*>;
    SpellCheckList CheckListFuncs, CheckListOther;

    auto SearchForFuncs = [=, &CheckListFuncs]() {
        if (StructToLookupTy) {
            StructDecl* StructToLookup = StructToLookupTy->GetStruct();
            if constexpr (!SpellChecking) {
                auto Itr = StructToLookup->Funcs.find(IRef->Ident);
                if (Itr != StructToLookup->Funcs.end()) {
                    IRef->Funcs = &Itr->second;
                    IRef->RefKind = IdentRef::RK::Funcs;
                }
            } else {
                for (auto& [Name, List] : StructToLookup->Funcs)
                    for (FuncDecl* Func : List)
                        CheckListFuncs.push_back(Func->Name.Text.str());
            }
        } else {

            // Check local namespace first since it has higher priority than the default namespace.
            if (LocalNamespace && FScope->UniqueNSpace) {
                // File marked with a namespace need to search the namespace the file belongs to as well.
                if constexpr (!SpellChecking) {
                    auto Itr = FScope->UniqueNSpace->Funcs.find(IRef->Ident);
                    if (Itr != FScope->UniqueNSpace->Funcs.end()) {
                        IRef->Funcs = &Itr->second;
                        IRef->RefKind = IdentRef::RK::Funcs;
                        return;
                    }
                } else {
                    for (auto& [Name, List] : FScope->UniqueNSpace->Funcs)
                        for (FuncDecl* Func : List)
                            CheckListFuncs.push_back(Func->Name.Text.str());
                }
            }

            if (!LocalNamespace) {
                if constexpr (!SpellChecking) {
                    auto Itr = NamespaceToLookup->Funcs.find(IRef->Ident);
                    if (Itr != NamespaceToLookup->Funcs.end()) {
                        IRef->Funcs = &Itr->second;
                        IRef->RefKind = IdentRef::RK::Funcs;
                        return;
                    }
                } else {
                    for (auto& [Name, List] : NamespaceToLookup->Funcs)
                        for (FuncDecl* Func : List)
                            CheckListFuncs.push_back(Func->Name.Text.str());
                }
            }

            // Relative member functions.
            if (CStruct && LocalNamespace) {
                if constexpr (!SpellChecking) {
                    auto Itr = CStruct->Funcs.find(IRef->Ident);
                    if (Itr != CStruct->Funcs.end()) {
                        IRef->Funcs = &Itr->second;
                        IRef->RefKind = IdentRef::RK::Funcs;
                        return;
                    }
                } else {
                    for (auto& [Name, List] : CStruct->Funcs)
                        for (FuncDecl* Func : List)
                            CheckListFuncs.push_back(Func->Name.Text.str());
                }
            }

            if (LocalNamespace) {
                if constexpr (!SpellChecking) {
                    auto Itr = Mod->DefaultNamespace->Funcs.find(IRef->Ident);
                    if (Itr != Mod->DefaultNamespace->Funcs.end()) {
                        IRef->Funcs = &Itr->second;
                        IRef->RefKind = IdentRef::RK::Funcs;
                        return;
                    }
                } else {
                    for (auto& [Name, List] : Mod->DefaultNamespace->Funcs)
                        for (FuncDecl* Func : List)
                            CheckListFuncs.push_back(Func->Name.Text.str());
                }
            }

            // Searching for functions in static imports.
            if (LocalNamespace) {
                if constexpr (!SpellChecking) {
                    for (auto& Import : FScope->StaticImports) {
                        auto Itr = Import.NSpace->Funcs.find(IRef->Ident);
                        if (Itr != Import.NSpace->Funcs.end()) {
                            IRef->Funcs = &Itr->second;
                            IRef->RefKind = IdentRef::RK::Funcs;
                            return;
                        }
                    }
                } else {
                    for (auto& Import : FScope->StaticImports) {
                        for (auto& [Name, List] : Import.NSpace->Funcs)
                            for (FuncDecl* Func : List)
                                CheckListFuncs.push_back(Func->Name.Text.str());
                    }
                }
            }
        }
    };

    auto SearchNamespace = [=](Namespace* NSpace) {
        auto Itr = NSpace->Decls.find(IRef->Ident);
        if (Itr != NSpace->Decls.end()) {
            Decl* Dec = Itr->second;
            if (Dec->Is(AstKind::VAR_DECL)) {
                IRef->Var     = static_cast<VarDecl*>(Dec);
                IRef->RefKind = IdentRef::RK::Var;
                return true;
            } else if (Dec->Is(AstKind::STRUCT_DECL)) {
                IRef->Struct  = static_cast<StructDecl*>(Dec);
                IRef->RefKind = IdentRef::RK::Struct;
                return true;
            } else if (Dec->Is(AstKind::ENUM_DECL)) {
                IRef->Enum    = static_cast<EnumDecl*>(Dec);
                IRef->RefKind = IdentRef::RK::Enum;
                return true;
            } else {
                return false;
            }
        }
        return false;
    };

    auto SearchForOther = [=, &CheckListOther]() {
        if (StructToLookupTy) {
            VarDecl* Field = StructToLookupTy->GetStruct()->FindField(IRef->Ident);
            if (Field) {
                IRef->Var     = Field;
                IRef->RefKind = IdentRef::RK::Var;
            }
        } else {

            if (LocalNamespace && FScope->UniqueNSpace) {
                // File marked with a namespace need to search the namespace the file belongs to as well.
                if constexpr (!SpellChecking) {
                    if (SearchNamespace(FScope->UniqueNSpace)) {
                        return;
                    }
                } else {
                    for (auto& [Name, Dec] : FScope->UniqueNSpace->Decls)
                        CheckListOther.push_back(Dec->Name.Text.str());
                }
            }

            if (LocalNamespace) {
                if constexpr (!SpellChecking) {
                    if (SearchNamespace(Mod->DefaultNamespace)) {
                        return;
                    }
                } else {
                    for (auto& [Name, Dec] : Mod->DefaultNamespace->Decls)
                        CheckListOther.push_back(Dec->Name.Text.str());
                }
            } else {
                if constexpr (!SpellChecking) {
                    if (SearchNamespace(NamespaceToLookup)) {
                        return;
                    }
                } else {
                    for (auto& [Name, Dec] : NamespaceToLookup->Decls)
                        CheckListOther.push_back(Dec->Name.Text.str());
                }
            }

            // Search for private declarations.
            if (LocalNamespace) {
                if constexpr (!SpellChecking) {
                    if (Decl* Dec = FScope->FindDecl(IRef->Ident)) {
                        if (Dec->Is(AstKind::VAR_DECL)) {
                            IRef->Var = static_cast<VarDecl*>(Dec);
                            IRef->RefKind = IdentRef::RK::Var;
                            return;
                        } else if (Dec->Is(AstKind::STRUCT_DECL)) {
                            IRef->Struct = static_cast<StructDecl*>(Dec);
                            IRef->RefKind = IdentRef::RK::Struct;
                            return;
                        } else if (Dec->Is(AstKind::ENUM_DECL)) {
                            IRef->Enum = static_cast<EnumDecl*>(Dec);
                            IRef->RefKind = IdentRef::RK::Enum;
                            return;
                        }
                    }
                } else {
                    for (Decl* Dec : FScope->PrivateDecls)
                        CheckListOther.push_back(Dec->Name.Text.str());
                }
            }

            // Searching for global variables in static imports.
            if (LocalNamespace) {
                if constexpr (!SpellChecking) {
                    for (auto& Import : FScope->StaticImports) {
                        if (SearchNamespace(Import.NSpace)) {
                            return;
                        }
                    }
                } else {
                    for (auto& Import : FScope->StaticImports) {
                        for (auto& [Name, Dec] : Import.NSpace->Decls)
                            CheckListOther.push_back(Dec->Name.Text.str());
                    }
                }
            }

            if (LocalNamespace) {
                auto Itr = FScope->Imports.find(IRef->Ident);
                if (Itr != FScope->Imports.end()) {
                    if (Itr->second.NSpace) {
                        IRef->NSpace  = Itr->second.NSpace;
                        IRef->RefKind = IdentRef::RK::Import;
                    } else if (Itr->second.Decl) {
                        if (Itr->second.Decl->Is(AstKind::STRUCT_DECL)) {
                            IRef->Struct  = static_cast<StructDecl*>(Itr->second.Decl);
                            IRef->RefKind = IdentRef::RK::Struct;
                        } else {
                            IRef->Enum    = static_cast<EnumDecl*>(Itr->second.Decl);
                            IRef->RefKind = IdentRef::RK::Enum;
                        }
                    }
                }
            }
        }
    };

    // If it expects a function then we search the
    // function first otherwise we search for a variable
    // first.
    if (!IRef->IsFound() && ExpectsFuncCall) {
        SearchForFuncs();
        if (!IRef->IsFound())
            SearchForOther();
    } else if (!IRef->IsFound()) {
        SearchForOther();
        if (!IRef->IsFound())
            SearchForFuncs();
    }

    if constexpr (!SpellChecking) {
        switch (IRef->RefKind) {
        case IdentRef::RK::Var: {
            VarDecl* VarRef = IRef->Var;
            if (VarRef->IsGeneric()) {
                if (ExpectsGenericConstraint) break;

                if (IRef->GenBindings.empty()) {
                    Error(IRef, "Missing type bindings for generic variable");
                    IRef->Ty = Context.ErrorType;
                    break;
                } else {
                    if (IRef->GenBindings.size() != VarRef->GenData->GenTys.size()) {
                        Error(IRef,
                            "Incorrect number of type bindings for generic variable. Expected %s. Got %s",
                            VarRef->GenData->GenTys.size(), IRef->GenBindings.size());
                        IRef->Ty = Context.ErrorType;
                        break;
                    }
                }

                IRef->HasConstAddress = true;
                // Is foldable is always true for generic variables.

                if (VarRef->IsGlobal || VarRef->IsField()) {
                    // TODO: Deal with parent bindings.
                    GenericBinding* Binding = GetExistingBinding(VarRef, IRef->GenBindings, nullptr);
                    if (!Binding) {
                        Binding = CreateNewBinding(VarRef, IRef->GenBindings, nullptr);
                        BindTypes(VarRef, Binding);
                        EnsureChecked(IRef->Loc, VarRef);
                        IRef->Ty = VarRef->GenData->CurBinding->QualifiedType;
                        UnbindTypes(VarRef);
                    } else {
                        IRef->Ty = Binding->QualifiedType;
                    }
                    IRef->FoundBinding = Binding;
                }

                break;
            } else if (!IRef->GenBindings.empty()) {
                Error(IRef, "The variable '%s' does not have generics", VarRef->Name);
                IRef->Ty = Context.ErrorType;
                break;
            }

            // TODO: If it is a field wouldn't it already have been checked?
            if (VarRef->IsGlobal || VarRef->IsField()) {
                EnsureChecked(IRef->Loc, VarRef);
            }

            if (VarRef->IsField() && VarRef->Ty->ContainsGenerics) {
                // Want to use the qualified type because
                // binding is only temporary.
                auto& StructInfo = StructToLookupTy ? StructToLookupTy->FoundBinding->StructInfo
                                                    : CStruct->GetCurBinding()->StructInfo;
                IRef->Ty              = StructInfo->QualifiedFieldTypes[VarRef->GenQualIdx];
                IRef->HasConstAddress = StructInfo->FieldsConstAddressInfo[VarRef->GenQualIdx];
            } else {
                IRef->Ty = VarRef->Ty;
                IRef->HasConstAddress = VarRef->HasConstAddress;
            }

            if (!VarRef->IsComptime()) {
                IRef->IsFoldable = false;
            }
            break;
        }
        case IdentRef::RK::Import: {
            IRef->Ty = Context.ImportType;
            IRef->IsFoldable = false;
            break;
        }
        case IdentRef::RK::Funcs: {
            IRef->Ty = Context.FuncRefType;
            IRef->IsFoldable = false;
            break;
        }
        case IdentRef::RK::Struct: {
            // TODO: Deal with generics!
            if (!IRef->Struct->HasBeenChecked) {
                SemAnalyzer A(Context, IRef->Struct);
                A.CheckStructDecl(IRef->Struct);
            }
            IRef->Ty = Context.StructRefType;
            IRef->IsFoldable = false;
            break;
        }
        case IdentRef::RK::Enum: {
            if (!IRef->Struct->HasBeenChecked) {
                SemAnalyzer A(Context, IRef->Enum);
                A.CheckEnumDecl(IRef->Enum);
            }
            IRef->Ty = Context.EnumRefType;
            IRef->IsFoldable = false;
            break;
        }
        case IdentRef::RK::NotFound: {
            if (ExpectsFuncCall) {
                Log.BeginError(IRef->Loc, "Could not find a function for identifier '%s'", IRef->Ident);
                CheckIdentRef<true>(IRef,
                                    ExpectsFuncCall,
                                    NamespaceToLookup,
                                    StructToLookupTy,
                                    ExpectsGenericConstraint);
                Log.EndError();
            } else {
                Log.BeginError(IRef->Loc, "Could not find symbol for %s '%s'",
                    StructToLookupTy ? "field" : "identifier",
                    IRef->Ident);
                CheckIdentRef<true>(IRef,
                                    ExpectsFuncCall,
                                    NamespaceToLookup,
                                    StructToLookupTy,
                                    ExpectsGenericConstraint);
                Log.EndError();
            }
            IRef->Ty = Context.ErrorType;
            break;
        }
        default:
            assert(!"Unimplemented ident reference end case");
            break;
        }
    } else {
        ErrorSpellChecker SpellChecker;
        if (ExpectsFuncCall) {
            SpellChecker.AddSearches(CheckListFuncs);
            SpellChecker.Search(Log, IRef->Ident);
        } else {
            SpellChecker.AddSearches(CheckListOther);
            SpellChecker.Search(Log, IRef->Ident);
        }
    }
}

void arco::SemAnalyzer::CheckFieldAccessor(FieldAccessor* FieldAcc, bool ExpectsFuncCall) {

    Expr* Site = FieldAcc->Site;

    if (Site->Is(AstKind::IDENT_REF)) {
        CheckIdentRef<false>(static_cast<IdentRef*>(Site), false, nullptr);
    } else {
        CheckNode(Site);
    }
    YIELD_ERROR_WHEN(FieldAcc, Site);

    FieldAcc->HasConstAddress = Site->HasConstAddress;
    FieldAcc->IsFoldable = Site->IsFoldable;

    // Checking for .length operator
    if (FieldAcc->Ident == Context.LengthIdentifier) {
        if (Site->Ty->GetKind() == TypeKind::Array) {
            FieldAcc->IsArrayLength = true;
            FieldAcc->Ty = Context.IntType;
            return;
        } else if (Site->Ty->GetKind() == TypeKind::Slice) {
            FieldAcc->IsSliceLength = true;
            FieldAcc->Ty = Context.IntType;
            return;
        }
    } else if (FieldAcc->Ident == Context.BufferIdentifier) {
        if (Site->Ty->GetKind() == TypeKind::Slice) {
            FieldAcc->IsSliceBufferAcc = true;
            FieldAcc->Ty = PointerType::Create(Site->Ty->AsSliceTy()->GetElementType(), Context);
            return;
        }
    }

    if (Site->Ty == Context.EnumRefType) {
        IdentRef* IRef = static_cast<IdentRef*>(Site);
        FieldAcc->Ty = StructType::Create(IRef->Enum, Context);
        FieldAcc->EnumValue = IRef->Enum->FindValue(FieldAcc->Ident);
        if (IRef->Enum->IndexingInOrder) {
            FieldAcc->IsFoldable = true;
        }

        // Ex. Day.MONDAY
        if (!FieldAcc->EnumValue) {
            Error(FieldAcc, "enum '%s' does not contain value '%s'",
                IRef->Struct->Name, FieldAcc->Ident);
        }

        if (IRef->Enum->ValuesType && IRef->Enum->ValuesType->Equals(Context.CStrType)) {
            FieldAcc->HasConstAddress = true;
        } else {
            FieldAcc->HasConstAddress = false;
        }

        return;
    }

    if (Site->Ty == Context.ImportType) {
        IdentRef* IRef = static_cast<IdentRef*>(Site);
        CheckIdentRef<false>(FieldAcc, ExpectsFuncCall, IRef->NSpace);
        return;
    }

    bool InterfacePtrRef = Site->Ty->GetKind() == TypeKind::Pointer &&
                           Site->Ty->AsPointerTy()->GetElementType()->GetKind() == TypeKind::Interface;

    if (!(Site->Ty->GetKind() == TypeKind::Struct ||
          (Site->Ty->GetKind() == TypeKind::Pointer &&
          Site->Ty->AsPointerTy()->GetElementType()->GetKind() == TypeKind::Struct) ||
        InterfacePtrRef
         )) {
        Error(FieldAcc, "Cannot access field of type '%s'", Site->Ty);
        YIELD_ERROR(FieldAcc);
    }

    if (InterfacePtrRef) {
        InterfaceDecl* Interface = Site->Ty->AsPointerTy()->GetElementType()->AsStructType()->GetInterface();
        auto Itr = std::find_if(Interface->Funcs.begin(), Interface->Funcs.end(),
            [FieldAcc](FuncDecl* Func) {
                return Func->Name == FieldAcc->Ident; });

        if (Itr != Interface->Funcs.end()) {
            FieldAcc->Ty = Context.FuncRefType;
            FieldAcc->RefKind = IdentRef::RK::Funcs;
            FieldAcc->Funcs = &Interface->Funcs;
            FieldAcc->IsFoldable = false;
            return;
        } else {
            if (ExpectsFuncCall) {
                Error(FieldAcc, "Could not find a function for identifier '%s'", FieldAcc->Ident);
            } else {
                Error(FieldAcc, "Could not find symbol for identifier '%s'", FieldAcc->Ident);
            }
            YIELD_ERROR(FieldAcc);
        }
    }

    StructType* StructTy;
    if (Site->Ty->GetKind() == TypeKind::Struct) {
        StructTy = Site->Ty->AsStructType();
    } else {
        StructTy = Site->Ty->AsPointerTy()->GetElementType()->AsStructType();
    }

    if (StructTy->GetStruct()->IsBeingChecked) {
        // Disallows nonsense like the following:
        //
        // A struct {
        // 	b B* = new B;
        // 	ca int = b.cb;
        // }
        //
        // B struct {
        // 	a A* = new A;
        // 	cb int = a.ca;
        // }
        Error(FieldAcc, "Cannot access field of struct '%s' because the type is incomplete",
            StructTy);
        YIELD_ERROR(FieldAcc);
    } else {
        StructDecl* Struct = StructTy->GetStruct();
        CheckIdentRef<false>(FieldAcc, ExpectsFuncCall, nullptr, StructTy);
        if (FieldAcc->Ty != Context.ErrorType) {
            if (FieldAcc->RefKind == IdentRef::RK::Var && (FieldAcc->Var->Mods & ModKinds::PRIVATE)) {
                if (FieldAcc->Var->FScope != FScope) {
                    Error(FieldAcc, "Field not visible, it is private");
                }
            }
        }
    }
}

void arco::SemAnalyzer::CheckThisRef(ThisRef* This) {
    if (!CStruct) {
        Error(This, "Cannot use 'this' outside a struct scope");
        YIELD_ERROR(This);
    }
    CheckStructDecl(CStruct);
    This->IsFoldable = false;

    This->Ty = PointerType::Create(CStruct->StructTy, Context);
}

void arco::SemAnalyzer::CheckFuncCall(FuncCall* Call) {

    bool ArgHasError = false;
    for (auto& Arg : Call->Args) {
        CheckNode(Arg);
        if (Arg->Ty == Context.ErrorType)
            ArgHasError = true;
    }
    for (auto& Arg : Call->NamedArgs) {
        CheckNode(Arg.AssignValue);
        if (Arg.AssignValue->Ty == Context.ErrorType)
            ArgHasError = true;
    }
    if (ArgHasError) {
        YIELD_ERROR(Call);
    }

    switch (Call->Site->Kind) {
    case AstKind::IDENT_REF:
        CheckIdentRef<false>(static_cast<IdentRef*>(Call->Site), true, nullptr);
        break;
    case AstKind::FIELD_ACCESSOR:
        CheckFieldAccessor(static_cast<FieldAccessor*>(Call->Site), true);
        break;
    default:
        CheckNode(Call->Site);
        break;
    }
    YIELD_ERROR_WHEN(Call, Call->Site);

    Type* SiteTy = Call->Site->Ty;
    if (SiteTy->GetKind() == TypeKind::Function) {
        // Calling a variable!
        FunctionType* FuncTy = SiteTy->AsFunctionType();

        if (!Call->NamedArgs.empty()) {
            Error(Call, "Cannot call a variable with named arguments");
            YIELD_ERROR(Call);
        }

        if (Call->Args.size() != FuncTy->ParamTypes.size()) {
            DisplayErrorForSingleFuncForFuncCall("function",
                                                 Call->Loc,
                                                 FuncTy->ParamTypes,
                                                 Call->Args,
                                                 Call->NamedArgs,
                                                 0 /* Num default args */);
            YIELD_ERROR(Call);
        }

        for (ulen i = 0; i < Call->Args.size(); i++) {
            Expr*    Arg         = Call->Args[i];
            TypeInfo ParamTyInfo = FuncTy->ParamTypes[i];

            if (!IsAssignableTo(ParamTyInfo.Ty, Arg) ||
                ViolatesConstAssignment(ParamTyInfo.Ty, ParamTyInfo.ConstMemory, Arg)) {
                DisplayErrorForSingleFuncForFuncCall("function",
                                                     Call->Loc,
                                                     FuncTy->ParamTypes,
                                                     Call->Args,
                                                     Call->NamedArgs,
                                                     0 /* Num default args */);
                YIELD_ERROR(Call);
            }
        }

        // Creating casts for the arguments.
        for (ulen i = 0; i < Call->Args.size(); i++) {
            Expr*    Arg         = Call->Args[i];
            TypeInfo ParamTyInfo = FuncTy->ParamTypes[i];
            CreateCast(Arg, ParamTyInfo.Ty);
        }

        Call->Ty = FuncTy->RetTyInfo.Ty;
        Call->IsFoldable = false;
        Call->HasConstAddress = FuncTy->RetTyInfo.ConstMemory;

        return;
    } else if (SiteTy->GetKind() != TypeKind::FuncRef) {
        // Invalid call.
        DisplayErrorForBadCallSiteType(Call);
        YIELD_ERROR(Call);
    }

    IdentRef* IRef = static_cast<IdentRef*>(Call->Site);
    FuncsList* Canidates = IRef->Funcs;

    GenericBinding* GenericStructBinding = nullptr;
    if (Call->Site->Is(AstKind::FIELD_ACCESSOR)) {
        FieldAccessor* FieldAcc = static_cast<FieldAccessor*>(Call->Site);
        if (FieldAcc->Site->Ty->GetKind() == TypeKind::Struct) {
            StructType* StructTy = FieldAcc->Site->Ty->AsStructType();
            if (!StructTy->GetBindTypes().empty()) {
                GenericStructBinding = StructTy->FoundBinding;
            }
        }
    } else if (CStruct && CStruct->IsGeneric()) {
        // Checking to see if the call is to a member function of a generic
        // struct and if it is then have to use the current bindings of the
        // current generic struct.
        if (!Canidates->empty()) {
            if ((*Canidates)[0]->Struct == CStruct) {
                GenericStructBinding = CStruct->GetCurBinding();
            }
        }
    }

    GenericBinding* Binding;
    Type* RetTy;
    bool VarArgsPassAlong = false;
    Call->CalledFunc = CheckCallToCanidates(IRef->Ident,
                                            Call->Loc,
                                            Canidates,
                                            Call->Args,
                                            Call->NamedArgs,
                                            VarArgsPassAlong,
                                            Binding,
                                            RetTy,
                                            GenericStructBinding);

    if (!Call->CalledFunc) {
        YIELD_ERROR(Call);
    }
    if (Call->CalledFunc->Mods & ModKinds::PRIVATE) {
        if (Call->CalledFunc->FScope != FScope) {
            Error(Call, "%s not visible, it is private",
                Call->CalledFunc->IsConstructor ? "Constructor" : "Function");
        }
    }

    Call->VarArgsPassAlong = VarArgsPassAlong;
    Call->FoundBinding = Binding;

    Call->IsFoldable = false;
    Call->Ty = RetTy;
    Call->HasConstAddress = Call->CalledFunc->ReturnsConstAddress;

}

arco::FuncDecl* arco::SemAnalyzer::CheckCallToCanidates(Identifier FuncName,
                                                        SourceLoc ErrorLoc,
                                                        FuncsList* Canidates,
                                                        llvm::SmallVector<Expr*>&      Args,
                                                        llvm::SmallVector<NamedValue>& NamedArgs,
                                                        bool& VarArgsPassAlong,
                                                        GenericBinding*& Binding,
                                                        Type*& RetTy,
                                                        GenericBinding* StructGenericBinding) {

    // TODO:  Performance this may be more efficient to just do total stack allocation
    // by limited the number of overloaded functions/parameter allowed.

    llvm::SmallVector<BindableList, 8> AllBindableTypes;
    llvm::SmallVector<BindableList, 8> AllQualTypes;
    for (FuncDecl* Canidate : *Canidates) {
        if (Canidate->IsGeneric()) {
            // Initialize the partial binding for the generic function.
            AllBindableTypes.emplace_back(
                BindableList(Canidate->GenData->GenTys.size(), nullptr));
            AllQualTypes.emplace_back(
                BindableList(Canidate->GenData->NumQualifications, nullptr));
        }
    }

    BindableList* StructBindableTypes = StructGenericBinding ? &StructGenericBinding->BindableTypes
                                                             : nullptr;
    FuncDecl* Selected = FindBestFuncCallCanidate(FuncName,
                                                  Canidates,
                                                  Args,
                                                  NamedArgs,
                                                  VarArgsPassAlong,
                                                  AllBindableTypes,
                                                  AllQualTypes,
                                                  StructBindableTypes);
    if (!Selected) {
        DisplayErrorForNoMatchingFuncCall(ErrorLoc,
                                          Canidates,
                                          Args,
                                          NamedArgs,
                                          StructBindableTypes);
        return nullptr;
    }

    // If the call has named arguments the named arguments might conflict with the non-named
    // arguments so checking for that now.
    if (!NamedArgs.empty()) {
        bool MoreErrors = false;
        // Need to lookup the paramters again because there might have been multiple overloaded functions.
        for (NamedValue& NamedArg : NamedArgs) {
            auto Itr = std::find_if(Selected->Params.begin(), Selected->Params.end(),
                [&NamedArg](VarDecl* Param) {
                    return Param->Name == NamedArg.Name;
                });
            assert(Itr != Selected->Params.end() && "The canidate should not have been selected");
            NamedArg.VarRef = *Itr;

            if (NamedArg.VarRef->ParamIdx < Args.size()) {
                Error(NamedArg.NameLoc,
                    "Named argument '%s' already taken by non named argument at index: %s",
                    NamedArg.Name,
                    NamedArg.VarRef->ParamIdx + 1);
                MoreErrors = true;
            }
        }
        if (Selected->NumDefaultArgs) {
            // Checking that for all parameters between the non-named arguments and the default
            // arguments that the parameter actually recieved a value.
            // It is possible that it did not in cases like:
            //
            // fn foo(a int, b := 33) { }
            //
            // fn main() {
            //     foo(b=41);
            // }
            ulen StartOfDefaults = Selected->Params.size() - Selected->NumDefaultArgs;
            for (ulen i = Args.size(); i < StartOfDefaults; i++) {
                VarDecl* Param = Selected->Params[i];
                auto Itr = std::find_if(NamedArgs.begin(), NamedArgs.end(),
                    [Param](const NamedValue& V) {
                        return V.VarRef->ParamIdx == Param->ParamIdx;
                    });
                if (Itr == NamedArgs.end()) {
                    // TODO: better error reporting.
                    Error(ErrorLoc, "Parameter '%s' recieved no argument", Param->Name);
                    MoreErrors = true;
                }
            }
        }
        if (MoreErrors) {
            return nullptr;
        }
    }


    // Creating casts for the arguments.

    // TODO: Figure out a better way to obtain the the generic function index.
    ulen GenericIdx = 0;
    if (Selected->IsGeneric()) {
        for (FuncDecl* Canidate : *Canidates) {
            if (Canidate == Selected) break;
            if (Canidate->IsGeneric()) {
                ++GenericIdx;
            }
        }
    }

    BindableList* BindableTypes = nullptr;
    BindableList* QualTypes     = nullptr;
    if (Selected->IsGeneric()) {
        BindableTypes = &AllBindableTypes[GenericIdx];
        QualTypes     = &AllQualTypes[GenericIdx];
        // Making sure all of the generic types have bindings.
        for (ulen i = 0; i < Selected->GenData->GenTys.size(); i++) {
            if ((*BindableTypes)[i] == nullptr) {
                GenericType* GenTy = Selected->GenData->GenTys[i];
                // TODO: Make debug message more explainable.
                // TODO: Part of the explaination needs to include if the non-bound
                //       type was part of the struct that the function belongs to or not
                Error(ErrorLoc,
                    "Generic type '%s' did not recieve a binding type",
                    GenTy->ToString(false)
                    );
                return nullptr;
            }
        }
    } else {
        RetTy = Selected->RetTy;
    }
    CreateCallCasts(Selected, Args, NamedArgs, VarArgsPassAlong, QualTypes);

    if (!CatchingErrors && !Selected->RaisedErrors.empty()) {
        CheckIfErrorsAreCaptured(ErrorLoc, Selected);
    }

    if (!Selected->Interface) {
        // We do not want to generate this if it is an interface function
        // because interface functions are just pointers to functions not
        // actually functions themselves.
        if (Selected->IsGeneric()) {

            Binding = GetExistingBinding(Selected, *BindableTypes, StructGenericBinding);
            if (!Binding) {
                // TODO: Stilling the qualification types here needs to take into
                // account that they may already have been set!

                Binding = CreateNewBinding(Selected, std::move(*BindableTypes), StructGenericBinding);
                Binding->FuncInfo = new GenericFuncInfo; // TODO: Use of new

                // Need to set the qualifications of the parameters/return to generate
                // the function correctly.
                BindTypes(Selected, Binding);
                for (VarDecl* Param : Selected->Params) {
                    if (Param->Ty->ContainsGenerics)
                        Param->Ty->SetQualType((*QualTypes)[Param->GenQualIdx]);
                }
                if (Selected->RetTy->ContainsGenerics) {
                    RetTy = QualifyType(Selected->RetTy, Binding);
                    Binding->FuncInfo->QualRetTy = RetTy;
                } else {
                    RetTy = Selected->RetTy;
                }

                if (Selected->RetTy->ContainsGenerics) {
                    Selected->RetTy->SetQualType(RetTy);
                }
                Selected->GenData->CurBinding = Binding;


                if (StructGenericBinding) {
                    Binding->StructInfo = StructGenericBinding->StructInfo;
                }

                if (CFunc && CFunc->IsGeneric()) {
                    Binding->OriginalFile = CFunc->GenData->CurBinding->OriginalFile;
                    Binding->OriginalLoc = CFunc->GenData->CurBinding->OriginalLoc;
                } else {
                    Binding->OriginalFile = FScope;
                    Binding->OriginalLoc = ErrorLoc;
                }

                if (!FoundCompileError && !Context.CheckingUnhcecked) {
                    IRGenerator IRGen(Context);
                    IRGen.GenFuncDecl(Selected);
                }
                UnbindTypes(Selected);
                Context.RequestGen(Selected, Binding);
            } else {
                RetTy = Binding->FuncInfo->QualRetTy;
            }
        } else {
            RetTy = Selected->RetTy;
            RequestGenNonGenericFunc(Context, Selected);
        }
    } else {
        RetTy = Selected->RetTy;
    }

    return Selected;
}

void arco::SemAnalyzer::CreateCallCasts(FuncDecl* Selected,
                                        llvm::SmallVector<Expr*>&      Args,
                                        llvm::SmallVector<NamedValue>& NamedArgs,
                                        bool VarArgsPassAlong,
                                        const BindableList* QualifiedTypes) {

    ulen NumGenerics = Selected->IsGeneric() ? Selected->GenData->GenTys.size() : 0;

    if (!Selected->IsVariadic) {
        for (ulen i = 0; i < Args.size(); i++) {
            Expr* Arg = Args[i];
            VarDecl* Param = Selected->Params[i];
            if (Param->Ty->ContainsGenerics) {
                CreateCast(Arg, (*QualifiedTypes)[Param->GenQualIdx]);
            } else {
                CreateCast(Arg, Param->Ty->UnboxGeneric());
            }
        }
    } else {
        ulen i = 0;
        for (; i < Selected->Params.size() - 1; i++) {
            Expr* Arg = Args[i];
            VarDecl* Param = Selected->Params[i];
            if (Param->Ty->ContainsGenerics) {
                CreateCast(Arg, (*QualifiedTypes)[Param->GenQualIdx]);
            } else {
                CreateCast(Arg, Param->Ty->UnboxGeneric());
            }
        }
        if (!VarArgsPassAlong) {
            VarDecl* LastParam = Selected->Params[i];
            Type* VarArgTy = LastParam->Ty->AsSliceTy()->GetElementType();
            for (; i < Args.size(); i++) {
                Expr* Arg = Args[i];
                if (VarArgTy->ContainsGenerics) {
                    CreateCast(Arg, (*QualifiedTypes)[LastParam->GenQualIdx]);
                } else {
                    CreateCast(Arg, VarArgTy->UnboxGeneric());
                }
            }
        }
    }

    for (NamedValue& NamedValue : NamedArgs) {
        VarDecl* Param = NamedValue.VarRef;
        if (Param->Ty->ContainsGenerics) {
            CreateCast(NamedValue.AssignValue, (*QualifiedTypes)[Param->GenQualIdx]);
        } else {
            CreateCast(NamedValue.AssignValue, Param->Ty->UnboxGeneric());
        }
    }
}

arco::FuncDecl* arco::SemAnalyzer::FindBestFuncCallCanidate(Identifier FuncName,
                                                            FuncsList* Canidates,
                                                            llvm::SmallVector<Expr*>&      Args,
                                                            llvm::SmallVector<NamedValue>& NamedArgs,
                                                            bool& SelectedVarArgsPassAlong,
                                                            llvm::SmallVector<BindableList, 8>& AllBindableTypes,
                                                            llvm::SmallVector<BindableList, 8>& AllQualTypes,
                                                            BindableList* StructBindableTypes) {
    if (!Canidates) return nullptr;

    FuncDecl* Selection = nullptr;

    bool CheckName = !FuncName.IsNull();

    ulen GenericFuncCount = 0;
    ulen LeastConflicts             = std::numeric_limits<ulen>::max(),
         LeastEnumImplicitConflicts = std::numeric_limits<ulen>::max(),
         LeastSignConflicts         = std::numeric_limits<ulen>::max(),
         HasGenerics = true,
         BestIsPrivate = false,
         BestIsHasAny = false;
    for (ulen i = 0; i < Canidates->size(); i++) {
        FuncDecl* Canidate = (*Canidates)[i];
        BindableList* BindableTypes = nullptr;
        BindableList* QualTypes = nullptr;
        if (Canidate->IsGeneric()) {
            BindableTypes = &AllBindableTypes[GenericFuncCount];
            QualTypes     = &AllQualTypes[GenericFuncCount];
            ++GenericFuncCount;
        }

        if (!Canidate->ParamTypesChecked) {
            SemAnalyzer Analyzer(Context, Canidate);
            Analyzer.CheckFuncParams(Canidate, true);
        }
        if (CheckName) {
            if (Canidate->Name != FuncName) {
                continue;
            }
        }

        ulen NumConflicts = 0, EnumImplicitConflicts = 0, NumSignConflicts = 0;
        bool VarArgsPassAlong = false;
        bool IsPrivate = false;
        bool HasAny = false;
        if (Canidate->Mods & ModKinds::PRIVATE) {
            if (Canidate->FScope == FScope) {
                IsPrivate = true;
            } else {
                // Is it private but we are not within the correct file so cannot access.
                continue;
            }
        }
        if (!CompareAsCanidate(Canidate,
                               Args,
                               NamedArgs,
                               NumConflicts,
                               EnumImplicitConflicts,
                               NumSignConflicts,
                               VarArgsPassAlong,
                               HasAny,
                               BindableTypes,
                               QualTypes,
                               StructBindableTypes)) {
            continue;
        }

#define SET_BEST                                    \
 Selection                 = Canidate;              \
LeastConflicts             = NumConflicts;          \
LeastEnumImplicitConflicts = EnumImplicitConflicts; \
LeastSignConflicts         = NumSignConflicts;      \
SelectedVarArgsPassAlong   = VarArgsPassAlong;      \
BestIsPrivate              = IsPrivate;             \
BestIsHasAny               = HasAny;                \
HasGenerics = Canidate->IsGeneric()

        if (!Canidate->IsGeneric() && HasGenerics) {
            // Always select non-generic functions over generic except.
            SET_BEST;
        } else if (NumConflicts < LeastConflicts) {
            SET_BEST;
        } else if (NumConflicts == LeastConflicts &&
                   NumSignConflicts < LeastSignConflicts) {
            SET_BEST;
        } else if (NumConflicts == LeastConflicts &&
                   NumSignConflicts == LeastSignConflicts && // TODO: do we care about num sign over enum in this case?
                   EnumImplicitConflicts < LeastEnumImplicitConflicts) {
            SET_BEST;
        } else if (!BestIsPrivate && IsPrivate) {
            SET_BEST;
        }
    }
    return Selection;
}

bool arco::SemAnalyzer::CompareAsCanidate(FuncDecl* Canidate,
                                          llvm::SmallVector<Expr*>&      Args,
                                          llvm::SmallVector<NamedValue>& NamedArgs,
                                          ulen& NumConflicts,
                                          ulen& EnumImplicitConflicts,
                                          ulen& NumSignConflicts,
                                          bool& CanidateVarArgPassAlong,
                                          bool& HasAny,
                                          BindableList* BindableTypes,
                                          BindableList* QualTypes,
                                          BindableList* StructBindableTypes) {
    // TODO: performance: all this awful variadic stuff can be optimized out by
    // using templating.

    if (Canidate->ParsingError) {
        return false;
    }
    ulen TotalArgs = Args.size() + NamedArgs.size();
    if (Canidate->NumDefaultArgs) {
        if (!(TotalArgs >= Canidate->Params.size() - Canidate->NumDefaultArgs &&
              TotalArgs <= Canidate->Params.size())) {
            return false;
        }
    } else {
        if (Canidate->IsVariadic) {
            if (TotalArgs < Canidate->Params.size() - 1) {
                return false;
            }
        } else {
            if (TotalArgs != Canidate->Params.size()) {
                return false;
            }
        }
    }

    for (ulen i = 0; i < Args.size(); i++) {
        Expr* Arg = Args[i];

        VarDecl* Param;
        Type*    ParamType;

        if (Canidate->IsVariadic) {

            if (i >= Canidate->Params.size() - 1) {
                Param     = Canidate->Params[Canidate->Params.size() - 1];
                ParamType = Param->Ty->AsSliceTy()->GetElementType();
            } else {
                Param     = Canidate->Params[i];
                ParamType = Param->Ty;
            }

            if (CFunc->IsVariadic && i+1 == Args.size() && Arg->Is(AstKind::IDENT_REF)) {
                // Check for special case where varargs is passed to
                // varargs.

                IdentRef* IRef = static_cast<IdentRef*>(Arg);
                if (IRef->RefKind == IdentRef::RK::Var) {
                    if (CFunc->Params.size() > 0) {
                        VarDecl* VarArgsParam = CFunc->Params[CFunc->Params.size() - 1];
                        if (IRef->Var == VarArgsParam) {
                            // Making sure the slice types match as well
                            CanidateVarArgPassAlong = Param->Ty->Equals(VarArgsParam->Ty);
                            return CanidateVarArgPassAlong;
                        }
                    }
                }
            }

        } else {
            Param     = Canidate->Params[i];
            ParamType = Param->Ty;
        }

        // This must be kept as GetUniqueId because we care about the actual
        // type not what it would be if it was bound.
        if (!Context.StandAlone) {
            if (Param->Ty->GetUniqueId() == Context.AnyType->GetUniqueId()) {
                HasAny = true;
            }
        }

        if (!CheckCallArg(Arg,
                          Param,
                          ParamType,
                          NumConflicts,
                          EnumImplicitConflicts,
                          NumSignConflicts,
                          BindableTypes,
                          QualTypes,
                          StructBindableTypes)) {
            return false;
        }
    }

    if (!NamedArgs.empty() && Canidate->IsVariadic) {
        return false;
    }

    for (NamedValue& NamedArg : NamedArgs) {
        auto Itr = std::find_if(Canidate->Params.begin(), Canidate->Params.end(),
            [&NamedArg](VarDecl* Param) {
                return Param->Name == NamedArg.Name;
            });
        if (Itr != Canidate->Params.end()) {

            VarDecl* Param = *Itr;

            // This must be kept as GetUniqueId because we care about the actual
            // type not what it would be if it was bound.
            if (!Context.StandAlone) {
                if (Param->Ty->GetUniqueId() == Context.AnyType->GetUniqueId()) {
                    HasAny = true;
                }
            }

            if (!CheckCallArg(NamedArg.AssignValue,
                              Param,
                              Param->Ty,
                              NumConflicts,
                              EnumImplicitConflicts,
                              NumSignConflicts,
                              BindableTypes,
                              QualTypes,
                              StructBindableTypes)) {
                return false;
            }
        } else {
            // Could not find the parameter by the given name.
            return false;
        }
    }

    if (Canidate->IsGeneric()) {
        // Checking to make sure all the generic constraints are valid.
        for (GenericType* GenTy : Canidate->GenData->GenTys) {
            if (!CheckGenericConstraint(Canidate, GenTy, BindableTypes)) {
                return false;
            }
        }
    }

    return true;
}

bool arco::SemAnalyzer::CheckCallArg(Expr* Arg,
                                     VarDecl* Param,
                                     Type* ParamType,
                                     ulen& NumConflicts,
                                     ulen& EnumImplicitConflicts,
                                     ulen& NumSignConflicts,
                                     BindableList* BindableTypes,
                                     BindableList* QualTypes,
                                     BindableList* StructBindableTypes) {

    Type* ComparibleParamType = ParamType;
    bool HasConstAddress = Param->HasConstAddress;
    if (ParamType->ContainsGenerics) {
        Type* ExistingQualType = nullptr;
        auto Res = CheckCallArgGeneric(Arg->Ty->UnboxGeneric(),
                                       Param->ImplicitPtr,
                                       ParamType,
                                       *BindableTypes,
                                       *QualTypes,
                                       *StructBindableTypes,
                                       ExistingQualType,
                                       Param->GenQualIdx,
                                       true);
        if (!Res) {
            return false;
        }
        Type* QualType = (*QualTypes)[Param->GenQualIdx];
        HasConstAddress = Param->ExplicitlyMarkedConst ||
                          QualType->GetKind() == TypeKind::CStr;

        ComparibleParamType = QualType;

        if (!ExistingQualType) {
            if (ViolatesConstAssignment(ComparibleParamType, HasConstAddress, Arg)) {
                return false;
            }

            return true;
        }
    }

    if (!IsAssignableTo(ComparibleParamType, Arg)) {
        // May be an implicit pointer type.
        if (Param->ImplicitPtr) {
            if (!Param->Ty->AsPointerTy()->GetElementType()->Equals(Arg->Ty)) {
                return false;
            }
            if (!IsLValue(Arg) && Arg->Ty->GetKind() != TypeKind::Struct) {
                // Must be an l-value or a struct in which case an unseen allocation
                // will occure and the address will be takne.
                return false;
            }
        } else {
            return false;
        }
    }
    if (ViolatesConstAssignment(ComparibleParamType, HasConstAddress, Arg)) {
        return false;
    }
    if (!ComparibleParamType->Equals(Arg->Ty)) {
        ++NumConflicts;
        // TODO: Doesn't this need to unbox the generic? Has it already been unboxed?
        if (Arg->Ty->GetRealKind() == TypeKind::Enum) {
            if (ComparibleParamType->Equals(Context.AnyType)) {
                // TODO: Hacky, this probably is not what we want but for now we consider
                // calling a function that takes type Any to be considered not a conflict
                // in types. Although this would imply that calling a function with Any
                // vs. the function which explicitly takes the enum type to be equivalent.
                --NumConflicts;
                return true;
            }

            // This happens so it can resolve stuff like:
            // Day enum : int {
            //     MONDAY;
            //     TUESDAY;
            //     ...
            // }
            //
            // fn foo(a int) {}
            // fn foo(a float32) {}
            //
            // foo(Day.TUESDAY); // Calls int version!
            EnumDecl* Enum = static_cast<StructType*>(Arg->Ty)->GetEnum();
            if (!Enum->ValuesType->Equals(ComparibleParamType)) {
                ++EnumImplicitConflicts;
            }
        } else if (Arg->Ty->IsNumber()) {
            if (Arg->Ty->IsSigned() != ComparibleParamType->IsSigned()) {
                ++NumSignConflicts;
            }
        }
    }
    return true;
}

bool arco::SemAnalyzer::CheckCallArgGeneric(Type* ArgTy,
                                            bool AllowImplicitPointer,
                                            Type* ParamType,
                                            BindableList& BindableTypes,
                                            BindableList& QualTypes,
                                            BindableList& StructBindableTypes,
                                            Type*& QualType,
                                            ulen QualIdx,
                                            bool IsRoot,
                                            bool FromPtr,
                                            ArgMismatchData* MismatchData) {

    // Basically we want to decend the type and continue to qualify the generic type
    // with the type information if not already qualified.

    // The type is not bound yet so cannot rely on IsAssignableTo.

    // TODO: For already bound types could potentially just optimize by calling IsAssignableTo
    // here rather than relying on passing the information up and working from there.
    switch (ParamType->GetRealKind()) {
    case TypeKind::Generic: {
        // Any type can bind to a generic type with no additional content.

        // Want to always qualify the generic types because the information
        // needs to be known when checking other parameters.

        GenericType* GenTy = static_cast<GenericType*>(ParamType);

        Type* ExistingBind;
        if (GenTy->GetBoundToDecl()->Is(AstKind::FUNC_DECL)) {
            ExistingBind = BindableTypes[GenTy->GetIdx()];
        } else {
            ExistingBind = StructBindableTypes[GenTy->GetIdx()];
        }

        if (ExistingBind) {
            // Already bound for the generic! Needs to be qualified based
            // on the existing bound type.
            QualType = ExistingBind;
        } else {
            if (!ArgTy->TypeHasStorage()) {
                // Still want to allow void*, void**, ect..
                if (!(FromPtr && ArgTy->Equals(Context.VoidType))) {
                    if (MismatchData) {
                        MismatchData->FailBesidesBind = true;

                        std::string& MismatchInfo = MismatchData->MismatchInfo;
                        if (!MismatchInfo.empty()) MismatchInfo += "\n";

                        MismatchInfo += GetGenBindCallArgHeaderInfo(*MismatchData, ArgTy)
                            + " because the type has no storage.";
                    }

                    return false;
                }
            }

            if (GenTy->GetBoundToDecl()->Is(AstKind::FUNC_DECL)) {
                BindableTypes[GenTy->GetIdx()] = ArgTy;
            } else {
                StructBindableTypes[GenTy->GetIdx()] = ArgTy;
            }
        }

        break;
    }
    case TypeKind::Pointer: {
        // TODO: Change this to allow for other pointers types like cstr
        // and also allow also take into account mismatch.

        PointerType* ParamPtrTy = static_cast<PointerType*>(ParamType);

        if (AllowImplicitPointer) {
            // Implicit pointer case have to check to see if it
            // can be taken implicitly.
            auto Res = CheckCallArgGeneric(ArgTy,
                                           false,
                                           ParamPtrTy->GetElementType(),
                                           BindableTypes,
                                           QualTypes,
                                           StructBindableTypes,
                                           QualType,
                                           QualIdx,
                                           false,
                                           true,
                                           MismatchData);
            if (!Res) {
                ShowMismatchInfoForBindFail(IsRoot, ArgTy, ParamType, MismatchData);
                return false;
            }

            // ArgTy is not the correct qualified type because it was taken implicitly.
            // Have to create the type now.
            PointerType* QualPtrTy = PointerType::Create(ArgTy, Context);
            if (QualType) {
                QualType = QualPtrTy;
            } else {
                // NOTE: Hack! Argument type is implicitely taken so that is captured here.
                ArgTy = QualPtrTy;
            }
            break;
        }

        if (ArgTy->GetKind() != TypeKind::Pointer) {
            ShowMismatchInfoForBindFail(IsRoot, ArgTy, ParamType, MismatchData);
            return false;
        }
        PointerType* ArgPtrTy = ArgTy->AsPointerTy();

        // TODO: This recursion is not needed. Could optimize to just compare base
        //       types as long as the pointer depth is the same.
        auto Res = CheckCallArgGeneric(ArgPtrTy->GetElementType(),
                                       false,
                                       ParamPtrTy->GetElementType(),
                                       BindableTypes,
                                       QualTypes,
                                       StructBindableTypes,
                                       QualType,
                                       0, false,
                                       true,
                                       MismatchData);
        if (!Res) {
            ShowMismatchInfoForBindFail(IsRoot, ArgTy, ParamType, MismatchData);
            return false;
        }

        if (QualType) {
            QualType = ArgTy;
        }

        break;
    }
    case TypeKind::Slice: {
        if (ArgTy->GetKind() != TypeKind::Slice) {
            ShowMismatchInfoForBindFail(IsRoot, ArgTy, ParamType, MismatchData);
            return false;
        }

        SliceType* ArgSliceTy   = ArgTy->AsSliceTy();
        SliceType* ParamSliceTy = static_cast<SliceType*>(ParamType);

        auto Res = CheckCallArgGeneric(ArgSliceTy->GetElementType(),
                                       false,
                                       ParamSliceTy->GetElementType(),
                                       BindableTypes,
                                       QualTypes,
                                       StructBindableTypes,
                                       QualType,
                                       0, false,
                                       MismatchData);
        if (!Res) {
            ShowMismatchInfoForBindFail(IsRoot, ArgTy, ParamType, MismatchData);
            return false;
        }

        if (QualType) {
            QualType = ArgTy;
        }

        break;
    }
    case TypeKind::Array: {
        if (ArgTy->GetKind() != TypeKind::Array) {
            ShowMismatchInfoForBindFail(IsRoot, ArgTy, ParamType, MismatchData);
            return false;
        }

        ArrayType* ArgArrTy   = ArgTy->AsArrayTy();
        ArrayType* ParamArrTy = static_cast<ArrayType*>(ParamType);
        if (ArgArrTy->GetLength() != ParamArrTy->GetLength()) {
            ShowMismatchInfoForBindFail(IsRoot, ArgTy, ParamType, MismatchData);
            return false;
        }

        auto Res = CheckCallArgGeneric(ArgArrTy->GetElementType(),
                                       false,
                                       ParamArrTy->GetElementType(),
                                       BindableTypes,
                                       QualTypes,
                                       StructBindableTypes,
                                       QualType,
                                       0, false,
                                       MismatchData);
        if (!Res) {
            ShowMismatchInfoForBindFail(IsRoot, ArgTy, ParamType, MismatchData);
            return false;
        }

        if (QualType) {
            QualType = ArgTy;
        }

        break;
    }
    case TypeKind::Function: {
        if (ArgTy->GetKind() != TypeKind::Function) {
            ShowMismatchInfoForBindFail(IsRoot, ArgTy, ParamType, MismatchData);
            return false;
        }

        FunctionType* ArgFuncTy   = ArgTy->AsFunctionType();
        FunctionType* ParamFuncTy = static_cast<FunctionType*>(ParamType);

        if (ArgFuncTy->ParamTypes.size() != ParamFuncTy->ParamTypes.size()) {
            ShowMismatchInfoForBindFail(IsRoot, ArgTy, ParamType, MismatchData);
            return false;
        }

        bool FullyQualified = false;
        if (ParamFuncTy->RetTyInfo.Ty->ContainsGenerics) {
            auto Res = CheckCallArgGeneric(ArgFuncTy->RetTyInfo.Ty,
                                           false,
                                           ParamFuncTy->RetTyInfo.Ty,
                                           BindableTypes,
                                           QualTypes,
                                           StructBindableTypes,
                                           QualType,
                                           0, 0, false,
                                           MismatchData);
            if (!Res) {
                ShowMismatchInfoForBindFail(IsRoot, ArgTy, ParamType, MismatchData);
                return false;
            }
            FullyQualified &= QualType != nullptr;
        } else if (!ParamFuncTy->RetTyInfo.Ty->Equals(ParamFuncTy->RetTyInfo.Ty)) {
            ShowMismatchInfoForBindFail(IsRoot, ArgTy, ParamType, MismatchData);
            return false;
        }

        if (ArgFuncTy->RetTyInfo.ConstMemory != ParamFuncTy->RetTyInfo.ConstMemory) {
            ShowMismatchInfoForBindFail(IsRoot, ArgTy, ParamType, MismatchData);
            return false;
        }

        for (ulen i = 0; i < ArgFuncTy->ParamTypes.size(); i++) {
            TypeInfo ArgPInfo   = ArgFuncTy->ParamTypes[i];
            TypeInfo ParamPInfo = ParamFuncTy->ParamTypes[i];
            if (ParamPInfo.Ty->ContainsGenerics) {
                auto Res = CheckCallArgGeneric(ArgPInfo.Ty,
                                               false,
                                               ParamPInfo.Ty,
                                               BindableTypes,
                                               QualTypes,
                                               StructBindableTypes,
                                               QualType,
                                               0, 0, false,
                                               MismatchData);
                if (!Res) {
                    ShowMismatchInfoForBindFail(IsRoot, ArgTy, ParamType, MismatchData);
                    return false;
                }

                FullyQualified &= QualType != nullptr;
            }
            if (ArgPInfo.ConstMemory != ParamPInfo.ConstMemory) {
                ShowMismatchInfoForBindFail(IsRoot, ArgTy, ParamType, MismatchData);
                return false;
            }
        }

        if (FullyQualified) {
            QualType = ArgTy;
        }

        break;
    }
    case TypeKind::Struct: {
        if (ArgTy->GetKind() != TypeKind::Struct) {
            ShowMismatchInfoForBindFail(IsRoot, ArgTy, ParamType, MismatchData);
            return false;
        }

        StructType* ArgStructTy   = ArgTy->AsStructType();
        StructType* ParamStructTy = static_cast<StructType*>(ParamType);
        StructDecl* ArgStruct   = ArgStructTy->GetStruct();
        StructDecl* ParamStruct = ParamStructTy->GetStruct();

        if (ArgStruct != ParamStruct) {
            ShowMismatchInfoForBindFail(IsRoot, ArgTy, ParamType, MismatchData);
            return false;
        }

        bool FullyQualified = true;
        for (ulen i = 0; i < ArgStruct->GenData->GenTys.size(); i++) {
            Type* ArgBoundTy = ArgStructTy->FoundBinding->BindableTypes[i];
            Type* ParamGenTy = ParamStructTy->GetBindTypes()[i];
            if (ParamGenTy->ContainsGenerics) {
                auto Res = CheckCallArgGeneric(ArgBoundTy,
                                               false,
                                               ParamGenTy,
                                               BindableTypes,
                                               QualTypes,
                                               StructBindableTypes,
                                               QualType,
                                               0, false,
                                               MismatchData);
                if (!Res) {
                    ShowMismatchInfoForBindFail(IsRoot, ArgTy, ParamType, MismatchData);
                    return false;
                }
                FullyQualified &= QualType != nullptr;
            } else if (!ParamGenTy->Equals(ArgBoundTy)) {
                ShowMismatchInfoForBindFail(IsRoot, ArgTy, ParamType, MismatchData);
                return false;
            }
        }

        if (FullyQualified) {
            QualType = ArgTy;
        }

        break;
    }
    default:
        assert(!"unreachable");
        break;
    }

    if (IsRoot) {
        if (QualType) {
            QualTypes[QualIdx] = QualType;
        } else {
            QualTypes[QualIdx] = ArgTy;
        }
    }

    return true;
}

bool arco::SemAnalyzer::CheckGenericConstraint(FuncDecl* Canidate,
                                               GenericType* GenTy,
                                               BindableList* BindableTypes,
                                               std::string* MismatchInfo) {

    IdentRef* Constraint = GenTy->GetConstraintRef();
    if (!Constraint) return true;

    Type* BoundTy = (*BindableTypes)[GenTy->GetIdx()];

    llvm::SmallVector<Type*, 8> ConstraintBindTypes = { BoundTy };

    if (Constraint->RefKind != IdentRef::RK::Var) {
        Canidate->ParsingError = true;
        Logger Log(Canidate->FScope, Canidate->FScope->Buffer);
        Log.BeginError(Constraint->Loc,
            "Constraint '%s' is expected to be a variable",
            Constraint->Ident);
        Log.EndError();
        return false;
    }

    VarDecl* VarRef = Constraint->Var;
    if (!VarRef->IsGeneric()) {
        Canidate->ParsingError = true;
        Logger Log(Canidate->FScope, Canidate->FScope->Buffer);
        Log.BeginError(Constraint->Loc,
            "Constraint '%s' has no generics",
            Constraint->Ident);
        Log.EndError();
        return false;
    }

    if (VarRef->GenData->GenTys.size() != ConstraintBindTypes.size()) {
        Canidate->ParsingError = true;
        Logger Log(Canidate->FScope, Canidate->FScope->Buffer);
        Log.BeginError(Constraint->Loc,
            "Constraint '%s' has %s generic type%s but only %s %s bound",
            Constraint->Ident,
            VarRef->GenData->GenTys.size(),
            VarRef->GenData->GenTys.size() == 1 ? "" : "s",
            ConstraintBindTypes.size(),
            ConstraintBindTypes.size() == 1 ? "was" : "were"
            );
        Log.EndError();
        return false;
    }

    // TODO: Generic parent!
    GenericBinding* Binding = GetExistingBinding(Constraint->Var, ConstraintBindTypes, nullptr);
    if (!Binding) {
        Binding = CreateNewBinding(VarRef, std::move(ConstraintBindTypes), nullptr);
        BindTypes(VarRef, Binding);
        // TODO: Deal with the location meaning to be local since
        // it is used to check circularity.
        EnsureChecked(VarRef->Loc, VarRef);
        UnbindTypes(VarRef);

        Type* QualType = Binding->QualifiedType;
        // First time check of the particular constraint so need
        // to make sure that the constraint is a bool type and
        // if not then produce an error.
        if (VarRef->Ty == Context.ErrorType) {
            return false;
        }
        if (!QualType ->Equals(Context.BoolType)) {
            Logger Log(Canidate->FScope, Canidate->FScope->Buffer);
            std::string TypeSs;
            for (ulen i = 0; i < Binding->BindableTypes.size(); i++) {
                Type* BindType = Binding->BindableTypes[i];
                TypeSs += BindType->ToString();
                if (i+1 != Binding->BindableTypes.size()) {
                    TypeSs += ", ";
                }
            }
            Log.BeginError(Constraint->Loc,
                "Constraint '%s' did not reduce to type bool for type%s '%s'",
                Constraint->Ident,
                Binding->BindableTypes.size() == 1 ? "" : "s",
                TypeSs
                );
            Log.EndError();
        }
    }

    Type* QualType = Binding->QualifiedType;
    if (QualType == Context.ErrorType) {
        return false;
    }
    if (!QualType->Equals(Context.BoolType)) {
        return false;
    }

    llvm::ConstantInt* LLBool = llvm::cast<llvm::ConstantInt>(Binding->LLValue);
    bool Tof = LLBool->isZero();
    if (GenTy->ShouldInvertConstraint()) {
        Tof = !Tof;
    }
    if (Tof) {
        // The constraint condition failed!
        if (MismatchInfo) {
            if (!MismatchInfo->empty()) *MismatchInfo += "\n";
            *MismatchInfo += "- Generic constraint '"
                + (GenTy->ShouldInvertConstraint() ? std::string("!") : std::string(""))
                + Constraint->Ident.Text.str()
                + "' failed with bound type '"
                + BoundTy->ToString() + "'.\n  Constraint defined at: "
                + VarRef->FScope->Path + ":" + std::to_string(VarRef->Loc.LineNumber) + ".";
        }
        return false;
    }

    return true;
}

void arco::SemAnalyzer::DisplayErrorForNoMatchingFuncCall(SourceLoc ErrorLoc,
                                                          FuncsList* Canidates,
                                                          const llvm::SmallVector<Expr*>&      Args,
                                                          const llvm::SmallVector<NamedValue>& NamedArgs,
                                                          BindableList* StructBindableTypes) {

    const char* CallType = (*Canidates)[0]->IsConstructor ? "constructor" : "function";

    if (Canidates && Canidates->size() == 1) {
        FuncDecl* SingleCanidate = (*Canidates)[0];

        auto ParamTypes = ParamsToTypeInfo(SingleCanidate);
        DisplayErrorForSingleFuncForFuncCall(CallType,
                                             ErrorLoc,
                                             ParamTypes,
                                             Args,
                                             NamedArgs,
                                             SingleCanidate->NumDefaultArgs,
                                             SingleCanidate,
                                             StructBindableTypes);
    } else {
        if (!Canidates) {
            Error(ErrorLoc, "Could not find %s for call", CallType);
            return;
        }

        for (FuncDecl* Canidate : *Canidates) {
            if (Canidate->ParsingError) {
                // Let us not confuse the user by saying they couldn't
                // find a call to a function with bad arguments.
                return;
            }
        }

        // TODO: Since this limits the amount of functions that are shown to the user it should select the
        // closest matches and show those to increase finding the right function.
        FuncDecl* FirstCanidate = (*Canidates)[0];

        llvm::SmallVector<TypeInfo> ArgTypes;
        ArgTypes.reserve(Args.size());
        for (Expr* Arg : Args) {
            ArgTypes.push_back(TypeInfo{
                Arg->Ty,
                Arg->HasConstAddress
                });
        }
        std::string ErrorMsg = "Could not find overloaded " + std::string(CallType)
            + " '" + GetFuncDefForError(ArgTypes, FirstCanidate) + "'.";

        ErrorMsg += "\n\n  Possible Canidates:";
        ulen LongestDefLength = 0;
        for (FuncDecl* Canidate : *Canidates) {
            ulen Len = GetFuncDefForError(ParamsToTypeInfo(Canidate), Canidate).length();
            if (Len > LongestDefLength) {
                LongestDefLength = Len;
            }
        }
        ulen Count = 0;
        bool CanidateHasParsingError = false;

        ulen GenericFuncIdx = 0;
        for (FuncDecl* Canidate : *Canidates) {
            if (Count == Context.BailCountForShowingOverloadedFuncs) {
                ErrorMsg += "\n\n  And " + std::to_string(Canidates->size() - Count) + " more...";
                break;
            }
            llvm::SmallVector<TypeInfo> ParamTypes = ParamsToTypeInfo(Canidate);
            ErrorMsg += "\n\n     ";
            std::string Def = GetFuncDefForError(ParamTypes, Canidate);
            ErrorMsg += Def;
            ErrorMsg += std::string(LongestDefLength - Def.length(), ' ') + "   - declared at: "
                + Canidate->FScope->Path + std::string(":") + std::to_string(Canidate->Loc.LineNumber);
            ErrorMsg += "\n";

            ulen NumGenerics = 0, NumQualifications = 0;
            if (Canidate->IsGeneric()) {
                NumGenerics = Canidate->GenData->GenTys.size();
                NumQualifications = Canidate->GenData->NumQualifications;
            }
            BindableList BindableTypes(NumGenerics);
            BindableList QualTypes(NumQualifications);

            bool PreliminaryError = false;
            std::string MismatchInfo = GetCallMismatchInfo(CallType,
                                                           ParamTypes,
                                                           Args,
                                                           NamedArgs,
                                                           Canidate->NumDefaultArgs,
                                                           Canidate->IsVariadic,
                                                           NumGenerics,
                                                           NumQualifications,
                                                           BindableTypes,
                                                           QualTypes,
                                                           StructBindableTypes,
                                                           Canidate,
                                                           PreliminaryError);
            // Need to check to make sure that the generic types actually computed
            // correctly and did not just result in an error. If they did this is
            // considered equivalent to the function declaration having a parsing
            // error so no error is reported here.
            if (Canidate->IsGeneric() && !PreliminaryError) {
                if (DidGenericConstraintsHaveErrors(Canidate, BindableTypes)) {
                    return;
                }
            }

            std::stringstream StrStream(MismatchInfo.c_str());
            std::string Line;
            bool First = true;
            while (std::getline(StrStream, Line, '\n')) {
                if (!First) {
                    ErrorMsg += "\n";
                }
                First = false;
                ErrorMsg += "        " + Line;
            }
            ++Count;
        }

        ErrorMsg += "\n";
        Log.BeginError(ErrorLoc, ErrorMsg.c_str(), false);
        Log.EndError();

    }
}

void arco::SemAnalyzer::DisplayErrorForSingleFuncForFuncCall(
    const char* CallType,
    SourceLoc CallLoc,
    llvm::SmallVector<TypeInfo>& ParamTypes,
    const llvm::SmallVector<Expr*>&      Args,
    const llvm::SmallVector<NamedValue>& NamedArgs,
    ulen NumDefaultArgs,
    FuncDecl* CalledFunc,
    BindableList* StructBindableTypes) {

    if (CalledFunc && CalledFunc->ParsingError) {
        return;
    }

    // Single canidate so explicit details about
    // how there is a mismatch between the call and
    // the function is given.

    std::string FuncDef = GetFuncDefForError(ParamTypes, CalledFunc);
    std::string ErrorMsg = "Expected call to " + std::string(CallType) + " declaration: " + FuncDef + ".";
    if (CalledFunc) {
        std::string First = std::string(CallType);
        First[0] = std::toupper(First[0]);
        ErrorMsg += "\n" + std::string(Log.CalcHeaderIndent(CallLoc), ' ')
             + First + " declared at: "
             + CalledFunc->FScope->Path + std::string(":") + std::to_string(CalledFunc->Loc.LineNumber);
    }
    ErrorMsg += "\n\n";

    ulen NumGenerics = 0, NumQualifications = 0;
    if (CalledFunc && CalledFunc->IsGeneric()) {
        NumGenerics = CalledFunc->GenData->GenTys.size();
        NumQualifications = CalledFunc->GenData->NumQualifications;
    }
    BindableList BindableTypes(NumGenerics);
    BindableList QualTypes(NumQualifications);

    bool IsVariadic = CalledFunc ? CalledFunc->IsVariadic : false;
    bool PreliminaryError = false;
    std::string ExtMsg = GetCallMismatchInfo(CallType,
                                             ParamTypes,
                                             Args,
                                             NamedArgs,
                                             NumDefaultArgs,
                                             IsVariadic,
                                             NumGenerics,
                                             NumQualifications,
                                             BindableTypes,
                                             QualTypes,
                                             StructBindableTypes,
                                             CalledFunc,
                                             PreliminaryError);
    if (CalledFunc && CalledFunc->IsGeneric() && !PreliminaryError) {
        // Need to check to make sure that the generic types actually computed
        // correctly and did not just result in an error. If they did this is
        // considered equivalent to the function declaration having a parsing
        // error so no error is reported here.
        if (DidGenericConstraintsHaveErrors(CalledFunc, BindableTypes)) {
            return;
        }
    }

    Log.SetMsgToShowAbovePrimaryLocAligned(ExtMsg.c_str());
    Log.BeginError(CallLoc, ErrorMsg.c_str(), false);
    Log.EndError();
}

bool arco::SemAnalyzer::DidGenericConstraintsHaveErrors(FuncDecl* Canidate,
                                                        const BindableList& BindableTypes) {
    for (GenericType* GenTy : Canidate->GenData->GenTys) {
        IdentRef* Constraint = GenTy->GetConstraintRef();
        if (!Constraint) continue;

        Type* BoundTy = BindableTypes[GenTy->GetIdx()];
        llvm::SmallVector<Type*, 8> ConstraintBindTypes = { BoundTy };

        // TODO: Don't just assume it is a variable.
        VarDecl* VarRef = Constraint->Var;
        if (VarRef->GenData->GenTys.size() != ConstraintBindTypes.size()) {
            return true;
        }
        if (VarRef->Ty == Context.ErrorType) {
            return true;
        }

        // TODO: parent binding
        GenericBinding* Binding = GetExistingBinding(Constraint->Var, ConstraintBindTypes, nullptr);
        BindTypes(VarRef, Binding);
        Type* QualType = VarRef->GenData->CurBinding->QualifiedType;
        if (QualType == Context.ErrorType) {
            return true;
        }
        if (!QualType->Equals(Context.BoolType)) {
            return true;
        }
        UnbindTypes(VarRef);
    }
    return false;
}

std::string arco::SemAnalyzer::GetFuncDefForError(const llvm::SmallVector<TypeInfo>& ParamTypes, FuncDecl* CalledFunc) {
    bool IsVariadic = CalledFunc && CalledFunc->IsVariadic;
    std::string FuncDef;
    if (CalledFunc) {
        if (!CalledFunc->IsConstructor) {
            FuncDef += "fn ";
        }
        FuncDef += CalledFunc->Name.Text.str();
    } else {
        FuncDef += "fn";
    }
    FuncDef += "(";
    for (ulen i = 0; i < ParamTypes.size(); i++) {
        FuncDef += (ParamTypes[i].ConstMemory && ParamTypes[i].Ty->GetKind() != TypeKind::CStr) ? "const " : "";
        if (!IsVariadic || i+1 != ParamTypes.size()) {
            FuncDef += ParamTypes[i].Ty->ToString(false);
        } else {
            FuncDef += ParamTypes[i].Ty->AsSliceTy()->GetElementType()->ToString(false);
        }
        if (i != ParamTypes.size() - 1) {
            FuncDef += ", ";
        }
    }
    if (IsVariadic) {
        FuncDef += "...";
    }
    FuncDef += ")";
    return FuncDef;
}

std::string arco::SemAnalyzer::GetCallMismatchInfo(const char* CallType,
                                                   llvm::SmallVector<TypeInfo>& ParamTypes,
                                                   const llvm::SmallVector<Expr*>&      Args,
                                                   const llvm::SmallVector<NamedValue>& NamedArgs,
                                                   ulen NumDefaultArgs,
                                                   bool IsVariadic,
                                                   ulen NumGenerics,
                                                   ulen NumQualifications,
                                                   BindableList& BindableTypes,
                                                   BindableList& QualTypes,
                                                   BindableList* StructBindableTypes,
                                                   FuncDecl* Canidate,
                                                   bool& PreliminaryError) {

    if (Canidate) {
        if (Canidate->Mods & ModKinds::PRIVATE) {
            if (Canidate->FScope != FScope) {
                PreliminaryError = true;
                return "- Function not visible, it is private";
            }
        }
    }

    if (IsVariadic && !NamedArgs.empty()) {
        PreliminaryError = true;
        return "- Cannot call variadic " + std::string(CallType) + " with named arguments";
    }

    bool IncorrectNumberOfArgs = false;
    std::string MismatchInfo = "";
    ulen TotalArgs = Args.size() + NamedArgs.size();
    if (NumDefaultArgs) {
        ulen MinArgs = ParamTypes.size() - NumDefaultArgs;
        ulen MaxArgs = ParamTypes.size();
        if (!(TotalArgs >= MinArgs &&
              TotalArgs <= MaxArgs)) {
            IncorrectNumberOfArgs = true;
            MismatchInfo = "- Incorrect number of arguments. Expected between "
                     + std::to_string(MinArgs) + std::string("-") + std::to_string(MaxArgs)
                     + ". Got " + std::to_string(TotalArgs) + ".";

        }
    } else {
        if (IsVariadic) {
            if (TotalArgs < ParamTypes.size() - 1) {
                IncorrectNumberOfArgs = true;
                MismatchInfo = "- Incorrect number of arguments. Expected at least "
                     + std::to_string(ParamTypes.size() - 1)
                     + ". Got " + std::to_string(TotalArgs) + ".";
            }
        } else {
            if (TotalArgs != ParamTypes.size()) {
                IncorrectNumberOfArgs = true;
                MismatchInfo = "- Incorrect number of arguments. Expected "
                     + std::to_string(ParamTypes.size())
                     + ". Got " + std::to_string(TotalArgs) + ".";
            }
        }
    }

    if (IncorrectNumberOfArgs) {
        PreliminaryError = true;
        return MismatchInfo;
    }

    // One or more of the arguments are incorrect.
    ulen GenQualIdx = 0;
    bool EncounteredError = false;
    for (ulen ArgCount = 0; ArgCount < Args.size(); ArgCount++) {
        Expr* Arg = Args[ArgCount];
        bool  ParamConstMemory, AllowImplicitPtr;
        Type* ParamTy;
        if (IsVariadic) {
            if (ArgCount >= ParamTypes.size() - 1) {
                TypeInfo ParamTyInfo = ParamTypes[ParamTypes.size() - 1];
                ParamTy          = ParamTyInfo.Ty->AsSliceTy()->GetElementType();
                ParamConstMemory = ParamTyInfo.ConstMemory;
                AllowImplicitPtr = ParamTyInfo.AllowImplicitPtr;
            } else {
                TypeInfo ParamTyInfo = ParamTypes[ArgCount];
                ParamTy          = ParamTyInfo.Ty;
                ParamConstMemory = ParamTyInfo.ConstMemory;
                AllowImplicitPtr = ParamTyInfo.AllowImplicitPtr;
            }
        } else {
            TypeInfo ParamTyInfo = ParamTypes[ArgCount];
            ParamTy          = ParamTyInfo.Ty;
            ParamConstMemory = ParamTyInfo.ConstMemory;
            AllowImplicitPtr = ParamTyInfo.AllowImplicitPtr;
        }

        ArgMismatchData ArgData = {
            ArgCount,
            Identifier(),
            MismatchInfo
        };
        GetCallMismatchInfoForArg(
            ParamTy,
            Arg,
            AllowImplicitPtr,
            ParamConstMemory,
            BindableTypes,
            QualTypes,
            StructBindableTypes,
            GenQualIdx,
            ArgData
        );
    }

    if (Canidate) {
        for (const NamedValue& NamedArg : NamedArgs) {

            auto Itr = std::find_if(Canidate->Params.begin(), Canidate->Params.end(),
                [&NamedArg](VarDecl* Param) {
                    return Param->Name == NamedArg.Name;
                });
            if (Itr == Canidate->Params.end()) {
                if (EncounteredError)  MismatchInfo += "\n";
                MismatchInfo += "- Could not find parameter by name: '" + NamedArg.Name.Text.str() + "'.";
                EncounteredError = true;
                continue;
            }

            VarDecl* Param = *Itr;

            Expr* Arg = NamedArg.AssignValue;
            bool  ParamConstMemory = Param->HasConstAddress;
            Type* ParamTy = Param->Ty;

            ArgMismatchData ArgData = {
                static_cast<ulen>(-1),
                NamedArg.Name,
                MismatchInfo
            };
            GetCallMismatchInfoForArg(
                ParamTy,
                Arg,
                Param->ImplicitPtr,
                ParamConstMemory,
                BindableTypes,
                QualTypes,
                StructBindableTypes,
                GenQualIdx,
                ArgData
            );
        }
    }

    if (Canidate && Canidate->IsGeneric()) {
        for (GenericType* GenTy : Canidate->GenData->GenTys) {
            CheckGenericConstraint(Canidate, GenTy, &BindableTypes, &MismatchInfo);
        }
    }

    return MismatchInfo;
}

void arco::SemAnalyzer::GetCallMismatchInfoForArg(Type* ParamTy,
                                                  Expr* Arg,
                                                  bool AllowImplicitPtr,
                                                  bool ParamConstMemory,
                                                  BindableList& BindableTypes,
                                                  BindableList& QualTypes,
                                                  BindableList* StructBindableTypes,
                                                  ulen& QualIdx,
                                                  ArgMismatchData& MismatchData) {
    Type* QualType = nullptr;
    bool IsAssignable = true;
    bool GenRes = true;
    if (ParamTy->ContainsGenerics) {
        Type* ExistingQualType = nullptr;
        GenRes = CheckCallArgGeneric(Arg->Ty->UnboxGeneric(),
                                     AllowImplicitPtr,
                                     ParamTy,
                                     BindableTypes,
                                     QualTypes,
                                     *StructBindableTypes,
                                     ExistingQualType,
                                     QualIdx,
                                     true,
                                     false,
                                     &MismatchData);

        if (GenRes && ExistingQualType) {
            IsAssignable = IsAssignableTo(ExistingQualType, Arg->Ty, Arg);
        }

        QualType = QualTypes[QualIdx];
        if (QualType) {
            ParamConstMemory |= QualType->Equals(Context.CStrType);
        }

    } else if (!IsAssignableTo(ParamTy, Arg->Ty, Arg)) {
        IsAssignable = false;
    }

    if (!IsAssignable) {

        std::string ParamTyString;
        if (ParamTy->ContainsGenerics) {
            ParamTyString = ParamTy->ToString(false, &BindableTypes, StructBindableTypes);
        } else {
            ParamTyString = ParamTy->ToString();
        }

        std::string& MismatchInfo = MismatchData.MismatchInfo;
        if (!MismatchInfo.empty())  MismatchInfo += "\n";

        if (MismatchData.ArgCount == -1) {
            MismatchInfo += "- Cannot assign named argument '" + MismatchData.Name.Text.str() + "'"
                + " of type '" + Arg->Ty->ToString() + "' "
                + "to parameter of type '" + ParamTyString + "'.";
        } else {
            MismatchInfo += "- Cannot assign argument "
                + std::to_string(MismatchData.ArgCount + 1) + " of type '" + Arg->Ty->ToString() + "' "
                + "to parameter of type '" + ParamTyString + "'.";
        }
    }

    if (ParamTy->ContainsGenerics) {
        ++QualIdx;
    }

    if (IsAssignable && GenRes) {
        if (ViolatesConstAssignment(QualType ? QualType : ParamTy, ParamConstMemory, Arg)) {
            std::string& MismatchInfo = MismatchData.MismatchInfo;
            if (!MismatchInfo.empty())  MismatchInfo += "\n";

            if (MismatchData.ArgCount == -1) {
                MismatchInfo += "- Cannot assign named argument '" + MismatchData.Name.Text.str() + "'"
                    + " with const memory to non-const parameter.";
            } else {
                MismatchInfo += "- Cannot assign argument "
                    + std::to_string(MismatchData.ArgCount + 1)
                    + " with const memory to non-const parameter.";
            }
        }
    }
}

void arco::SemAnalyzer::ShowMismatchInfoForBindFail(bool IsRoot,
                                                    Type* ArgTy,
                                                    Type* ParamType,
                                                    ArgMismatchData* MismatchData) {
    if (!IsRoot)                       return;
    if (!MismatchData)                 return;
    if (MismatchData->FailBesidesBind) return;

    std::string& MismatchInfo = MismatchData->MismatchInfo;
    if (!MismatchInfo.empty()) MismatchInfo += "\n";

    MismatchInfo += GetGenBindCallArgHeaderInfo(*MismatchData, ArgTy)
        + " to parameter of type '" + ParamType->ToString(false) + "'.";
}

std::string arco::SemAnalyzer::GetGenBindCallArgHeaderInfo(ArgMismatchData& MismatchData, Type* ArgTy) {
    if (MismatchData.ArgCount != -1) {
        return "- Failed to bind argument " + std::to_string(MismatchData.ArgCount + 1)
            + " of type '" + ArgTy->ToString() + "'";
    } else {
        return "- Failed to bind named argument '" + MismatchData.Name.Text.str()
            + "' of type '" + ArgTy->ToString() + "'";
    }
    return std::string();
}

void arco::SemAnalyzer::DisplayErrorForBadCallSiteType(FuncCall* Call) {
    Type* SiteTy = Call->Site->Ty;
    Log.BeginError(Call->Loc, "cannot call type '%s'", SiteTy);
    if (SiteTy->GetKind() == TypeKind::StructRef) {
        IdentRef* IRef = static_cast<IdentRef*>(Call->Site);
        StructDecl* Struct = IRef->Struct;
        // TODO: Should this validate that there is a constructor first?
        Log.AddNoteLine([Struct, Call](auto& OS) {
            OS << "Calling a constructor? Use: ";
            OS << Struct->Name;
            if (Call->Args.empty()) {
                OS << "{}";
            } else {
                OS << "{ ";
                // Cannot rely on the locations of the arguments because
                // there are named and non named arguments. Going to
                // traverse the text and since parens must balance otherwise
                // there would have been a parsing error we may simply rely
                // on the count of them to determine a stopping condition.
                llvm::StringRef BeginText = Call->Loc.Text;
                ulen ParamCount = 1;
                const char* TextPtr = BeginText.data();

                llvm::SmallVector<std::pair<const char*, ulen>> ArgRangePairs;
                for (Expr* Arg : Call->Args) {
                    SourceLoc ArgLoc = GetExpandedLoc(Arg);
                    const char* TextStart = ArgLoc.Text.data();
                    ArgRangePairs.push_back({ TextStart, ArgLoc.Text.size() });
                }
                for (NamedValue& Arg : Call->NamedArgs) {
                    SourceLoc ArgLoc = GetExpandedLoc(Arg.AssignValue);
                    const char* TextStart = ArgLoc.Text.data();
                    ArgRangePairs.push_back({ TextStart, ArgLoc.Text.size() });
                }

                bool AllArgsTooLong = true;
                bool ArgTextTooLong = false;
                ++TextPtr; // Skip first param.
                while (ParamCount != 0) {

                    auto Itr = std::find_if(ArgRangePairs.begin(), ArgRangePairs.end(),
                        [TextPtr](auto& RangePair) {
                            return TextPtr == std::get<0>(RangePair);
                        });
                    if (Itr != ArgRangePairs.end()) {
                        auto& RangePair = *Itr;
                        // TODO: Would be nice for this to not include the trimmed space.
                        ulen ArgTextLength = std::get<1>(RangePair);
                        ulen TotalLength = (TextPtr + ArgTextLength) - BeginText.data() - 1;
                        if (TotalLength > 12) {
                            ArgTextTooLong = true;
                            break;
                        }
                        AllArgsTooLong = false;
                        TextPtr += ArgTextLength;
                    }

                    if (*TextPtr == '(') {
                        ++ParamCount;
                    } else if (*TextPtr == ')') {
                        --ParamCount;
                    }
                    ++TextPtr;
                }

                ulen ArgTextLength = TextPtr - BeginText.begin();
                if (!ArgTextTooLong) {
                    ArgTextLength -= 2; // -2 skip )
                } else {
                    ArgTextLength -= 1;
                }

                llvm::StringRef ArgsText = llvm::StringRef(
                    BeginText.begin() + 1, // +1 skip (
                    ArgTextLength
                );

                ArgsText = ArgsText.trim();
                OS << ArgsText;
                if (ArgTextTooLong) {
                    SetTerminalColor(TerminalColorBrightBlue);
                    if (!AllArgsTooLong) {
                        OS << " ";
                    }
                    OS << "...";
                    SetTerminalColor(TerminalColorDefault);
                }
                OS << " }";
            }

            OS << " instead.";

            });
    }
    Log.EndError();

}

void arco::SemAnalyzer::CheckIfErrorsAreCaptured(SourceLoc ErrorLoc, FuncDecl* CalledFunc) {
    bool HasRaises = true;
    if (CFunc) {
        for (const auto& RaisedError : CalledFunc->RaisedErrors) {
            auto Itr = std::find_if(CFunc->RaisedErrors.begin(),
                CFunc->RaisedErrors.end(),
                [&RaisedError](const auto& OurRaisedError) {
                    return RaisedError.ErrorStruct == OurRaisedError.ErrorStruct;
                });
            if (Itr == CFunc->RaisedErrors.end()) {
                HasRaises = false;
                break;
            }
        }
    } else {
        HasRaises = false;
    }

    if (!HasRaises) {
        Error(ErrorLoc, "Cannot ignore raised errors from %s call",
            CalledFunc->IsConstructor ? "constructor" : "function");
    }
}

void arco::SemAnalyzer::CheckTryError(TryError* Try) {

    if (Context.StandAlone) {
        Error(Try, "Cannot use try expression when -stand-alone flag is set");
        YIELD_ERROR(Try);
    }

    if (!CheckStdPanicFuncExists()) {
        YIELD_ERROR(Try);
    }

    if (Try->Value->Is(AstKind::FUNC_CALL)) {

        CatchingErrors = true;

        FuncCall* Call = static_cast<FuncCall*>(Try->Value);
        CheckFuncCall(Call);
        YIELD_ERROR_WHEN(Try, Try->Value);

        CatchingErrors = false;

        Try->Ty = Try->Value->Ty;
        Try->IsFoldable = false;

        if (!Call->CalledFunc || Call->CalledFunc->RaisedErrors.empty()) {
            Error(Try, "Expected called function to raise errors");
            YIELD_ERROR(Try);
        }

    } else {
        Error(Try, "Expected try to handle errors from function call");
        YIELD_ERROR(Try);
    }
}

bool arco::SemAnalyzer::CheckStdPanicFuncExists() {
    if (Context.StdErrorPanicFunc) return true;

    Module* StdModule = Context.ModNamesToMods.find("std")->second;
    auto Itr = StdModule->DefaultNamespace->Funcs.find(Identifier("panic"));
    if (Itr == StdModule->DefaultNamespace->Funcs.end()) {
        Logger::GlobalError(llvm::errs(), "Standard library is missing 'panic' function");
    } else {
        FuncsList& List = Itr->second;
        bool Found = false;

        for (FuncDecl* Func : List) {

            SemAnalyzer A(Context, Func);
            A.CheckFuncDecl(Func);
            if (Func->Params.size() != 1) {
                continue;
            }

            if (!Func->Params[0]->Ty->Equals(Context.ErrorInterfacePtrType)) {
                continue;
            }
            Context.StdErrorPanicFunc = Func;
            Found = true;
            break;
        }
        if (!Found) {
            Logger::GlobalError(llvm::errs(), "Standard library is missing 'panic' function");
            return false;
        }
    }
    RequestGenNonGenericFunc(Context, Context.StdErrorPanicFunc);
    return true;
}

void arco::SemAnalyzer::CheckArray(Array* Arr) {

    Type* ElmTypes = Arr->ReqBaseType;
    bool ElmHaveErrors = false, ElmAreArrs = false;
    for (Expr* Elm : Arr->Elements) {
        CheckNode(Elm);
        if (Elm->Ty == Context.ErrorType) {
            ElmHaveErrors = true;
            continue;
        }
        if (Elm->Is(AstKind::ARRAY)) {
            ElmAreArrs = true;
        }

        if (!Elm->IsFoldable) {
            Arr->IsFoldable = false;
        }

        if ((Elm->Ty->GetKind() == TypeKind::Pointer &&
             Elm->HasConstAddress)) {
            Error(Elm, "Cannot add const pointers to arrays");
        }

        if (!ElmTypes) {
            ElmTypes = Elm->Ty;
        } else if (ElmAreArrs) {
            // The sub-arrays must have exactly the same type.
            if (!ElmTypes->Equals(Elm->Ty)) {
                Error(Elm, "Array has incompatible sub-array elements");
            }
        } else {
            if (!IsAssignableTo(ElmTypes, Elm->Ty, Elm)) {
                // Maybe the reserve is allowed.
                if (Arr->ReqBaseType) {
                    DisplayErrorForTypeMismatch(
                        "Array element not assignable to explicit array type. Element type '%s', Array type: '%s'",
                        Elm->Loc,
                        Elm,
                        Arr->ReqBaseType);
                } else if (!IsAssignableTo(Elm->Ty, ElmTypes, nullptr)) {
                    DisplayErrorForTypeMismatch(
                        "Array has incompatible elements. Element type '%s', Array type: '%s'",
                        Elm->Loc,
                        Elm,
                        ElmTypes);
                } else {
                    ElmTypes = Elm->Ty;
                }
            }
        }
    }

    if (ElmHaveErrors) {
        YIELD_ERROR(Arr);
    }

    if (!ElmTypes) {
        ElmTypes = Context.EmptyArrayElmType;
    }

    // Making sure all the elements are the same type.
    if (!ElmAreArrs) {
        for (Expr* Elm : Arr->Elements) {
            CreateCast(Elm, ElmTypes);
        }
    }

    Arr->Ty = ArrayType::Create(ElmTypes, Arr->RequiredNumElements, Context);

}

void arco::SemAnalyzer::CheckArrayAccess(ArrayAccess* Access) {

    CheckNode(Access->Index);
    CheckNode(Access->Site);

    YIELD_ERROR_WHEN(Access, Access->Index);
    YIELD_ERROR_WHEN(Access, Access->Site);

    if (!Access->Index->Ty->IsInt()) {
        Error(Access, "Expected int type for index. Found type '%s'", Access->Index->Ty);
    }

    TypeKind Kind = Access->Site->Ty->GetKind();
    if (!(Kind == TypeKind::Array || Kind == TypeKind::Pointer || Kind == TypeKind::CStr ||
          Kind == TypeKind::Slice)) {
        Error(Access, "Cannot index non-array or pointer type. Type was '%s'",
            Access->Site->Ty);
        YIELD_ERROR(Access);
    }

    Access->IsFoldable = false;
    if (Kind == TypeKind::CStr) {
        Access->Ty = Context.CharType;
        Access->HasConstAddress = true;
    } else {
        Access->Ty = Access->Site->Ty->AsContainerType()->GetElementType();
    }
}

void arco::SemAnalyzer::CheckTypeCast(TypeCast* Cast) {
    Cast->Ty = Cast->ToType;
    if (!FixupType(Cast->Ty)) {
        YIELD_ERROR(Cast);
    }

    CheckNode(Cast->Value);

    YIELD_ERROR_WHEN(Cast, Cast->Value);
    Cast->IsFoldable = Cast->Value->IsFoldable;
    Cast->HasConstAddress = Cast->Value->HasConstAddress;

    if (!IsCastableTo(Cast->Ty, Cast->Value->Ty)) {
        Error(Cast, "Cannot cast from type '%s' to type '%s'", Cast->Value->Ty, Cast->Ty);
        YIELD_ERROR(Cast);
    }
    if (Cast->Value->Ty->GetKind() == TypeKind::CStr) {
        Cast->IsFoldable = false;
    }
}

void arco::SemAnalyzer::CheckTypeBitCast(TypeBitCast* Cast) {
    Cast->Ty = Cast->ToType;
    if (!FixupType(Cast->Ty)) {
        YIELD_ERROR(Cast);
    }

    CheckNode(Cast->Value);
    YIELD_ERROR_WHEN(Cast, Cast->Value);
    Cast->IsFoldable = true;
    Cast->HasConstAddress = Cast->Value->HasConstAddress;

    Type* ValueTy = Cast->Value->Ty;
    if (!( (Cast->Ty->IsNumber() || Cast->Ty->IsQualifiedPointer()) &&
           (ValueTy->IsNumber()  || ValueTy->IsQualifiedPointer() )
         )
        ) {
        Error(Cast, "Cannot bitcast from type '%s' to type '%s'", Cast->Value->Ty, Cast->Ty);
        YIELD_ERROR(Cast);
    } else {
        if (Cast->Ty->GetSizeInBytes(Context.LLArcoModule) != ValueTy->GetSizeInBytes(Context.LLArcoModule)) {
            Error(Cast, "Cannot bitcast types of different sizes");
            YIELD_ERROR(Cast);
        }
    }
    if (Cast->Value->Ty->GetKind() == TypeKind::CStr) {
        Cast->IsFoldable = false;
    }
}

void arco::SemAnalyzer::CheckStructInitializer(StructInitializer* StructInit) {
    StructType* StructTy = StructInit->Ty->AsStructType();
    if (!FixupStructType(StructTy, false)) {
        YIELD_ERROR(StructInit);
    }
    if (StructTy->GetStruct()->ParsingError) {
        return;
    }

    // TODO: May want to allow if the fields are foldable!
    StructInit->IsFoldable = false;
    StructInit->CalledConstructor = CheckStructInitArgs(StructTy,
                                                        StructInit->Loc,
                                                        StructInit->Args,
                                                        StructInit->NamedArgs,
                                                        StructInit->VarArgsPassAlong);

}

arco::FuncDecl* arco::SemAnalyzer::CheckStructInitArgs(StructType* StructTy,
                                                       SourceLoc ErrorLoc,
                                                       llvm::SmallVector<Expr*>&      Args,
                                                       llvm::SmallVector<NamedValue>& NamedArgs,
                                                       bool& VarArgPassAlong) {

    bool ArgsHaveErrors = false;
    for (ulen i = 0; i < Args.size(); i++) {
        Expr* Value = Args[i];
        CheckNode(Value);
        if (Value->Ty == Context.ErrorType) {
            ArgsHaveErrors = true;
        }
    }
    for (NamedValue& NamedArg : NamedArgs) {
        CheckNode(NamedArg.AssignValue);
        if (NamedArg.AssignValue->Ty == Context.ErrorType) {
            ArgsHaveErrors = true;
        }
    }

    StructDecl* Struct = StructTy->GetStruct();
    if (!Struct->Constructors.empty()) {
        // Calling constructor!

        if (ArgsHaveErrors) {
            return nullptr;
        }

        GenericBinding* Binding;
        Type* RetTy;
        return CheckCallToCanidates(
                Identifier{},
                ErrorLoc,
                &Struct->Constructors,
                Args,
                NamedArgs,
                VarArgPassAlong,
                Binding,
                RetTy,
                StructTy->FoundBinding
            );
    }

    for (ulen i = 0; i < Args.size(); i++) {
        Expr* Value = Args[i];

        if (i >= Struct->Fields.size()) {
            Error(GetExpandedLoc(Value), "Too many fields in initializer");
            return nullptr;
        }

        if (Value->Ty == Context.ErrorType) {
            continue;
        }

        VarDecl* Field = Struct->Fields[i];
        Type* FieldTy = Field->Ty;
        if (!Field->Assignment && Field->Ty->GetKind() == TypeKind::Struct) {
            StructDecl* StructForTy = Field->Ty->AsStructType()->GetStruct();

            if (!StructForTy->Constructors.empty() && !StructForTy->DefaultConstructor) {
                Error(ErrorLoc, "No default constructor to initialize the field '%s'", Field->Name);
            }
        }

        if (!IsAssignableTo(FieldTy, Value)) {
            DisplayErrorForTypeMismatch(
                "Cannot assign value of type '%s' to field of type '%s'",
                GetExpandedLoc(Value),
                Value,
                FieldTy);
        } else if (ViolatesConstAssignment(Field, Value)) {
            Error(GetExpandedLoc(Value), "Cannot assign argument with const memory to non-const field");
        } else {
            CreateCast(Value, FieldTy);
        }
    }
    for (NamedValue& NamedArg : NamedArgs) {
        if (NamedArg.AssignValue->Ty == Context.ErrorType) {
            continue;
        }

        auto Itr = std::find_if(Struct->Fields.begin(), Struct->Fields.end(),
            [&NamedArg](VarDecl* Field) {
                return NamedArg.Name == Field->Name;
            });
        if (Itr != Struct->Fields.end()) {

            VarDecl* Field = *Itr;
            NamedArg.VarRef = Field;

            if (Field->FieldIdx < Args.size()) {
                Error(NamedArg.NameLoc,
                    "Named argument '%s' already taken by non named argument at index: %s",
                    NamedArg.Name,
                    Field->FieldIdx + 1);
            }

            if (!IsAssignableTo(Field->Ty, NamedArg.AssignValue)) {
                DisplayErrorForTypeMismatch(
                    "Cannot assign value of type '%s' to field of type '%s'",
                    GetExpandedLoc(NamedArg.AssignValue),
                    NamedArg.AssignValue,
                    Field->Ty);
            } else if (ViolatesConstAssignment(Field, NamedArg.AssignValue)) {
                Error(GetExpandedLoc(NamedArg.AssignValue), "Cannot assign argument with const memory to non-const field");
            } else {
                CreateCast(NamedArg.AssignValue, Field->Ty);
            }
        } else {
            Error(NamedArg.NameLoc, "Could not find field by name: '%s'", NamedArg.Name);
        }
    }

    return nullptr;
}

void arco::SemAnalyzer::CheckHeapAlloc(HeapAlloc* Alloc, Type* AssignToType) {
    Alloc->IsFoldable = false;

    Type* TypeToAlloc = Alloc->TypeToAlloc;

    if (!FixupType(TypeToAlloc, true)) {
        YIELD_ERROR(Alloc);
    }


    if (TypeToAlloc->GetKind() == TypeKind::Struct) {
        StructType* StructTy = TypeToAlloc->AsStructType();
        Alloc->CalledConstructor = CheckStructInitArgs(StructTy,
                                                       Alloc->Loc,
                                                       Alloc->Values,
                                                       Alloc->NamedValues,
                                                       Alloc->VarArgsPassAlong);
    } else if (!Alloc->Values.empty()) {
        if (Alloc->Values.size() > 1) {
            Error(Alloc->Loc, "Too many values to initialize type '%s'", Alloc->TypeToAlloc);
        } else {
            CheckNode(Alloc->Values[0]);
            if (Alloc->Values[0]->Ty != Context.ErrorType) {
                if (TypeToAlloc->GetKind() == TypeKind::Array && Alloc->Values[0]->IsNot(AstKind::ARRAY)) {
                    Error(GetExpandedLoc(Alloc->Values[0]), "The array must be created inline");
                }

                if (!IsAssignableTo(Alloc->TypeToAlloc, Alloc->Values[0])) {
                    Error(GetExpandedLoc(Alloc->Values[0]),
                        "Cannot initialize allocation of type '%s' with type '%s'",
                        Alloc->TypeToAlloc,
                        Alloc->Values[0]->Ty);
                } else {
                    CreateCast(Alloc->Values[0], Alloc->TypeToAlloc);
                }
            }
        }
    } else if (TypeToAlloc->GetKind() == TypeKind::Array) {
        ArrayType* ArrayTy = TypeToAlloc->AsArrayTy();
        Type* BaseTy = ArrayTy->GetBaseType();
        if (BaseTy->GetKind() == TypeKind::Struct) {
            StructType* StructTy = BaseTy->AsStructType();
            if (!StructTy->GetStruct()->Constructors.empty() && !StructTy->GetStruct()->DefaultConstructor) {
                Error(Alloc, "Cannot allocate array of structs because there is no default constructor");
            }
        }
    }

    if (TypeToAlloc->GetKind() == TypeKind::Array) {
        Type* ArrBaseTy = TypeToAlloc->AsArrayTy()->GetBaseType();
        if (AssignToType && AssignToType->GetKind() == TypeKind::Pointer) {

            PointerType* PtrTy = AssignToType->AsPointerTy();
            if (PtrTy->GetElementType()->Equals(ArrBaseTy)) {
                if (!Alloc->LeaveUninitialized && ArrBaseTy->TypeNeedsDestruction()) {
                    Error(Alloc,
                        "Cannot allocate memory as type '%s' because the base type '%s' has destructors. Use type '%s' instead",
                        PtrTy, ArrBaseTy, SliceType::Create(ArrBaseTy, Context));
                    YIELD_ERROR(Alloc);
                }

                Alloc->Ty = PointerType::Create(ArrBaseTy, Context);
            } else {
                // Not matching let error reporting deal with mismatch.
                // TODO: This should probably take into account poylmorphism.
                Alloc->Ty = SliceType::Create(ArrBaseTy, Context);
                Alloc->UsesSlice = true;
            }
        } else {
            if (ArrBaseTy->TypeNeedsDestruction() || (AssignToType && AssignToType->GetKind() == TypeKind::Slice)) {
                // Use slice type if it has destructors so that it knows how to delete.
                Alloc->Ty = SliceType::Create(ArrBaseTy, Context);
                Alloc->UsesSlice = true;
            } else {
                Alloc->Ty = PointerType::Create(ArrBaseTy, Context);
            }
        }
    } else {
        Alloc->Ty = PointerType::Create(TypeToAlloc, Context);
    }
}

void arco::SemAnalyzer::CheckSizeOf(SizeOf* SOf) {
    if (!FixupType(SOf->TypeToGetSizeOf)) {
        YIELD_ERROR(SOf);
    }

    // TODO: Does this need to check for incompleteness of enum and interface types as well?
    // what about arrays?
    if (!SOf->TypeToGetSizeOf->TypeHasStorage()) {
        Error(SOf,
            "Cannot get size of type '%s' because it has no size",
            SOf->TypeToGetSizeOf);
    }


    //TypeOrExpr* TOrE = SOf->TOrE;
    //
    //CheckTypeOrExpr(TOrE);
    //YIELD_ERROR_WHEN(SOf, TOrE);
    //
    //if (TOrE->ResolvedType) {
    //    SOf->TypeToGetSizeOf = TOrE->ResolvedType;
    //} else {
    //    if (!TOrE->ResolvedExpr->Ty->TypeHasStorage()) {
    //        Error(TOrE,
    //            "Cannot get size of type '%s' because it has no size",
    //            TOrE->ResolvedExpr->Ty);
    //    }
    //
    //    SOf->TypeToGetSizeOf = TOrE->ResolvedExpr->Ty;
    //}
}

void arco::SemAnalyzer::CheckTypeOf(TypeOf* TOf) {
    if (!Context.StdTypeStruct) {
        Error(TOf, "Cannot use typeof operator when -stand-alone flag is set");
        YIELD_ERROR(TOf);
    }
    if (!Context.StdTypeStruct->HasBeenChecked) {
        SemAnalyzer A(Context, Context.StdTypeStruct);
        A.CheckStructDecl(Context.StdTypeStruct);
    }

    FixupType(TOf->TypeToGetTypeOf);
    TOf->Ty = PointerType::Create(StructType::Create(Context.StdTypeStruct, {}, Context), Context);
    TOf->HasConstAddress = true;
    TOf->IsFoldable = false;
}

void arco::SemAnalyzer::CheckTypeId(TypeId* TId) {
    if (!Context.StdTypeStruct) {
        Error(TId, "Cannot use typeid operator when -stand-alone flag is set");
        YIELD_ERROR(TId);
    }

    FixupType(TId->TypeToGetTypeId);
    if (!Context.StdTypeIdEnum->HasBeenChecked) {
        SemAnalyzer A(Context, Context.StdTypeIdEnum);
        A.CheckEnumDecl(Context.StdTypeIdEnum);
    }

    TId->Ty = StructType::Create(Context.StdTypeIdEnum, Context);
    TId->HasConstAddress = true;
}

void arco::SemAnalyzer::CheckRange(Range* Rg) {

    CheckNode(Rg->LHS);
    CheckNode(Rg->RHS);

    if (Rg->LHS->Ty == Context.ErrorType ||
        Rg->RHS->Ty == Context.ErrorType) {
        YIELD_ERROR(Rg);
    }

    bool Errors = false;
    if (!Rg->LHS->Ty->IsInt() &&
            Rg->LHS->Ty->GetRealKind() != TypeKind::Enum) {
        Error(Rg->LHS, "Expected indexable type for range. Found type '%s'", Rg->LHS->Ty);
        Errors = true;
    }
    if (!Rg->RHS->Ty->IsInt() &&
            Rg->RHS->Ty->GetRealKind() != TypeKind::Enum) {
        Error(Rg->RHS, "Expected indexable type for range. Found type '%s'", Rg->RHS->Ty);
        Errors = true;
    }

    if (Errors) {
        YIELD_ERROR(Rg);
    }

    // TODO: Check if the range doesn't go off into infinitity?

    if (!IsAssignableTo(Rg->RHS->Ty, Rg->LHS)) {
        if (!IsAssignableTo(Rg->LHS->Ty, Rg->RHS)) {
            Error(Rg, "The left hand and right hand types of the range are incompatible");
            YIELD_ERROR(Rg);
        } else {
            CreateCast(Rg->RHS, Rg->LHS->Ty);
            Rg->Ty = Rg->LHS->Ty;
        }
    } else {
        CreateCast(Rg->LHS, Rg->RHS->Ty);
        Rg->Ty = Rg->RHS->Ty;
    }
}

void arco::SemAnalyzer::CheckMoveObj(MoveObj* Move) {
    CheckNode(Move->Value);
    YIELD_ERROR_WHEN(Move, Move->Value);

    Move->IsFoldable = false;
    Move->Ty = Move->Value->Ty;
    if (!IsLValue(Move->Value)) {
        Error(Move, "Expected to be a modifiable value for move");
    } else if (Move->HasConstAddress) {
        Error(Move, "Cannot move constant memory");
    }
}

void arco::SemAnalyzer::CheckInitObj(InitObjStmt* Init) {
    CheckNode(Init->Address);
    CheckNode(Init->Value);
    YIELD_IF_ERROR(Init->Address);
    YIELD_IF_ERROR(Init->Value);

    if (!IsLValue(Init->Address)) {
        Error(Init, "Expected to be a modifiable value for init");
    }
}

void arco::SemAnalyzer::CheckTernary(Ternary* Tern) {
    CheckCondition(Tern->Cond, "Ternary");

    CheckNode(Tern->LHS);
    CheckNode(Tern->RHS);
    if (Tern->LHS->Ty == Context.ErrorType ||
        Tern->RHS->Ty == Context.ErrorType) {
        YIELD_ERROR(Tern);
    }

    // TODO: lossen foldable constraint
    Tern->IsFoldable = false;
    Tern->HasConstAddress = Tern->LHS->HasConstAddress || Tern->RHS->HasConstAddress;

    if (IsAssignableTo(Tern->LHS->Ty, Tern->RHS)) {
        Tern->Ty = Tern->LHS->Ty;
        CreateCast(Tern->RHS, Tern->Ty);
    } else if (IsAssignableTo(Tern->RHS->Ty, Tern->LHS)) {
        Tern->Ty = Tern->RHS->Ty;
        CreateCast(Tern->LHS, Tern->Ty);
    } else {
        Error(Tern, "Incompatible values for ternary expression");
        YIELD_ERROR(Tern);
    }
}

void arco::SemAnalyzer::CheckVarDeclList(VarDeclList* List) {
    bool HasErrors = false;
    for (VarDecl* Var : List->Decls) {
        CheckVarDecl(Var);
        if (Var->Ty == Context.ErrorType) {
            HasErrors = true;
        }
    }
    if (HasErrors) {
        //YIELD_ERROR(List);
    }
}

void arco::SemAnalyzer::CheckCatchError(CatchError* Catch, VarDecl* CaptureVar) {

    CatchingErrors = true;

    Catch->ErrorVar->Ty = Context.ErrorInterfacePtrType;

    CheckNode(Catch->CaughtExpr);
    YIELD_ERROR_WHEN(Catch, Catch->CaughtExpr);
    Catch->Ty = Catch->CaughtExpr->Ty;

    Scope NewScope;
    CheckScopeStmts(Catch->Scope, NewScope);

    if (!NewScope.AllPathsBranch) {
        if (CaptureVar) {
            Error(CaptureVar, "Cannot assign to variable from caught expression because not all caught paths branch");
        }
    }

    CatchingErrors = false;

}

void arco::SemAnalyzer::CheckTypeOrExpr(TypeOrExpr* TOrE) {
    if (TOrE->ResolvedType) {
        if (!FixupType(TOrE->ResolvedType)) {
            TOrE->Ty = Context.ErrorType;
        }
        return;
    } else if (TOrE->ResolvedExpr) {
        CheckNode(TOrE->ResolvedExpr);
        YIELD_ERROR_WHEN(TOrE, TOrE->ResolvedExpr);
        return;
    }

    CheckIdentRef<false>(TOrE->AmbIdentRef, false, nullptr);
    YIELD_ERROR_WHEN(TOrE, TOrE->AmbIdentRef);

    if (TOrE->AmbIdentRef->RefKind == IdentRef::RK::Struct) {
        // NOTE: If AmbTyOfE was a type it would just resolve to a type during
        // parsing.

        StructType* StructTy = StructType::Create(TOrE->AmbIdentRef->Struct, {}, Context);
        if (!FixupType(StructTy)) {
            YIELD_ERROR(TOrE);
        }

        // It is an array of struct types.

        TOrE->ResolvedType = ArrayType::Create(StructTy,
                                               TOrE->AmbTyOfE->ResolvedExpr,
                                               false,
                                               Context);
        if (!FixupType(TOrE->ResolvedType)) {
            YIELD_ERROR(TOrE);
        }
    } else {

    }
}

void arco::SemAnalyzer::CheckCondition(Expr* Cond, const char* PreErrorText) {
    CheckNode(Cond);
    if (Cond->Ty == Context.ErrorType) return;
    if (!IsComparable(Cond->Ty)) {
        Error(Cond, "%s condition expected to be type 'bool' but found type '%s'",
             PreErrorText, Cond->Ty);
    }
}

void arco::SemAnalyzer::CreateCast(Expr* E, Type* ToType) {
    if (E->Ty->Equals(ToType)) return;
    E->CastTy = ToType;
}

bool arco::SemAnalyzer::IsAssignableTo(Type* ToTy, Expr* FromExpr) {
    return IsAssignableTo(ToTy, FromExpr->Ty, FromExpr);
}

bool arco::SemAnalyzer::IsAssignableTo(Type* ToTy, Type* FromTy, Expr* FromExpr) {
    if (FromTy->UnboxGeneric()->GetRealKind() == TypeKind::Enum ||
        ToTy->UnboxGeneric()->GetRealKind() == TypeKind::Enum) {

        if (ToTy->UnboxGeneric()->GetRealKind()== TypeKind::Enum) {
            return ToTy->Equals(FromTy);
        }

        // Enums have special rules for assignments since they
        // can assign to whatever their index type is.
        EnumDecl* Enum = static_cast<StructType*>(FromTy->UnboxGeneric())->GetEnum();
        return IsAssignableTo(ToTy, Enum->ValuesType, nullptr);
    }

    switch (ToTy->GetKind()) {
    case TypeKind::Int8:
    case TypeKind::Int16:
    case TypeKind::Int32:
    case TypeKind::Int64:
    case TypeKind::UInt8:
    case TypeKind::UInt16:
    case TypeKind::UInt32:
    case TypeKind::UInt64:
    case TypeKind::Char: {
        if (FromTy->IsInt()) {
            if (ToTy->GetSizeInBytes(Context.LLArcoModule) >= FromTy->GetSizeInBytes(Context.LLArcoModule)) {
                // Destination has enough capacity to store the integer.
                return true;
            } else if (FromExpr && FromExpr->Is(AstKind::NUMBER_LITERAL)) {
                // If the FromExpr is a basic number literal
                // then it will be allowed as long as it's value
                // would not result in a loss of data

                NumberLiteral* Num = static_cast<NumberLiteral*>(FromExpr);

#define RANGE(ty, v)     v >= std::numeric_limits<ty>::min() && v <= std::numeric_limits<ty>::max();
#define POS_RANGE(ty, v) v >= 0 && v <= std::numeric_limits<ty>::max();

                if (Num->Ty->IsSigned()) {
                    switch (ToTy->GetKind()) {
                    case TypeKind::Int8:   return RANGE(i8, Num->SignedIntValue);
                    case TypeKind::Int16:  return RANGE(i16, Num->SignedIntValue);
                    case TypeKind::Int32:  return RANGE(i32, Num->SignedIntValue);
                    case TypeKind::Int64:  return RANGE(i64, Num->SignedIntValue);
                    case TypeKind::UInt8:  return POS_RANGE(u8, Num->SignedIntValue);
                    case TypeKind::UInt16: return POS_RANGE(u16, Num->SignedIntValue);
                    case TypeKind::UInt32: return POS_RANGE(u32, Num->SignedIntValue);
                    case TypeKind::UInt64: return POS_RANGE(u64, Num->SignedIntValue);
                    case TypeKind::Char:   return RANGE(i8, Num->SignedIntValue);
                    }
                } else {
                    switch (ToTy->GetKind()) {
                    case TypeKind::Int8:   return RANGE(i8, Num->UnsignedIntValue);
                    case TypeKind::Int16:  return RANGE(i16, Num->UnsignedIntValue);
                    case TypeKind::Int32:  return RANGE(i32, Num->UnsignedIntValue);
                    case TypeKind::Int64:  return RANGE(i64, Num->UnsignedIntValue);
                    case TypeKind::UInt8:  return POS_RANGE(u8, Num->UnsignedIntValue);
                    case TypeKind::UInt16: return POS_RANGE(u16, Num->UnsignedIntValue);
                    case TypeKind::UInt32: return POS_RANGE(u32, Num->UnsignedIntValue);
                    case TypeKind::UInt64: return POS_RANGE(u64, Num->UnsignedIntValue);
                    case TypeKind::Char:   return RANGE(i8, Num->UnsignedIntValue);
                    }
                }
            }
            return false;
        } else if (FromTy->IsFloat() && FromExpr && FromExpr->Is(AstKind::NUMBER_LITERAL)) {
            NumberLiteral* Num = static_cast<NumberLiteral*>(FromExpr);

            i64 AsInt;
            if (FromTy->GetKind() == TypeKind::Float32) {
                float Value = Num->Float32Value;
                AsInt = Value;
                if (Value - AsInt > 0) {
                    return false;
                }
            } else {
                double Value = Num->Float64Value;
                AsInt = Value;
                if (Value - AsInt > 0) {
                    return false;
                }
            }
            switch (ToTy->GetKind()) {
            case TypeKind::Int8:   return RANGE(i8, AsInt);
            case TypeKind::Int16:  return RANGE(i16, AsInt);
            case TypeKind::Int32:  return RANGE(i32, AsInt);
            case TypeKind::Int64:  return RANGE(i64, AsInt);
            case TypeKind::UInt8:  return POS_RANGE(u8, AsInt);
            case TypeKind::UInt16: return POS_RANGE(u16, AsInt);
            case TypeKind::UInt32: return POS_RANGE(u32, AsInt);
            case TypeKind::UInt64: return POS_RANGE(u64, AsInt);
            case TypeKind::Char:   return RANGE(i8, AsInt);
            }
        }
        return false;
    }
    case TypeKind::Ptrsize:
        return FromTy->Equals(Context.PtrsizeType);
    case TypeKind::Int: {
        if (FromTy->IsSystemInt()) return true;
        if (FromTy->IsInt()) return FromTy->GetTrivialTypeSizeInBytes() <= 4;
        if (FromTy->IsFloat() && FromExpr && FromExpr->Is(AstKind::NUMBER_LITERAL)) {
            NumberLiteral* Num = static_cast<NumberLiteral*>(FromExpr);

            i64 AsInt;
            if (FromTy->GetKind() == TypeKind::Float32) {
                float Value = Num->Float32Value;
                AsInt = Value;
                if (Value - AsInt > 0) {
                    return false;
                }
            } else {
                double Value = Num->Float64Value;
                AsInt = Value;
                if (Value - AsInt > 0) {
                    return false;
                }
            }
            if (ToTy->GetKind() == TypeKind::Int) {
                return RANGE(i32, AsInt);
            } else {
                return POS_RANGE(u32, AsInt);
            }
        }
        return false;
    }
#undef RANGE
#undef POS_RANGE
    case TypeKind::Float32:
    case TypeKind::Float64:
        if (FromTy->IsInt()) {
            return true;
        } else {
            return FromTy->Equals(ToTy);
        }
    case TypeKind::CStr: {
        if (FromTy == Context.NullType)
            return true;
        else if (FromTy->GetKind() == TypeKind::Array) {
            ArrayType* FromArrayTy = FromTy->AsArrayTy();
            return FromArrayTy->GetElementType() == Context.CharType;
        }
        return FromTy->Equals(Context.CStrType) || FromTy->Equals(Context.CharPtrType);
    }
    case TypeKind::Pointer: {
        if (FromTy == Context.NullType)
            return true;
        else if (FromTy->GetKind() == TypeKind::Array) {
            if (ToTy->Equals(Context.VoidPtrType)) {
                // Even multi-dimensional arrays are assignable to void*.
                return true;
            }

            PointerType* ToPtrTy     = ToTy->AsPointerTy();
            ArrayType*   FromArrayTy = FromTy->AsArrayTy();
            if (FromArrayTy->GetDepthLevel() == 1) {
                return ToPtrTy->GetElementType()->Equals(FromArrayTy->GetElementType()) ||
                       ToPtrTy->Equals(Context.VoidPtrType);
            }
            return false;
        } else if (FromTy->IsPointer()) {
            PointerType* ToPtrTy = ToTy->AsPointerTy();
            if (ToTy->Equals(Context.VoidPtrType)) {
                return true; // Can always assign to void*
            } else if (ToPtrTy->GetElementType()->GetKind() == TypeKind::Interface) {
                // InterfaceType*
                Type* FromPtrElmTy = FromTy->GetPointerElementType(Context);
                Type* ToPtrElmTy = ToTy->GetPointerElementType(Context);
                if (FromPtrElmTy->GetKind() == TypeKind::Struct) {
                    StructDecl* Struct = FromPtrElmTy->AsStructType()->GetStruct();
                    if (!Struct->Interfaces.empty()) {
                        InterfaceDecl* Interface = ToPtrElmTy->AsStructType()->GetInterface();
                        auto Itr = std::find_if(Struct->Interfaces.begin(), Struct->Interfaces.end(),
                            [Interface](const InterfaceDecl* StructInterface) {
                                return StructInterface == Interface;
                            });
                        return Itr != Struct->Interfaces.end();
                    } else {
                        return false;
                    }
                }

                if (FromPtrElmTy->Equals(ToPtrElmTy)) {
                    return true;
                }
                return false;
            } else {
                return ToTy->Equals(FromTy);
            }
        } else {
            return false;
        }
    }
    case TypeKind::Slice: {
        if (FromTy->GetKind() == TypeKind::Array) {
            return ToTy->AsSliceTy()->GetElementType()->Equals(
                FromTy->AsArrayTy()->GetElementType());
        }

        return ToTy->Equals(FromTy);
    }
    case TypeKind::Array: {
        if (FromTy->GetKind() != TypeKind::Array) {
            return false;
        }

        ArrayType* ToArrayType   = ToTy->AsArrayTy();
        ArrayType* FromArrayType = FromTy->AsArrayTy();
        ulen ToArrayDepth = ToArrayType->GetDepthLevel();
        if (ToArrayDepth != FromArrayType->GetDepthLevel()) {
            return false;
        }

        if (FromExpr && FromExpr->Is(AstKind::ARRAY)) {
            // When creating an array the array that is created
            // may take on the length its destination.
            //
            // Making sure that the length of the destination
            // is the same or bigger than the length of the
            // source.
            if (ToArrayType->GetLength() < FromArrayType->GetLength()) {
                return false;
            }
            for (ulen i = 1; i < ToArrayDepth; i++) {
                ToArrayType   = ToTy->AsArrayTy();
                FromArrayType = FromTy->AsArrayTy();
                if (ToArrayType->GetLength() < FromArrayType->GetLength()) {
                    return false;
                }
            }
        } else {
            if (ToArrayType->GetLength() != FromArrayType->GetLength()) {
                return false;
            }
        }

        if (FromArrayType->GetBaseType() == Context.EmptyArrayElmType) {
            return true;
        }

        return IsAssignableTo(
            ToArrayType->GetBaseType(),
            FromArrayType->GetBaseType(),
            nullptr
        );
    }
    case TypeKind::Struct: {
        if (!Context.StandAlone) {
            StructDecl* Struct = ToTy->AsStructType()->GetStruct();
            if (Struct == Context.StdAnyStruct) {
                if (!FromTy->TypeHasStorage()) {
                    return false;
                }
                return true;
            }
            if (Struct == Context.StdStringStruct) {
                if (FromTy->GetKind() == TypeKind::CStr) {
                    return true;
                }
            }
        }

        return ToTy->Equals(FromTy);
    }
    default:
        return ToTy->Equals(FromTy);
    }
}

bool arco::SemAnalyzer::IsCastableTo(Type* ToTy, Type* FromTy) {
    switch (ToTy->GetKind()) {
    case TypeKind::Int8:
    case TypeKind::Int16:
    case TypeKind::Int32:
    case TypeKind::Int64:
    case TypeKind::UInt8:
    case TypeKind::UInt16:
    case TypeKind::UInt32:
    case TypeKind::UInt64:
    case TypeKind::Int:
    case TypeKind::Ptrsize:
    case TypeKind::Char:
        if (FromTy->IsNumber() || FromTy->IsQualifiedPointer() || FromTy->GetKind() == TypeKind::Bool) {
            // Allow pointers/numbers/bools to cast to integers.
            return true;
        }
        return false;
    case TypeKind::Float32:
    case TypeKind::Float64:
        return FromTy->IsNumber();
    case TypeKind::Pointer:
    case TypeKind::CStr:
        if (FromTy->IsNumber() || FromTy->IsQualifiedPointer()) {
            // Allow numbers/pointers to cast to pointers.
            return true;
        }
        return IsAssignableTo(ToTy, FromTy, nullptr);
    case TypeKind::Struct:
        // Prevent any craziness that might come with casting to Any
        return false;
        return false;
    case TypeKind::Bool:
        if (FromTy->IsNumber() || FromTy->IsQualifiedPointer() ||
            FromTy->GetKind() == TypeKind::Bool) {
            return true;
        }
        return false;
    default:
        return IsAssignableTo(ToTy, FromTy, nullptr);
    }
}

bool arco::SemAnalyzer::ViolatesConstAssignment(VarDecl* DestVar, Expr* Assignment) {
    return ViolatesConstAssignment(DestVar->Ty, DestVar->HasConstAddress, Assignment);
}

bool arco::SemAnalyzer::ViolatesConstAssignment(Type* DestTy, bool DestConstAddress, Expr* Assignment) {
    TypeKind DestTyKind = DestTy->UnboxGeneric()->GetRealKind();
    return !DestConstAddress           &&
           Assignment->HasConstAddress &&
           (DestTyKind == TypeKind::Pointer || DestTyKind == TypeKind::CStr ||
            DestTyKind == TypeKind::Array   || DestTyKind == TypeKind::Function);
}

bool arco::SemAnalyzer::FixupType(Type* Ty, bool AllowDynamicArrays, bool PartialGenFixup) {
    if (Ty->ContainsGenerics && !PartialGenFixup) {

        if (NotFullyQualifiedVarState) {
            Error(CVar->Loc, "Cannot use generic types when using type inference since the generic type is not qualified");
            return false;
        }

        // If the type contains generic information and we get to this point then
        // the type should have already been qualified which means there is no reason
        // to fix the type since all relevent information should have already been fixed.
        return true;
    }

    TypeKind Kind = Ty->GetRealKind();

    if (Kind == TypeKind::Array) {
        return FixupArrayType(static_cast<ArrayType*>(Ty), PartialGenFixup);
    } else if (Kind == TypeKind::Struct || Kind == TypeKind::Interface || Kind == TypeKind::Enum) {
        return FixupStructType(static_cast<StructType*>(Ty), PartialGenFixup);
    } else if (Kind == TypeKind::Pointer) {
        PointerType* PointerTy = static_cast<PointerType*>(Ty);
        Type* ElementType = PointerTy->GetElementType();
        if (!FixupType(ElementType, false, PartialGenFixup)) {
            return false;
        }

        if (!PointerTy->GetUniqueId()) {
            // Could not resolve the unique id during parsing so we must
            // create the unique id now.

            u32 UniqueKey = ElementType->GetUniqueId();
            auto Itr = Context.PointerTyCache.find(UniqueKey);
            if (Itr != Context.PointerTyCache.end()) {
                PointerTy->SetUniqueId(Itr->second->GetUniqueId());
            } else {
                // First seen version of this type.
                PointerTy->SetUniqueId(Context.UniqueTypeIdCounter++);
                Context.PointerTyCache.insert({ UniqueKey, PointerTy });
            }
        }

        return true;
    } else if (Kind == TypeKind::Slice) {
        SliceType* SliceTy = static_cast<SliceType*>(Ty);
        Type* ElementType = SliceTy->GetElementType();
        if (!FixupType(ElementType, false, PartialGenFixup)) {
            return false;
        }

        if (!SliceTy->GetUniqueId()) {
            // Could not resolve the unique id during parsing so we must
            // create the unique id now.

            u32 UniqueKey = ElementType->GetUniqueId();
            auto Itr = Context.SliceTyCache.find(UniqueKey);
            if (Itr != Context.SliceTyCache.end()) {
                SliceTy->SetUniqueId(Itr->second->GetUniqueId());
            } else {
                // First seen version of this type.
                SliceTy->SetUniqueId(Context.UniqueTypeIdCounter++);
                Context.SliceTyCache.insert({ UniqueKey, SliceTy });
            }
        }

        return true;
    } else if (Kind == TypeKind::Function) {
        FunctionType* FuncTy = static_cast<FunctionType*>(Ty);
        for (auto ParamTy : FuncTy->ParamTypes) {
            if (!FixupType(ParamTy.Ty, false, PartialGenFixup)) {
                return false;
            }
        }
        llvm::SmallVector<u32> UniqueKey = FunctionType::GetUniqueHashKey(FuncTy->RetTyInfo, FuncTy->ParamTypes);
        auto Itr = Context.FunctionTyCache.find(UniqueKey);
        if (Itr != Context.FunctionTyCache.end()) {
            FuncTy->SetUniqueId(Itr->second->GetUniqueId());
        } else {
            // First seen version of this type.
            FuncTy->SetUniqueId(Context.UniqueTypeIdCounter++);
            Context.FunctionTyCache.insert({ UniqueKey, FuncTy });
        }
        return true;
    }
    return true;
}

bool arco::SemAnalyzer::FixupArrayType(ArrayType* ArrayTy, bool PartialGenFixup, bool FixupElmTy) {
    Expr* LengthExpr = ArrayTy->GetLengthExpr();
    if (!LengthExpr) {
        // If the length expression is nullptr this means that the
        // length is determined by the assignment of the variable.
        return true;
    }

    CheckNode(LengthExpr);
    if (LengthExpr->Ty == Context.ErrorType) {
        return false;
    }

    if (!LengthExpr) return true;

    bool AllowDynamic = ArrayTy->AllowsForDynamic();

    if (!LengthExpr->IsFoldable && !AllowDynamic) {
        Error(GetExpandedLoc(LengthExpr), "Could not compute the length of the array at compile time");
        return false;
    } else if (!LengthExpr->Ty->IsInt()) {
        Error(GetExpandedLoc(LengthExpr), "The length of the array is expected to be an integer");
        return false;
    }

    if (LengthExpr->IsFoldable) {
        llvm::Value* LLValue = GenFoldable(GetExpandedLoc(LengthExpr), LengthExpr);
        if (!LLValue) return false;

        llvm::ConstantInt* LLInt = llvm::cast<llvm::ConstantInt>(LLValue);

        if (LLInt->isZero()) {
            Error(GetExpandedLoc(LengthExpr), "The length of the array cannot be zero");
            return false;
        } else if (LengthExpr->Ty->IsSigned() && LLInt->isNegative()) {
            Error(GetExpandedLoc(LengthExpr), "The length of the array cannot be negative");
            return false;
        }

        ArrayTy->AssignLength(LLInt->getZExtValue());
    }

    // NOTE: At this point the element type should have it's uniqueTypeId
    // correctly set.
    Type* ElementType = ArrayTy->GetElementType();
    if (FixupElmTy) {
        if (!FixupType(ElementType, AllowDynamic, PartialGenFixup)) {
            return false;
        }
    }

    // TODO: we could pass the type by reference and delete the
    // non-unique array.
    std::pair<u32, ulen> UniqueKey = { ElementType->GetUniqueId(), ArrayTy->GetLength() };
    auto Itr = Context.ArrayTyCache.find(UniqueKey);
    if (Itr != Context.ArrayTyCache.end()) {
        // an exact version of this array type already exists.
        ArrayTy->SetUniqueId(Itr->second->GetUniqueId());
    } else {
        ArrayTy->SetUniqueId(Context.UniqueTypeIdCounter++);
        Context.ArrayTyCache.insert({ UniqueKey, ArrayTy });
    }

    return true;
}

bool arco::SemAnalyzer::FixupStructType(StructType* StructTy, bool PartialGenFixup) {
    Identifier StructName = StructTy->GetStructName();
    Decl* FoundDecl = FindStructLikeTypeByName(StructName);

    if (!FoundDecl) {
        Error(StructTy->GetErrorLoc(), "Could not find struct, or enum by name '%s'", StructTy->GetStructName());
        return false;
    }

    if (FoundDecl->Is(AstKind::STRUCT_DECL)) {
        StructDecl* Struct = static_cast<StructDecl*>(FoundDecl);

        StructTy->AssignStruct(Struct);

        // TODO: Fix this needs to store the found bindings within the
        // qualified version!
        if (Struct->IsGeneric()) {

            const BindableList& BindableTypes = StructTy->GetBindTypes();
            for (Type* BindType : BindableTypes) {
                if (!FixupType(BindType, false, PartialGenFixup)) {
                    return false;
                }
            }

            // TODO: This displays the wrong error location!
            if (BindableTypes.empty()) {
                Error(StructTy->GetErrorLoc(), "Missing type bindings for generic struct");
                return false;
            } else if (BindableTypes.size() != Struct->GenData->GenTys.size()) {
                Error(StructTy->GetErrorLoc(),
                    "Incorrect number of type bindings for generic struct. Expected %s. Got %s",
                    Struct->GenData->GenTys.size(), StructTy->GetBindTypes().size());
                return false;
            }

            if (PartialGenFixup && StructTy->ContainsGenerics) {
                // Don't know the generic information yet.
                return true;
            }

            FinishGenericStructType(StructTy);

        } else if (!StructTy->GetBindTypes().empty()) {
            Error(StructTy->GetErrorLoc(),
                  "The struct type '%s' does not have generics",
                  StructTy->GetStructName());
            return false;
        } else {
            FinishNonGenericStructType(Context, StructTy);
        }
    } else if (FoundDecl->Is(AstKind::ENUM_DECL)) {
        EnumDecl* Enum = static_cast<EnumDecl*>(FoundDecl);
        StructTy->AssignEnum(Enum);
        StructTy->SetUniqueId(Enum->UniqueTypeId);
        SemAnalyzer Analyzer(Context, FoundDecl);
        Analyzer.CheckEnumDecl(Enum);
    } else if (FoundDecl->Is(AstKind::INTERFACE_DECL)) {
        InterfaceDecl* Interface = static_cast<InterfaceDecl*>(FoundDecl);
        StructTy->AssignInterface(Interface);
        StructTy->SetUniqueId(Interface->UniqueTypeId);
        SemAnalyzer Analyzer(Context, FoundDecl);
        Analyzer.CheckInterfaceDecl(Interface);
    } else {
        assert(!"Not handled");
    }

    return true;
}

void arco::SemAnalyzer::FinishNonGenericStructType(ArcoContext& Context, StructType* StructTy) {
    StructDecl* Struct = StructTy->GetStruct();

    StructTy->SetUniqueId(Struct->UniqueTypeId);
    llvm::StructType* LLStructTy = nullptr;
    if (!Struct->LLStructTy) {
        LLStructTy = GenForwardDeclStructType(Context);
        Struct->LLStructTy = LLStructTy;
        StructTy->LLStructType = LLStructTy;
    } else {
        StructTy->LLStructType = Struct->LLStructTy;
    }
    Struct->StructTy = StructTy;
    SemAnalyzer Analyzer(Context, Struct);
    Analyzer.CheckStructDecl(Struct);
    if (!FoundCompileError && !Context.CheckingUnhcecked) {
        // NOTE: see comment above for generic binding case.
        if (LLStructTy) {
            GenStructType(Context, StructTy, LLStructTy);

            IRGenerator IRGen(Context);
            if (StructTy->DoesNeedsDestruction() && !Struct->Destructor) {
                IRGen.GenImplicitDestructor(StructTy);
            }

            if (StructTy->DoesFieldsHaveAssignment() && !Struct->DefaultConstructor) {
                IRGen.GenImplicitDefaultConstructor(StructTy);
            }

            if (StructTy->DoesNeedsMove() && !Struct->MoveConstructor) {
                IRGen.GenImplicitMoveConstructor(StructTy);
            }

            if (StructTy->DoesNeedsCopy() && !Struct->CopyConstructor) {
                IRGen.GenImplicitCopyConstructor(StructTy);
            }
        }
    }
}

void arco::SemAnalyzer::FinishGenericStructType(StructType* StructTy) {

    StructDecl* Struct = StructTy->GetStruct();
    const BindableList& BindableTypes = StructTy->GetBindTypes();

    GenericBinding* Binding = GetExistingBinding(Struct, BindableTypes, nullptr);
    if (!Binding) {
        Binding = CreateNewBinding(Struct, BindableTypes, nullptr);
        Binding->StructInfo = new GenericStructInfo; // TODO: Use of new
        Binding->StructInfo->UniqueTypeId = Context.UniqueTypeIdCounter++;
        Binding->StructInfo->QualStructTy = StructTy; // At this point the struct type must be qualified.
        StructTy->FoundBinding = Binding;
        StructTy->SetUniqueId(Binding->StructInfo->UniqueTypeId);

        llvm::SmallVector<Type*> PrevQualTypes;
        if (!Struct->GenData->BindingStack.empty()) {
            PrevQualTypes = Struct->GenData->TypesNeedingQualification;
        }
        BindTypes(Struct, Binding);
        if (!Context.CheckingUnhcecked) {
            llvm::StructType* LLStructType = GenForwardDeclStructType(Context);
            Binding->StructInfo->LLStructType = LLStructType;
        }

        SemAnalyzer Analyzer(Context, Struct);
        Analyzer.CheckStructDecl(Struct);

        if (!FoundCompileError && !Context.CheckingUnhcecked) {
            // Creating the llvm struct type here because generating on demand causes
            // complications with generics due to needing the qualification information.
            GenStructType(Context, StructTy, Binding->StructInfo->LLStructType);

            // TODO: This will not need to be ran if there are no constructors!
            IRGenerator IRGen(Context);
            IRGen.GenGenericStructConstructorSharedInstructions(StructTy);

            if (StructTy->DoesNeedsDestruction() && !Struct->Destructor) {
                IRGen.GenImplicitDestructor(StructTy);
            }

            if (StructTy->DoesFieldsHaveAssignment() && !Struct->DefaultConstructor) {
                IRGen.GenImplicitDefaultConstructor(StructTy);
            }
        }

        if (!PrevQualTypes.empty()) {
            Struct->GenData->TypesNeedingQualification = PrevQualTypes;
        }

        UnbindTypes(Struct);
    } else {
        // else already checked.
        StructTy->SetUniqueId(Binding->StructInfo->UniqueTypeId);
        StructTy->FoundBinding = Binding;
        StructTy->LLStructType = Binding->StructInfo->LLStructType;
    }
}

arco::Decl* arco::SemAnalyzer::FindStructLikeTypeByName(Identifier Name) {

    Decl* FoundDecl = nullptr;

    auto Itr = FScope->Imports.find(Name);
    if (Itr != FScope->Imports.end() && Itr->second.Decl) {
        FoundDecl = Itr->second.Decl;
    }

    if (!FoundDecl) {
        if (Decl* Dec = FScope->FindDecl(Name)) {
            if (Dec->IsStructLike()) {
                FoundDecl = Dec;
            }
        }
    }

    if (!FoundDecl) {
        auto Itr = Mod->DefaultNamespace->Decls.find(Name);
        if (Itr != Mod->DefaultNamespace->Decls.end() && Itr->second->IsStructLike()) {
            FoundDecl = Itr->second;
        }
    }

    if (!FoundDecl && FScope->UniqueNSpace) {
        auto Itr = FScope->UniqueNSpace->Decls.find(Name);
        if (Itr != FScope->UniqueNSpace->Decls.end() && Itr->second->IsStructLike()) {
            FoundDecl = Itr->second;
        }
    }

    return FoundDecl;
}

arco::Type* arco::SemAnalyzer::QualifyType(Type* Ty, const GenericBinding* Binding) {
    switch (Ty->GetRealKind()) {
    case TypeKind::Generic: {
        GenericType* GenTy = static_cast<GenericType*>(Ty);
        Decl* BoundToDec = GenTy->GetBoundToDecl();
        while (Binding->Dec != BoundToDec) {
            Binding = Binding->ParentBinding;
        }

        return GenTy->GetBoundTy();
    }
    case TypeKind::Pointer: {
        PointerType* PtrTy = static_cast<PointerType*>(Ty);
        Type* QualElmTy = QualifyType(PtrTy->GetElementType(), Binding);
        return PointerType::Create(QualElmTy, Context);
    }
    case TypeKind::Slice: {
        SliceType* SliceTy = static_cast<SliceType*>(Ty);
        Type* QualElmTy = QualifyType(SliceTy->GetElementType(), Binding);
        return SliceType::Create(QualElmTy, Context);
    }
    case TypeKind::Array: {
        ArrayType* ArrTy = static_cast<ArrayType*>(Ty);
        Type* QualElmTy = QualifyType(ArrTy->GetElementType(), Binding);

        ArrayType* QualArrTy = ArrayType::Create(QualElmTy,
                                                 ArrTy->GetLengthExpr(),
                                                 ArrTy->AllowsForDynamic(),
                                                 Context);
        FixupArrayType(QualArrTy, false, false);
        return QualArrTy;
    }
    case TypeKind::Function: {
        FunctionType* FuncTy = static_cast<FunctionType*>(Ty);
        Type* RetTy = FuncTy->RetTyInfo.Ty;
        Type* QualRetTy = RetTy->ContainsGenerics ? QualifyType(RetTy, Binding)
                                                  : RetTy;

        llvm::SmallVector<TypeInfo> ParamTypes;
        ParamTypes.reserve(FuncTy->ParamTypes.size());
        for (const TypeInfo& PInfo : FuncTy->ParamTypes) {
            Type* QualParamTy = PInfo.Ty->ContainsGenerics ? QualifyType(PInfo.Ty, Binding)
                                                           : PInfo.Ty;
            ParamTypes.push_back(TypeInfo{
                QualParamTy,
                PInfo.ConstMemory
                });
        }

        return FunctionType::Create(
            TypeInfo{ QualRetTy, FuncTy->RetTyInfo.ConstMemory },
            std::move(ParamTypes),
            Context);
    }
    case TypeKind::Struct: {
        StructType* StructTy = static_cast<StructType*>(Ty);

        llvm::SmallVector<Type*, 8> QualifiedBindTypes;
        for (Type* BindType : StructTy->GetBindTypes()) {
            if (BindType->ContainsGenerics) {
                QualifiedBindTypes.push_back(QualifyType(BindType, Binding));
            } else {
                QualifiedBindTypes.push_back(BindType);
            }
        }

        if (!StructTy->GetStruct()) {
            FixupStructType(StructTy, true);
        }

        StructType* QualStructTy = StructType::Create(StructTy->GetStruct(), QualifiedBindTypes, Context);
        FinishGenericStructType(QualStructTy);
        return QualStructTy;
    }
    default:
        assert(!"unreachable");
        return nullptr;
    }
}

void arco::SemAnalyzer::CheckModifibility(Expr* LValue) {
    if (!IsLValue(LValue)) {
        Error(LValue, "Expected to be a modifiable value");
    } else {
        if (LValue->HasConstAddress) {
            TypeKind K = LValue->Ty->GetKind();
            bool HasIndirectMemory = K == TypeKind::Pointer ||
                                     K == TypeKind::CStr    ||
                                     K == TypeKind::Array   ||
                                     K == TypeKind::Slice;

            if (!HasIndirectMemory) {
                if (LValue->Is(AstKind::FIELD_ACCESSOR)) {
                    Error(LValue, "Cannot modify field with const memory");
                } else {
                    Error(LValue, "Cannot modify a variable with const memory");
                }
            }
        }
    }
}

bool arco::SemAnalyzer::IsLValue(Expr* E) {
    if (E->Is(AstKind::IDENT_REF) || E->Is(AstKind::FIELD_ACCESSOR)) {
        return static_cast<IdentRef*>(E)->RefKind == IdentRef::RK::Var;
    }
    if (E->Is(AstKind::UNARY_OP)) {
        UnaryOp* UOP = static_cast<UnaryOp*>(E);
        return UOP->Op == '*';
    }
    if (E->Is(AstKind::ARRAY_ACCESS)) {
        return true;
    }
    return false;
}

void arco::SemAnalyzer::EnsureChecked(SourceLoc ErrLoc, VarDecl* Var) {

    if (CGlobal) {
        if (Var->IsGlobal && Var->IsBeingChecked) {
            DisplayCircularDepError(ErrLoc, Var, "Global variables form a circular dependency");
        }
        CGlobal->DepD = Var;
    } else if (CField) {
        if (Var->IsField() && Var->IsBeingChecked) {
            DisplayCircularDepError(ErrLoc, Var, "Fields form a circular dependency");
        }
        CField->DepD = Var;
    }


    SemAnalyzer Analyzer(Context, Var);
    Analyzer.CheckVarDecl(Var);

}

void arco::SemAnalyzer::DisplayCircularDepError(SourceLoc ErrLoc, VarDecl* StartDep, const char* ErrHeader) {
    Log.BeginError(ErrLoc, ErrHeader);
    llvm::SmallVector<Decl*> DepOrder;
    ulen LongestIdentLen = 0;
    VarDecl* DepD = StartDep;
    while (DepD) {
        if (DepD->Name.Text.size() > LongestIdentLen) {
            LongestIdentLen = DepD->Name.Text.size();
        }
        if (std::find(DepOrder.begin(), DepOrder.end(), DepD) != DepOrder.end()) {
            // TODO: Loops back on itself?
            break;
        }
        DepOrder.push_back(DepD);
        DepD = DepD->DepD;
    }
    Log.AddNoteLine([](llvm::raw_ostream& OS) {
        OS << "Dependency graph:";
    });
    auto Itr = DepOrder.begin();
    while (Itr != DepOrder.end()) {
        Decl* DepRHS = nullptr;
        Decl* DepLHS = *Itr;
        if ((Itr + 1) != DepOrder.end()) {
            DepRHS = *(Itr + 1);
        } else {
            DepRHS = StartDep;
        }
        Log.AddNoteLine([=](llvm::raw_ostream& OS) {
            std::string LPad = std::string(LongestIdentLen - DepLHS->Name.Text.size(), ' ');
            std::string RPad = std::string(LongestIdentLen - DepRHS->Name.Text.size(), ' ');

            OS << "'" << DepLHS->Name << "'" << LPad << " deps-on ";
            OS << "'" << DepRHS->Name << "'" << RPad << "  At: ";
            OS << DepLHS->FScope->Path << ".arco:" << DepLHS->Loc.LineNumber;
        });
        ++Itr;
    }

    Log.EndError();
}

void arco::SemAnalyzer::CheckForDuplicateFuncs(const FuncsList& FuncList) {
    // TODO: can this use +1 offset for performance?

    // TODO: Should probably check for ambiguous functions with default arguments.
    // EX.
    //
    // fn foo(a int) {}
    // fn foo(a int, b := 4) {}
    //
    // TODO: Alternative optimization: mark functions which already stated that they
    // are a duplicate and then just skip them in the future.

    // TODO: Deal with checking for duplicate generic functions somehow.
    for (const FuncDecl* Func1 : FuncList) {
        if (Func1->IsGeneric()) continue;
        bool IsPrivate = Func1->Mods & ModKinds::PRIVATE;

        for (const FuncDecl* Func2 : FuncList) {
            if (Func2->IsGeneric()) continue;
            if (IsPrivate) {
                // Only care about conflicts if they belong to the same file.
                if (Func2->FScope != Func1->FScope) continue;
            } else if (Func2->Mods & ModKinds::PRIVATE) {
                continue;
            }

            if (Func1 == Func2) continue;
            if (Func1->Name != Func2->Name) continue;
            if (Func1->Params.size() != Func2->Params.size()) continue;
            if (std::equal(Func1->Params.begin(),
                            Func1->Params.end(),
                            Func2->Params.begin(),
                            [](const VarDecl* Param1, const VarDecl* Param2) {
                return Param1->Ty->Equals(Param2->Ty);
            })) {
                FileScope* FScope = Func1->FScope;
                Logger Log(FScope, FScope->Buffer);
                Log.BeginError(Func1->Loc,
                    "Duplicate declaration of %s '%s'. First declared at: %s:%s",
                    Func1->IsConstructor ? "constructor" : "function",
                    Func1->Name,
                    Func2->FScope->Path,
                    Func2->Loc.LineNumber);
                Log.EndError();
            }
        }
    }
}

bool arco::SemAnalyzer::IsComparable(Type* Ty) {
    return Ty->Equals(Context.BoolType) || Ty->IsPointer();
}

void arco::SemAnalyzer::DisplayNoteInfoForTypeMismatch(Expr* FromExpr, Type* ToTy) {
    if (FromExpr->Ty->Equals(Context.FuncRefType) && ToTy->GetKind() == TypeKind::Function) {
        Log.AddNoteLine([=](llvm::raw_ostream& OS) {
            IdentRef* Ref = static_cast<IdentRef*>(FromExpr);
            OS << "If you wish to get the function type use: &" << Ref->Ident << ".";
        });
    } else if (ToTy->IsInt() && FromExpr->Is(AstKind::NUMBER_LITERAL)) {
        // The integer does not fit into the destination type. So adding additional
        // information to report about that case.
        if (!FromExpr->Ty->IsFloat()) {
            Log.AddNoteLine([=](llvm::raw_ostream& OS) {
                NumberLiteral* Number = static_cast<NumberLiteral*>(FromExpr);

                OS << "The value ";
                if (FromExpr->Ty->IsSigned()) {
                    OS << Number->SignedIntValue;
                } else {
                    OS << Number->UnsignedIntValue;
                }
                OS << " could not fit into '" << ToTy << "' ";
                OS << "(" << ToTy->GetSizeInBytes(Context.LLArcoModule) * 8 << " bits)";
                });
        }
    }
}

void arco::SemAnalyzer::DisplayErrorForTypeMismatch(const char* ErrMsg, SourceLoc ErrorLoc,
                                                    Expr* FromExpr, Type* ToTy) {
    Log.BeginError(ErrorLoc, ErrMsg, FromExpr->Ty, ToTy);
    DisplayNoteInfoForTypeMismatch(FromExpr, ToTy);
    Log.EndError();
}

llvm::SmallVector<arco::TypeInfo> arco::SemAnalyzer::ParamsToTypeInfo(FuncDecl* Func) {
    llvm::SmallVector<TypeInfo> ParamTypes;
    ParamTypes.reserve(Func->Params.size());
    for (VarDecl* Param : Func->Params) {
        ParamTypes.push_back(TypeInfo{
            Param->Ty,
            Param->HasConstAddress,
            Param->ImplicitPtr
            });
    }
    return ParamTypes;
}

llvm::Value* arco::SemAnalyzer::GenFoldable(SourceLoc ErrorLoc, Expr* E) {
    IRGenerator IRGen(Context);
    llvm::Value* LLValue = IRGen.GenRValue(E);
    if (LLValue->getValueID() == llvm::Value::ValueTy::PoisonValueVal) {
        Error(ErrorLoc, "Signed overflow");
        return nullptr;
    }
    return LLValue;
}

void arco::SemAnalyzer::RequestGenNonGenericFunc(ArcoContext& Context, FuncDecl* Func) {
    if (!FoundCompileError && !Context.CheckingUnhcecked) {
        if (!Func->GenRequestedAlready) {
            IRGenerator IRGen(Context);
            IRGen.GenFuncDecl(Func);
        }
    }
    Context.RequestGen(Func);
}

void arco::SemAnalyzer::AddGenericErrorInfo() {
    if (CFunc && CFunc->IsGeneric()) {
        if (CFunc->GenData->CurBinding) {
            //Log.AddMarkMessage(
            //    CFunc->GenData->CurBinding->OriginalFile,
            //    CFunc->GenData->CurBinding->OriginalLoc,
            //    "Original bind location");
        }
    }
}

void arco::SemAnalyzer::CreateQualifications(Decl* D) {
    GenericBinding* Binding = D->GenData->CurBinding;
    GenericData* GenData = D->GenData;
    do {
        // TODO: This is broken still because it tries to qualify based on
        // the bindable types of the function but it may be referencing
        // the bindable types of the parent declaration to try and bind.
        for (Type* UnqualifiedType : GenData->TypesNeedingQualification) {
            Type* QualType = QualifyType(UnqualifiedType, Binding);
            UnqualifiedType->SetQualType(QualType);
        }
        for (VarDecl* VarNeedingConstQual : GenData->VarsNeedingConstQualification) {
            bool HasConstAddress = VarNeedingConstQual->Ty->GetKind() == TypeKind::CStr ||
                                    VarNeedingConstQual->ExplicitlyMarkedConst;
            VarNeedingConstQual->HasConstAddress = HasConstAddress;
        }
        Binding = Binding->ParentBinding;
        if (Binding) {
            GenData = GenData->ParentGenData;
        }
    } while (Binding);
}
