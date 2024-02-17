#include "SemAnalysis.h"

#include "Context.h"
#include "IRGen.h"

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
    : Context(Context), Mod(D->Mod), Log(D->FScope->Path.c_str(), D->FScope->Buffer)
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
        Logger Log(FScope->Path.c_str(), FScope->Buffer);
        Log.BeginError(InvalidStmt->Loc, "Statement does not belong at %s scope",
            ScopeKind);
        Log.EndError();
    }
}

void arco::SemAnalyzer::ResolveImports(FileScope* FScope, ArcoContext& Context) {
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
                    Logger Log(FScope->Path.c_str(), FScope->Buffer);
                    Log.BeginError(ErrorLoc, "Could not find module or namespace '%s'",
                        StructOrNamespaceImport.ModOrNamespace);
                    Log.EndError();
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
                        Logger Log(FScope->Path.c_str(), FScope->Buffer);
                        Log.BeginError(ErrorLoc, "Could not find type or namespace '%s' in module '%s'",
                            StructOrNamespaceImport.StructOrNamespace, StructOrNamespaceImport.ModOrNamespace);
                        Log.EndError();
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
                        Logger Log(FScope->Path.c_str(), FScope->Buffer);
                        Log.BeginError(ErrorLoc, "Could not find type '%s' in namespace '%s'",
                            StructOrNamespaceImport.StructOrNamespace, StructOrNamespaceImport.ModOrNamespace);
                        Log.EndError();
                    }
                } else {
                    Logger Log(FScope->Path.c_str(), FScope->Buffer);
                    Log.BeginError(ErrorLoc, "Could not find module or namespace '%s'",
                        StructOrNamespaceImport.ModOrNamespace);
                    Log.EndError();
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
                        Logger Log(FScope->Path.c_str(), FScope->Buffer);
                        Log.BeginError(ErrorLoc, "Could not find type '%s' in namespace '%s'",
                            StructOrNamespaceImport.StructName, StructOrNamespaceImport.StructOrNamespace);
                        Log.EndError();
                    }
                } else {
                    Logger Log(FScope->Path.c_str(), FScope->Buffer);
                    Log.BeginError(ErrorLoc, "Could not find namespace '%s' in module '%s'",
                        StructOrNamespaceImport.StructOrNamespace, StructOrNamespaceImport.ModOrNamespace);
                    Log.EndError();
                }
            } else {
                Logger Log(FScope->Path.c_str(), FScope->Buffer);
                Log.BeginError(ErrorLoc, "Could not find module '%s'",
                    StructOrNamespaceImport.ModOrNamespace);
                Log.EndError();
            }
        }
    }
    for (auto& Import : FScope->StaticImports) {
        if (Import.Mod && Import.NamespaceName.IsNull()) {
            // Importing module's default namespace.
            Import.NSpace = Import.Mod->DefaultNamespace;
        } else if (Import.Mod) {
            // Statically importing namespace from module.
            auto Itr = Import.Mod->Namespaces.find(Import.NamespaceName);
            if (Itr == Import.Mod->Namespaces.end()) {
                Logger Log(FScope->Path.c_str(), FScope->Buffer);
                Log.BeginError(Import.ErrorLoc, "Could not find namespace '%s' in module '%s'",
                    Import.NamespaceName, Import.Mod->Name);
                Log.EndError();
                continue;
            }
            Import.NSpace = Itr->second;
        } else {
            // Statically importing namespace from the source file's module.
            auto Itr = FScope->Mod->Namespaces.find(Import.NamespaceName);
            if (Itr == FScope->Mod->Namespaces.end()) {
                Logger Log(FScope->Path.c_str(), FScope->Buffer);
                Log.BeginError(Import.ErrorLoc, "Could not find namespace or module '%s'", Import.NamespaceName);
                Log.EndError();
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
    }
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
        }
    }
}

void arco::SemAnalyzer::CheckFuncDecl(FuncDecl* Func) {
    if (Func->HasBeenChecked) return;
    Func->HasBeenChecked = true;
    Context.UncheckedDecls.erase(Func);
    if (Func->ParsingError) return;
    
    CFunc   = Func;
    CStruct = Func->Struct;
    FScope  = Func->FScope;

    // -- DEBUG
    // llvm::outs() << "Checking function: " << Func->Name << "\n";

    if (!Func->IsConstructor && !Func->InitializerValues.empty()) {
        Error(Func, "Only constructors can have initializer values");
    }
    if (!Func->IsConstructor && Func->IsCopyConstructor) {
        Error(Func, "Only constructors can use keyword copyobj to indicate it is a copy constructor");
    }
    if (!Func->IsConstructor && Func->IsMoveConstructor) {
        Error(Func, "Only constructors can use keyword moveobj to indicate it is a move constructor");
    }

    CheckFuncParams(Func);
    if (Func->RetTy->GetKind() == TypeKind::Array) {
        Error(Func, "Functions cannot return arrays");
    }
    
    auto CheckMoveOrCopyConstructor = [=](const char* ConstructorType) {
        if (Func->IsVariadic) {
            Error(Func->Loc, "%s constructors cannot be variadic", ConstructorType);
        }
        if (Func->Params.empty()) {
            Error(Func->Loc, "%s constructor must have argument for struct to copy", ConstructorType);
        } else if (Func->Params.size() > 1) {
            Error(Func->Loc, "%s constructor must have only one argument for the struct to copy", ConstructorType);
        } else {
            Type* ParamTy = Func->Params[0]->Ty;
            Type* ExpectedType = PointerType::Create(StructType::Create(Func->Struct, Context), Context);
            if (!ParamTy->Equals(ExpectedType)) {
                Error(Func->Loc, "%s constructor must have type '%s' but found type '%s'",
                    ConstructorType, ExpectedType->ToString(), ParamTy->ToString());
            }
        }
    };

    if (Func->IsConstructor && Func->IsCopyConstructor) {
        CheckMoveOrCopyConstructor("Copy");
    } else if (Func->IsConstructor && Func->IsMoveConstructor) {
        CheckMoveOrCopyConstructor("Copy");
    }

    if (Func->IsConstructor) {

        StructDecl* Struct = Func->Struct;
        if (!Struct->HasBeenChecked) {
            SemAnalyzer Analyzer(Context, Struct);
            Analyzer.CheckStructDecl(Struct);
        }

        if (!Struct->ParsingError) {
            for (const FuncDecl::InitializerValue& InitValue : Func->InitializerValues) {
                CheckNode(InitValue.Assignment);
                if (!Struct->FindField(InitValue.FieldName)) {
                    Error(Func, "No field '%s' for initializer value", InitValue.FieldName);
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
                if (InitValue) {
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
        if (!Func->NativeName.empty()) {
            Name = Identifier(Func->NativeName);
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
    if (Struct->HasBeenChecked) return;
    Struct->HasBeenChecked = true;
    Context.UncheckedDecls.erase(Struct);
    if (Struct->ParsingError) return;

    Struct->IsBeingChecked = true;
    FScope  = Struct->FScope;
    CStruct = Struct;

    if (Struct->Destructor) {
        Struct->NeedsDestruction = true;
    }

    for (VarDecl* Field : Struct->Fields) {
        CheckVarDecl(Field);
        if (Field->Assignment) {
            Struct->FieldsHaveAssignment = true;
        }

        // TODO: Does the field need to be checked for a complete type here?
        if (Field->Ty->GetKind() == TypeKind::Struct) {
            StructType* StructTy = Field->Ty->AsStructType();
            StructDecl* OStruct = StructTy->GetStruct();
            if (Struct) {
                Struct->FieldsHaveAssignment |= OStruct->FieldsHaveAssignment;
                Struct->NeedsDestruction |= OStruct->NeedsDestruction;
            }
        } else if (Field->Ty->GetKind() == TypeKind::Array) {
            ArrayType* ArrayTy = Field->Ty->AsArrayTy();

            Type* BaseTy = ArrayTy->GetBaseType();
            if (BaseTy->GetKind() == TypeKind::Struct) {
                StructType* StructTy = Field->Ty->AsStructType();
                StructDecl* OStruct = StructTy->GetStruct();
                if (Struct) {
                    Struct->FieldsHaveAssignment |= OStruct->FieldsHaveAssignment;
                    Struct->NeedsDestruction |= OStruct->NeedsDestruction;
                }
            }
        }
    }

    // Want to set this to false before checking member functions or it will
    // complain about the type not being complete but all the relavent
    // type information has been determined already.
    Struct->IsBeingChecked = false;

    if (Struct->Destructor) {
        SemAnalyzer A(Context, Struct->Destructor);
        A.CheckFuncDecl(Struct->Destructor);
        Context.RequestGen(Struct->Destructor);
    }
    if (Struct->CopyConstructor) {
        SemAnalyzer A(Context, Struct->CopyConstructor);
        A.CheckFuncDecl(Struct->CopyConstructor);
        Context.RequestGen(Struct->CopyConstructor);
    }
    if (Struct->MoveConstructor) {
        SemAnalyzer A(Context, Struct->MoveConstructor);
        A.CheckFuncDecl(Struct->MoveConstructor);
        Context.RequestGen(Struct->MoveConstructor);
    }
}

void arco::SemAnalyzer::CheckFuncParams(FuncDecl* Func) {
    if (Func->ParamTypesChecked) return;
    Func->ParamTypesChecked = true;
    if (Func->ParsingError) return;

    CFunc   = Func;
    CStruct = Func->Struct;
    FScope  = Func->FScope;

    if (!FixupType(Func->RetTy)) {
        Func->RetTy = Context.ErrorType;
    }

    llvm::SmallVector<VarDecl*, 2> Params = Func->Params;
    bool ParamsHaveAssignment = false, ParamAssignmentNotLast = false;
    for (VarDecl* Param : Params) {
        CheckVarDecl(Param);
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
            Error(Last, "Cannot have variadic arguments of type '%s'", Last->Ty->ToString());
            Func->ParsingError = true; // Hacky but reduces error messages later on.
        }
        // TODO: can there be circular dependency here?
        Last->Ty = SliceType::Create(Last->Ty, Context);
    }

    if (ParamAssignmentNotLast) {
        Error(Func, "Parameter default arguments must come last in the parameter list");
        Func->ParsingError = true; // Hacky but reduces error messages later on.
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
    case AstKind::IF:
        CheckIf(static_cast<IfStmt*>(Node));
        break;
    case AstKind::NESTED_SCOPE:
        CheckNestedScope(static_cast<NestedScopeStmt*>(Node));
        break;
    case AstKind::BINARY_OP:
        CheckBinaryOp(static_cast<BinaryOp*>(Node));
        break;
    case AstKind::UNARY_OP:
        CheckUnaryOp(static_cast<UnaryOp*>(Node));
        break;
    case AstKind::IDENT_REF:
        CheckIdentRef(static_cast<IdentRef*>(Node), false, Mod->DefaultNamespace);
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
    case AstKind::STRUCT_INITIALIZER:
        CheckStructInitializer(static_cast<StructInitializer*>(Node));
        break;
    case AstKind::HEAP_ALLOC:
        CheckHeapAlloc(static_cast<HeapAlloc*>(Node));
        break;
    case AstKind::SIZEOF:
        CheckSizeOf(static_cast<SizeOf*>(Node));
        break;
    case AstKind::TYPEOF:
        CheckTypeOf(static_cast<TypeOf*>(Node));
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

void arco::SemAnalyzer::CheckEnumDecl(EnumDecl* Enum) {
    if (Enum->HasBeenChecked) return;
    Enum->HasBeenChecked = true;
    Context.UncheckedDecls.erase(Enum);
    if (Enum->ParsingError) return;

    Enum->IsBeingChecked = true;
    FScope  = Enum->FScope;
    
    
    // TODO: check to make sure there is not duplicate values

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
    IRGenerator IRGen(Context);
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
                    Value.Assignment->Ty->ToString(), ValuesType->ToString());
            } else {
                CreateCast(Value.Assignment, ValuesType);
                if (Value.Assignment->IsFoldable && Value.Assignment->Ty->IsInt()) {
                    // TODO: Check to make sure that the index is well.. indexable!

                    llvm::ConstantInt* LLValueIndex =
                        llvm::cast<llvm::ConstantInt>(IRGen.GenRValue(Value.Assignment));
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

void arco::SemAnalyzer::CheckVarDecl(VarDecl* Var) {
    if (Var->HasBeenChecked) return;
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

    if (Var->IsField()) {
        CField = Var;
    }

    // TODO: Is it needed to store a previous CGlobal/CField
    //       and set it to that once the function returns?
#define VAR_YIELD(E, TyErr)      \
Var->IsBeingChecked = false;     \
CGlobal = nullptr;               \
CField  = nullptr;               \
E;                               \
if constexpr (TyErr)             \
    Var->Ty = Context.ErrorType; \
return;

    if (!Var->TyIsInfered && !FixupType(Var->Ty)) {
        VAR_YIELD(, true);
    }

    if (Var->IsGlobal) {
        Context.RequestGen(Var);
    }


    if (Var->Assignment) {
        CheckNode(Var->Assignment);
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
                        FromArrayType->GetBaseType()->ToString(),
                        ImplicitArrayType->GetBaseType()->ToString()),
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

        if (ViolatesConstAssignment(Var, Var->Assignment)) {
            VAR_YIELD(Error(Var, "Cannot assign const memory to non-const variable"),
                true);
        }
    } else {
        // No assignment.

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
            } else if (StructForTy->DefaultConstructor) {
                Context.RequestGen(StructForTy->DefaultConstructor);
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
                        StructTy->ToString());
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
    }

    if (Var->Ty->Equals(Context.CStrType)) {
        Var->HasConstAddress = true;
    }

    if (Var->HasConstAddress && !Var->Assignment &&
        !Var->Ty->IsPointer() && !Var->IsParam()) {
        Error(Var, "Must initialize variables marked with const");
    }

    if (Var->Ty->Equals(Context.VoidType)) {
        VAR_YIELD(Error(Var, "Variables cannot have type 'void'"), true);
    }

    VAR_YIELD(, false);

#undef VAR_YIELD
}

void arco::SemAnalyzer::CheckReturn(ReturnStmt* Return) {
    LocScope->FoundTerminal  = true;
    LocScope->AllPathsReturn = true;
    
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
        Error(Return, "Return type '%s' does not match expected type '%s'",
            Return->Value ? Return->Value->Ty->ToString() : Context.VoidType->ToString(),
            CFunc->RetTy->ToString());
    }
}

void arco::SemAnalyzer::CheckLoopControl(LoopControlStmt* LoopControl) {
    LocScope->FoundTerminal = true;
    
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
            Error(Loop->IterOnExpr->Loc, "Cannot iterate on type '%s'", IterableType->ToString());
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
                    IterOnType->ToString(), Loop->VarVal->Ty->ToString());
                return;
            }
        }
    } else {
        // Type is infered.
        // TODO: Should this select for pointer types if the type is a structure
        // and a large enough to not want to copy?
        Loop->VarVal->Ty = IterOnType;
    }

    ++LoopDepth;
    Scope LoopScope;
    CheckScopeStmts(Loop->Scope, LoopScope);
    --LoopDepth;
}

void arco::SemAnalyzer::CheckDeleteStmt(DeleteStmt* Delete) {
    CheckNode(Delete->Value);
    YIELD_IF_ERROR(Delete->Value);

    if (Delete->Value->Ty->GetKind() != TypeKind::Pointer) {
        Error(Delete, "Cannot delete type '%s'", Delete->Value->Ty->ToString());
    }
}

bool arco::SemAnalyzer::CheckIf(IfStmt* If) {

    CheckCondition(If->Cond, "If");

    Scope IfBodyScope;
    CheckScopeStmts(If->Scope, IfBodyScope);
    bool AllPathsReturn = If->Else && IfBodyScope.AllPathsReturn;

    if (If->Else) {
        if (If->Else->Is(AstKind::IF)) {
            // This is an else if case.
            AllPathsReturn &= CheckIf(static_cast<IfStmt*>(If->Else));
        } else {
            AllPathsReturn &= CheckNestedScope(static_cast<NestedScopeStmt*>(If->Else));
        }
    }

    LocScope->AllPathsReturn = AllPathsReturn;
    LocScope->FoundTerminal |= AllPathsReturn;

    return AllPathsReturn;
}

bool arco::SemAnalyzer::CheckNestedScope(NestedScopeStmt* NestedScope) {
    Scope NewScope;
    CheckScopeStmts(NestedScope->Scope, NewScope);
    return NewScope.AllPathsReturn;
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
    CheckNode(BinOp->RHS);

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
    T->ToString(),                                                       \
    LTy->ToString(),                                                     \
    Token::TokenKindToString(BinOp->Op, Context),		                 \
    RTy->ToString()                                                      \
    );                                                                   \
YIELD_ERROR(BinOp)

#define TYPE_MISMATCH()                                                  \
Error(BinOp, "Operator '%s' had mismatched types   ('%s' %s '%s')",      \
    Token::TokenKindToString(BinOp->Op, Context),                        \
    LTy->ToString(),                                                     \
    Token::TokenKindToString(BinOp->Op, Context),		                 \
    RTy->ToString()                                                      \
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


        if (BinOp->Op == TokenKind::SLASH_EQ || BinOp->Op == TokenKind::MOD_EQ) {
            if (BinOp->RHS->IsFoldable) {
                IRGenerator IRGen(Context);
                llvm::Constant* LLInt = llvm::cast<llvm::Constant>(IRGen.GenRValue(BinOp->RHS));
                if (LLInt->isZeroValue()) {
                    Error(BinOp, "Division by zero");
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
            IRGenerator IRGen(Context);
            llvm::Constant* LLInt = llvm::cast<llvm::Constant>(IRGen.GenRValue(BinOp->RHS));
            if (LLInt->isZeroValue()) {
                Error(BinOp, "Division by zero");
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
            IRGenerator IRGen(Context);
            llvm::ConstantInt* LLInt = llvm::cast<llvm::ConstantInt>(IRGen.GenRValue(BinOp->RHS));
            if (LLInt->isZeroValue()) {
                Error(BinOp, "Division by zero");
            }
        } else if (BinOp->RHS->IsFoldable) { // << or >>
            IRGenerator IRGen(Context);
            llvm::ConstantInt* LLInt = llvm::cast<llvm::ConstantInt>(IRGen.GenRValue(BinOp->RHS));
            if (LLInt->getZExtValue()-1 > LTy->GetTrivialTypeSizeInBytes() * 8) {
                Error(BinOp, "Shifting bits larger than bit size of type");
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
                      LTy->ToString(),
                      Token::TokenKindToString(BinOp->Op, Context),
                      RTy->ToString()
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
        if (LTy->IsPointer() || LTy->GetKind() == TypeKind::Null) {

            if (!(RTy->IsPointer() || RTy->GetKind() == TypeKind::Null)) {
                Error(BinOp->RHS, "Expected to be a pointer");
            }

            if (RTy->GetKind() == TypeKind::Null) {
                CreateCast(BinOp->RHS, BinOp->LHS->Ty);
            } else if (LTy->GetKind() == TypeKind::Null) {
                CreateCast(BinOp->LHS, BinOp->RHS->Ty);
            }

            BinOp->Ty = Context.BoolType;
        } else if (RTy->IsPointer() || RTy->GetKind() == TypeKind::Null) {
            
            if (!(LTy->IsPointer() || LTy->GetKind() == TypeKind::Null)) {
                Error(BinOp->LHS, "Expected to be a pointer");
            }

            if (RTy->GetKind() == TypeKind::Null) {
                CreateCast(BinOp->RHS, BinOp->LHS->Ty);
            } else if (LTy->GetKind() == TypeKind::Null) {
                CreateCast(BinOp->LHS, BinOp->RHS->Ty);
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

#define OPERATOR_CANNOT_APPLY(T)                                  \
Error(UniOp, "Operator '%s' cannot apply to type '%s'",           \
    Token::TokenKindToString(UniOp->Op, Context), T->ToString()); \
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
            
            if (Func->Struct) {
                Error(UniOp, "Not supporting taking address of member functions yet!");
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
            
            Context.RequestGen(Func);

            UniOp->HasConstAddress = false;
            UniOp->Ty = FunctionType::Create(TypeInfo{ Func->RetTy, Func->ReturnsConstAddress },
                                             std::move(ParamTypes), Context);
        } else {
            if (!IsLValue(UniOp->Value) && UniOp->Value->Ty->GetKind() != TypeKind::Struct) {
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
    default:
        assert(!"Unhandled unary check");
        break;
    }
}

void arco::SemAnalyzer::CheckIdentRef(IdentRef* IRef,
                                      bool ExpectsFuncCall,
                                      Namespace* NamespaceToLookup,
                                      StructDecl* StructToLookup) {

    bool LocalNamespace = NamespaceToLookup == Mod->DefaultNamespace;

    auto SearchForFuncs = [=]() {
        if (StructToLookup) {
            auto Itr = StructToLookup->Funcs.find(IRef->Ident);
            if (Itr != StructToLookup->Funcs.end()) {
                IRef->Funcs   = &Itr->second;
                IRef->RefKind = IdentRef::RK::Funcs;
            }
        } else {
            // Relative member functions.
            if (CStruct) {
                auto Itr = CStruct->Funcs.find(IRef->Ident);
                if (Itr != CStruct->Funcs.end()) {
                    IRef->Funcs   = &Itr->second;
                    IRef->RefKind = IdentRef::RK::Funcs;
                    return;
                }
            }
            

            auto Itr = NamespaceToLookup->Funcs.find(IRef->Ident);
            if (Itr != NamespaceToLookup->Funcs.end()) {
                IRef->Funcs   = &Itr->second;
                IRef->RefKind = IdentRef::RK::Funcs;
                return;
            }
            if (LocalNamespace && FScope->UniqueNSpace) {
                // File marked with a namespace need to search the namespace the file belongs to as well.
                Itr = FScope->UniqueNSpace->Funcs.find(IRef->Ident);
                if (Itr != FScope->UniqueNSpace->Funcs.end()) {
                    IRef->Funcs   = &Itr->second;
                    IRef->RefKind = IdentRef::RK::Funcs;
                    return;
                }
            }

            // Searching for functions in static imports.
            if (LocalNamespace) {
                for (auto& Import : FScope->StaticImports) {
                    auto Itr = Import.NSpace->Funcs.find(IRef->Ident);
                    if (Itr != Import.NSpace->Funcs.end()) {
                        IRef->Funcs   = &Itr->second;
                        IRef->RefKind = IdentRef::RK::Funcs;
                        return;
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

    auto SearchForOther = [=]() {
        if (StructToLookup) {
            VarDecl* Field = StructToLookup->FindField(IRef->Ident);
            if (Field) {
                IRef->Var     = Field;
                IRef->RefKind = IdentRef::RK::Var;
            }
        } else {
            
            if (SearchNamespace(NamespaceToLookup)) {
                return;
            }

            if (LocalNamespace && FScope->UniqueNSpace) {
                // File marked with a namespace need to search the namespace the file belongs to as well.
                if (SearchNamespace(FScope->UniqueNSpace)) {
                    return;
                }
            }

            // Searching for global variables in static imports.
            if (LocalNamespace) {
                for (auto& Import : FScope->StaticImports) {
                    if (SearchNamespace(Import.NSpace)) {
                        return;
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
    
    switch (IRef->RefKind) {
    case IdentRef::RK::Var: {
        VarDecl* VarRef = IRef->Var;
        if (VarRef->IsGlobal || VarRef->IsField()) {
            EnsureChecked(IRef->Loc, VarRef);
        }

        IRef->Ty = VarRef->Ty;

        IRef->HasConstAddress = VarRef->HasConstAddress;
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
            Error(IRef, "Could not find a function for identifier '%s'", IRef->Ident);
        } else {
            Error(IRef, "Could not find symbol for %s '%s'",
                  StructToLookup ? "field" : "identifier",
                  IRef->Ident);
        }
        IRef->Ty = Context.ErrorType;
        break;
    }
    default:
        assert(!"Unimplemented ident reference end case");
        break;
    }

}

void arco::SemAnalyzer::CheckFieldAccessor(FieldAccessor* FieldAcc, bool ExpectsFuncCall) {

    Expr* Site = FieldAcc->Site;

    if (Site->Is(AstKind::IDENT_REF)) {
        CheckIdentRef(static_cast<IdentRef*>(Site), false, Mod->DefaultNamespace);
    } else {
        CheckNode(Site);
    }
    YIELD_ERROR_WHEN(FieldAcc, Site);
    FieldAcc->HasConstAddress = Site->HasConstAddress;
    FieldAcc->IsFoldable      = Site->IsFoldable;

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
    }

    if (Site->Ty == Context.EnumRefType) {
        IdentRef* IRef = static_cast<IdentRef*>(Site);
        FieldAcc->Ty = StructType::Create(IRef->Enum, Context);
        FieldAcc->EnumValue = IRef->Enum->FindValue(FieldAcc->Ident);

        // Ex. Day.MONDAY
        if (!FieldAcc->EnumValue) {
            Error(FieldAcc, "enum '%s' does not contain value '%s'",
                IRef->Struct->Name, FieldAcc->Ident);
        }

        return;
    }

    if (Site->Ty == Context.ImportType) {
        IdentRef* IRef = static_cast<IdentRef*>(Site);
        CheckIdentRef(FieldAcc, ExpectsFuncCall, IRef->NSpace);
        return;
    }

    if (!(Site->Ty->GetKind() == TypeKind::Struct ||
          (Site->Ty->GetKind() == TypeKind::Pointer &&
          Site->Ty->AsPointerTy()->GetElementType()->GetKind() == TypeKind::Struct)
         )) {
        Error(FieldAcc, "Cannot access field of type '%s'", Site->Ty->ToString());
        YIELD_ERROR(FieldAcc);
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
            StructTy->ToString());
        YIELD_ERROR(FieldAcc);
    } else {
        CheckIdentRef(FieldAcc, ExpectsFuncCall, Mod->DefaultNamespace, StructTy->GetStruct());
    }
}

void arco::SemAnalyzer::CheckThisRef(ThisRef* This) {
    if (!CStruct) {
        Error(This, "Cannot use 'this' outside a struct scope");
        YIELD_ERROR(This);
    }
    This->IsFoldable = false;
    This->Ty = PointerType::Create(StructType::Create(CStruct, Context), Context);
}

void arco::SemAnalyzer::CheckFuncCall(FuncCall* Call) {
    
    bool ArgHasError = false;
    for (auto& Arg : Call->Args) {
        CheckNode(Arg.E);
        if (Arg.E->Ty == Context.ErrorType)
            ArgHasError = true;
    }
    if (ArgHasError) {
        YIELD_ERROR(Call);
    }

    switch (Call->Site->Kind) {
    case AstKind::IDENT_REF:
        CheckIdentRef(static_cast<IdentRef*>(Call->Site), true, Mod->DefaultNamespace);
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

        if (Call->Args.size() != FuncTy->ParamTypes.size()) {
            DisplayErrorForSingleFuncForFuncCall("function",
                                                 Call->Loc,
                                                 FuncTy->ParamTypes,
                                                 Call->Args,
                                                 0 /* Num default args */);
            YIELD_ERROR(Call);
        }

        for (ulen i = 0; i < Call->Args.size(); i++) {
            Expr*    Arg         = Call->Args[i].E;
            TypeInfo ParamTyInfo = FuncTy->ParamTypes[i];

            if (!IsAssignableTo(ParamTyInfo.Ty, Arg) ||
                ViolatesConstAssignment(ParamTyInfo.Ty, ParamTyInfo.ConstMemory, Arg)) {
                DisplayErrorForSingleFuncForFuncCall("function",
                                                     Call->Loc,
                                                     
                                                     FuncTy->ParamTypes,
                                                     Call->Args,
                                                     0 /* Num default args */);
                YIELD_ERROR(Call);
            }
        }

        // Creating casts for the arguments.
        for (ulen i = 0; i < Call->Args.size(); i++) {
            Expr*    Arg         = Call->Args[i].E;
            TypeInfo ParamTyInfo = FuncTy->ParamTypes[i];
            CreateCast(Arg, ParamTyInfo.Ty);
        }

        Call->Ty = FuncTy->RetTyInfo.Ty;
        Call->IsFoldable = false;
        Call->HasConstAddress = FuncTy->RetTyInfo.ConstMemory;

        return;
    } else if (SiteTy->GetKind() != TypeKind::FuncRef) {
        // Invalid call.
        Log.BeginError(Call->Loc, "cannot call type '%s'", SiteTy->ToString());
        if (SiteTy->GetKind() == TypeKind::StructRef) {
            IdentRef* IRef = static_cast<IdentRef*>(Call->Site);
            StructDecl* Struct = IRef->Struct;
            // TODO: Should this validate that there is a constructor first?
            Log.AddNoteLine([Struct](auto& OS) {
                OS << "Tried to call a constructor? Use: ";
                OS << Struct->Name;
                OS << "{ <your args> } instead.";
            });
        }
        Log.EndError();

        YIELD_ERROR(Call);
    }

    FuncsList* Canidates = static_cast<IdentRef*>(Call->Site)->Funcs;

    bool VarArgsPassAlong = false;
    Call->CalledFunc = CheckCallToCanidates(Call->Loc, Canidates, Call->Args, VarArgsPassAlong);
    if (!Call->CalledFunc) {
        YIELD_ERROR(Call);
    }
    Call->VarArgsPassAlong = VarArgsPassAlong;

    Call->IsFoldable = false;
    Call->Ty = Call->CalledFunc->RetTy;
    Call->HasConstAddress = Call->CalledFunc->ReturnsConstAddress;

}

arco::FuncDecl* arco::SemAnalyzer::CheckCallToCanidates(SourceLoc ErrorLoc,
                                                        FuncsList* Canidates,
                                                        llvm::SmallVector<NonNamedValue, 2>& Args,
                                                        bool& VarArgsPassAlong) {
    FuncDecl* Selected = FindBestFuncCallCanidate(Canidates, Args, VarArgsPassAlong);
    if (!Selected) {
        DisplayErrorForNoMatchingFuncCall(ErrorLoc, Canidates, Args);
        return nullptr;
    }
    
    // Creating casts for the arguments.
    if (!Selected->IsVariadic) {
        for (ulen i = 0; i < Args.size(); i++) {
            Expr*    Arg   = Args[i].E;
            VarDecl* Param = Selected->Params[i];
            CreateCast(Arg, Param->Ty);
        }
    } else {
        ulen i = 0;
        for (; i < Selected->Params.size() - 1; i++) {
            Expr*    Arg   = Args[i].E;
            VarDecl* Param = Selected->Params[i];
            CreateCast(Arg, Param->Ty);
        }
        if (!VarArgsPassAlong) {
            VarDecl* LastParam = Selected->Params[i];
            Type*    VarArgTy  = LastParam->Ty->AsSliceTy()->GetElementType();
            for (; i < Args.size(); i++) {
                Expr* Arg = Args[i].E;
                CreateCast(Arg, VarArgTy);
            }
        }
    }
    
    Context.RequestGen(Selected);
    
    return Selected;
}

arco::FuncDecl* arco::SemAnalyzer::FindBestFuncCallCanidate(FuncsList* Canidates,
                                                            const llvm::SmallVector<NonNamedValue, 2>& Args,
                                                            bool& SelectedVarArgsPassAlong) {
    if (!Canidates) return nullptr;

    FuncDecl* Selection = nullptr;

    // TODO: make calls to functions with Any absolute last to consider.

    ulen LeastConflicts             = std::numeric_limits<ulen>::max(),
         LeastEnumImplicitConflicts = std::numeric_limits<ulen>::max(),
         LeastSignConflicts         = std::numeric_limits<ulen>::max();
    for (ulen i = 0; i < Canidates->size(); i++) {
        FuncDecl* Canidate = (*Canidates)[i];
        if (!Canidate->ParamTypesChecked) {
            SemAnalyzer Analyzer(Context, Canidate);
            Analyzer.CheckFuncParams(Canidate);
        }

        ulen NumConflicts = 0, EnumImplicitConflicts = 0, NumSignConflicts = 0;
        bool VarArgsPassAlong = false;
        if (!CompareAsCanidate(Canidate, Args, NumConflicts, EnumImplicitConflicts, NumSignConflicts, VarArgsPassAlong)) {
            continue;
        }

#define SET_BEST                                    \
 Selection                  = Canidate;             \
LeastConflicts             = NumConflicts;          \
LeastEnumImplicitConflicts = EnumImplicitConflicts; \
LeastSignConflicts         = NumSignConflicts;      \
SelectedVarArgsPassAlong   = VarArgsPassAlong;

        // TODO: What will be the performance on all this?
        if (NumConflicts < LeastConflicts) {
            SET_BEST;
        } else if (NumConflicts == LeastConflicts &&
                   NumSignConflicts < LeastSignConflicts) {
            SET_BEST;
        } else if (NumConflicts == LeastConflicts &&
                   NumSignConflicts == LeastSignConflicts && // TODO: do we care about num sign over enum in this case?
                   EnumImplicitConflicts < LeastEnumImplicitConflicts) {
            SET_BEST;
        }
    }
    return Selection;
}

bool arco::SemAnalyzer::CompareAsCanidate(FuncDecl* Canidate,
                                          const llvm::SmallVector<NonNamedValue, 2>& Args,
                                          ulen& NumConflicts,
                                          ulen& EnumImplicitConflicts,
                                          ulen& NumSignConflicts,
                                          bool& CanidateVarArgPassAlong) {
    // TODO: performance: all this awful variadic stuff can be optimized out by
    // using templating.

    if (Canidate->ParsingError) {
        return false;
    }
    if (Canidate->NumDefaultArgs) {
        if (!(Args.size() >= Canidate->Params.size() - Canidate->NumDefaultArgs &&
              Args.size() <= Canidate->Params.size())) {
            return false;
        }
    } else {
        if (Canidate->IsVariadic) {
            if (Args.size() < Canidate->Params.size() - 1) {
                return false;
            }
        } else {
            if (Args.size() != Canidate->Params.size()) {
                return false;
            }
        }
    }

    for (ulen i = 0; i < Args.size(); i++) {
        Expr* Arg = Args[i].E;
        
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
        
        if (!IsAssignableTo(ParamType, Arg)) {
            return false;
        }
        if (ViolatesConstAssignment(Param, Arg)) {
            return false;
        }
        if (!ParamType->Equals(Arg->Ty)) {
            ++NumConflicts;
            if (Arg->Ty->GetRealKind() == TypeKind::Enum) {
                
                EnumDecl* Enum = static_cast<StructType*>(Arg->Ty)->GetEnum();
                if (!Enum->ValuesType->Equals(ParamType)) {
                    ++EnumImplicitConflicts;
                }
            } else if (Arg->Ty->IsNumber()) {
                if (Arg->Ty->IsSigned() != ParamType->IsSigned()) {
                    ++NumSignConflicts;
                }
            }
        }
    }

    return true;
}

void arco::SemAnalyzer::DisplayErrorForNoMatchingFuncCall(SourceLoc ErrorLoc,
                                                          FuncsList* Canidates,
                                                          const llvm::SmallVector<NonNamedValue, 2>& Args) {
    
    const char* CallType = (*Canidates)[0]->IsConstructor ? "constructor" : "function";

    auto ParamsToTypeInfo = [](FuncDecl* Func) {
        llvm::SmallVector<TypeInfo> ParamTypes;
        ParamTypes.reserve(Func->Params.size());
        for (VarDecl* Param : Func->Params) {
            ParamTypes.push_back(TypeInfo{
                Param->Ty,
                Param->HasConstAddress
                });
        }
        return ParamTypes;
    };

    if (Canidates && Canidates->size() == 1) {
        FuncDecl* SingleCanidate = (*Canidates)[0];
        
        DisplayErrorForSingleFuncForFuncCall(CallType,
                                             ErrorLoc,
                                             ParamsToTypeInfo(SingleCanidate),
                                             Args,
                                             SingleCanidate->NumDefaultArgs,
                                             SingleCanidate);
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

        FuncDecl* FirstCanidate = (*Canidates)[0];

        llvm::SmallVector<TypeInfo> ArgTypes;
        ArgTypes.reserve(Args.size());
        for (NonNamedValue Arg : Args) {
            ArgTypes.push_back(TypeInfo{
                Arg.E->Ty,
                Arg.E->HasConstAddress
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
        const ulen BailMax = 6;
        ulen Count = 0;
        bool CanidateHasParsingError = false;

        for (FuncDecl* Canidate : *Canidates) {
            if (Count == BailMax) {
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
            std::string MismatchInfo = GetCallMismatchInfo(ParamTypes, Args, Canidate->NumDefaultArgs, Canidate->IsVariadic);
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
    const llvm::SmallVector<TypeInfo>& ParamTypes,
    const llvm::SmallVector<NonNamedValue, 2>& Args,
    ulen NumDefaultArgs,
    FuncDecl* CalledFunc) {

    if (CalledFunc && CalledFunc->ParsingError) {
        return;
    }

    // Single canidate so explicit details about
    // how there is a mismatch between the call and
    // the function is given.
        
    bool EncounteredError = false;

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

    bool IsVariadic = CalledFunc ? CalledFunc->IsVariadic : false;
    std::string ExtMsg = GetCallMismatchInfo(ParamTypes, Args, NumDefaultArgs, IsVariadic);
    
    Log.SetMsgToShowAbovePrimaryLocAligned(ExtMsg.c_str());
    Log.BeginError(CallLoc, ErrorMsg.c_str(), false);
    Log.EndError();
}

std::string arco::SemAnalyzer::GetFuncDefForError(const llvm::SmallVector<TypeInfo>& ParamTypes, FuncDecl* CalledFunc) {
    bool IsVariadic = CalledFunc && CalledFunc->IsVariadic;
    std::string FuncDef = CalledFunc ? ("fn " + CalledFunc->Name.Text.str()) : "fn";
    FuncDef += "(";
    for (ulen i = 0; i < ParamTypes.size(); i++) {
        FuncDef += (ParamTypes[i].ConstMemory && ParamTypes[i].Ty->GetKind() != TypeKind::CStr) ? "const " : "";
        if (!IsVariadic || i+1 != ParamTypes.size()) {
            FuncDef += ParamTypes[i].Ty->ToString();
        } else {
            FuncDef += ParamTypes[i].Ty->AsSliceTy()->GetElementType()->ToString();
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

std::string arco::SemAnalyzer::GetCallMismatchInfo(const llvm::SmallVector<TypeInfo>& ParamTypes,
                                                   const llvm::SmallVector<NonNamedValue, 2>& Args,
                                                   ulen NumDefaultArgs,
                                                   bool IsVariadic) {
    bool IncorrectNumberOfArgs = false;
    std::string MismatchInfo = "";
    if (NumDefaultArgs) {
        ulen MinArgs = ParamTypes.size() - NumDefaultArgs;
        ulen MaxArgs = ParamTypes.size();
        if (!(Args.size() >= MinArgs &&
              Args.size() <= MaxArgs)) {
            IncorrectNumberOfArgs = true;
            MismatchInfo = "- Incorrect number of arguments. Expected between "
                     + std::to_string(MinArgs) + std::string("-") + std::to_string(MaxArgs)
                     + ". Got " + std::to_string(Args.size()) + ".";
            
        }
    } else {
        if (IsVariadic) {
            if (Args.size() < ParamTypes.size() - 1) {
                IncorrectNumberOfArgs = true;
                MismatchInfo = "- Incorrect number of arguments. Expected at least "
                     + std::to_string(ParamTypes.size() - 1)
                     + ". Got " + std::to_string(Args.size()) + ".";
            }
        } else {
            if (Args.size() != ParamTypes.size()) {
                IncorrectNumberOfArgs = true;
                MismatchInfo = "- Incorrect number of arguments. Expected "
                     + std::to_string(ParamTypes.size())
                     + ". Got " + std::to_string(Args.size()) + ".";
            }
        }
    }

    if (!IncorrectNumberOfArgs) {
        // One or more of the arguments are incorrect.
        bool EncounteredError = false;
        for (ulen ArgCount = 0; ArgCount < Args.size(); ArgCount++) {
            Expr* Arg = Args[ArgCount].E;
            bool  ParamConstMemory;
            Type* ParamTy;
            if (IsVariadic) {
                if (ArgCount >= ParamTypes.size() - 1) {
                    TypeInfo ParamTyInfo = ParamTypes[ParamTypes.size() - 1];
                    ParamTy          = ParamTyInfo.Ty->AsSliceTy()->GetElementType();
                    ParamConstMemory = ParamTyInfo.ConstMemory;
                } else {
                    TypeInfo ParamTyInfo = ParamTypes[ArgCount];
                    ParamTy          = ParamTyInfo.Ty;
                    ParamConstMemory = ParamTyInfo.ConstMemory;
                }
            } else {
                TypeInfo ParamTyInfo = ParamTypes[ArgCount];
                ParamTy          = ParamTyInfo.Ty;
                ParamConstMemory = ParamTyInfo.ConstMemory;
            }
            
            if (!IsAssignableTo(ParamTy, Arg->Ty, Arg)) {
                if (EncounteredError)  MismatchInfo += "\n";
                MismatchInfo += "- Cannot assign argument "
                       + std::to_string(ArgCount+1) + " of type '" + Arg->Ty->ToString() + "' "
                       + "to parameter of type '" + ParamTy->ToString() + "'.";
                EncounteredError = true;
            } else if (ViolatesConstAssignment(ParamTy, ParamConstMemory, Arg)) {
                if (EncounteredError)  MismatchInfo += "\n";
                MismatchInfo += "- Cannot assign argument "
                       + std::to_string(ArgCount+1) + " with const memory to non-const parameter.";
                
                EncounteredError = true;
            }
        }
    }
    
    return MismatchInfo;
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
        Error(Access, "Expected int type for index. Found type '%s'", Access->Index->Ty->ToString());
    }

    TypeKind Kind = Access->Site->Ty->GetKind();
    if (!(Kind == TypeKind::Array || Kind == TypeKind::Pointer || Kind == TypeKind::CStr ||
          Kind == TypeKind::Slice)) {
        Error(Access, "Cannot index non-array or pointer type. Type was '%s'",
            Access->Site->Ty->ToString());
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
        Error(Cast, "Cannot cast from type '%s' to type '%s'",
            Cast->Value->Ty->ToString(), Cast->Ty->ToString());
    }
}

void arco::SemAnalyzer::CheckStructInitializer(StructInitializer* StructInit) {
    StructType* StructTy = StructInit->Ty->AsStructType();
    if (!FixupStructType(StructTy)) {
        YIELD_ERROR(StructInit);
    }

    StructDecl* Struct = StructTy->GetStruct();

    // TODO: May want to allow if the fields are foldable!
    StructInit->IsFoldable = false;
    StructInit->CalledConstructor = CheckStructInitArgs(Struct, StructInit->Loc, StructInit->Args, StructInit->VarArgsPassAlong);

}

arco::FuncDecl* arco::SemAnalyzer::CheckStructInitArgs(StructDecl* Struct,
                                                       SourceLoc ErrorLoc,
                                                       llvm::SmallVector<NonNamedValue, 2>& Args,
                                                       bool& VarArgPassAlong) {

    bool ArgsHaveErrors = false;
    for (ulen i = 0; i < Args.size(); i++) {
        NonNamedValue Value = Args[i];
        CheckNode(Value.E);
        if (Value.E->Ty == Context.ErrorType) {
            ArgsHaveErrors = true;
        }
    }

    if (!Struct->Constructors.empty()) {
        // Calling constructor!

        if (ArgsHaveErrors) {
            return nullptr;
        }

        // TODO: don't ignore VarArgsPassAlong value
        return CheckCallToCanidates(
                ErrorLoc,
                &Struct->Constructors,
                Args,
                VarArgPassAlong);
    }

    for (ulen i = 0; i < Args.size(); i++) {
        NonNamedValue Value = Args[i];
        
        if (i >= Struct->Fields.size()) {
            Error(Value.ExpandedLoc, "Too many fields in initializer");
            return nullptr;
        }

        if (Value.E->Ty == Context.ErrorType) {
            continue;
        }

        VarDecl* Field = Struct->Fields[i];
        Type* FieldTy = Field->Ty;
        if (!Field->Assignment && Field->Ty->GetKind() == TypeKind::Struct) {
            StructDecl* StructForTy = Field->Ty->AsStructType()->GetStruct();

            if (!StructForTy->Constructors.empty() && !StructForTy->DefaultConstructor) {
                Error(ErrorLoc, "No default constructor to initialize the '%s' field", Field->Name);
            }
        }
        if (!IsAssignableTo(FieldTy, Value.E)) {
            DisplayErrorForTypeMismatch(
                "Cannot assign value of type '%s' to field of type '%s'",
                Value.ExpandedLoc,
                Value.E,
                FieldTy);
        } else {
            CreateCast(Value.E, FieldTy);
        }
    }

    return nullptr;
}

void arco::SemAnalyzer::CheckHeapAlloc(HeapAlloc* Alloc) {
    Alloc->IsFoldable = false;

    Type* TypeToAlloc = Alloc->TypeToAlloc;

    if (!FixupType(TypeToAlloc, true)) {
        YIELD_ERROR(Alloc);
    }


    if (TypeToAlloc->GetKind() == TypeKind::Struct) {
        StructType* StructTy = TypeToAlloc->AsStructType();
        Alloc->CalledConstructor = CheckStructInitArgs(StructTy->GetStruct(), Alloc->Loc, Alloc->Values, Alloc->VarArgsPassAlong);
    } else if (!Alloc->Values.empty()) {
        if (Alloc->Values.size() > 1) {
            Error(Alloc->Loc, "Too many values to initialize type '%s'", Alloc->TypeToAlloc->ToString());
        } else {
            CheckNode(Alloc->Values[0].E);
            if (Alloc->Values[0].E->Ty != Context.ErrorType) {
                if (TypeToAlloc->GetKind() == TypeKind::Array && Alloc->Values[0].E->IsNot(AstKind::ARRAY)) {
                    Error(Alloc->Values[0].ExpandedLoc, "The array must be created inline");
                }

                if (!IsAssignableTo(Alloc->TypeToAlloc, Alloc->Values[0].E)) {
                    Error(Alloc->Values[0].ExpandedLoc,
                        "Cannot initialize allocation of type '%s' with type '%s'",
                        Alloc->TypeToAlloc->ToString(),
                        Alloc->Values[0].E->Ty->ToString());
                } else {
                    CreateCast(Alloc->Values[0].E, Alloc->TypeToAlloc);
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
        Alloc->Ty = PointerType::Create(TypeToAlloc->AsArrayTy()->GetBaseType(), Context);
    } else {
        Alloc->Ty = PointerType::Create(TypeToAlloc, Context);
    }
}

void arco::SemAnalyzer::CheckSizeOf(SizeOf* SOf) {
    FixupType(SOf->TypeToGetSizeOf);
    if (SOf->TypeToGetSizeOf->GetKind() == TypeKind::Struct) {
        StructType* StructTy = SOf->TypeToGetSizeOf->AsStructType();
        if (StructTy->GetStruct()->IsBeingChecked) {
            Error(SOf, "Cannot get sizeof struct '%s' because the type is incomplete",
                StructTy->ToString());
        }
    }
}

void arco::SemAnalyzer::CheckTypeOf(TypeOf* TOf) {
    if (!Context.StdTypeStruct) {
        Error(TOf, "Cannot use typeof operator when there is no standard library");
        return;
    }
    FixupType(TOf->TypeToGetTypeOf);
    TOf->Ty = PointerType::Create(StructType::Create(Context.StdTypeStruct, Context), Context);
    TOf->HasConstAddress = true;
    TOf->IsFoldable = false; // TODO: change?
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
        Error(Rg->LHS, "Expected indexable type for range. Found type '%s'", Rg->LHS->Ty->ToString());
        Errors = true;
    }
    if (!Rg->RHS->Ty->IsInt() &&
            Rg->RHS->Ty->GetRealKind() != TypeKind::Enum) {
        Error(Rg->RHS, "Expected indexable type for range. Found type '%s'", Rg->RHS->Ty->ToString());
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

void arco::SemAnalyzer::CheckCondition(Expr* Cond, const char* PreErrorText) {
    CheckNode(Cond);
    if (Cond->Ty == Context.ErrorType) return;
    if (!IsComparable(Cond->Ty)) {
        Error(Cond, "%s condition expected to be type 'bool' but found type '%s'",
             PreErrorText, Cond->Ty->ToString());
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
    if (FromTy->GetRealKind() == TypeKind::Enum || ToTy->GetRealKind() == TypeKind::Enum) {
        if (ToTy->GetRealKind()== TypeKind::Enum) {
            return ToTy->Equals(FromTy);
        }

        // Enums have special rules for assignments since they
        // can assign to whatever their index type is.
        EnumDecl* Enum = static_cast<StructType*>(FromTy)->GetEnum();
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
            if (FromTy->IsSystemInt()) {
                if (FromExpr && FromExpr->IsFoldable) {
                    // TODO: Should check if the value is less than 32 bits
                    // for better system compatibility?
                    return true;
                }
                // Otherwise trying to assign a non-foldable value to
                // a explicit type should require a cast.
                return false;
            }

            if (ToTy->GetTrivialTypeSizeInBytes() >= FromTy->GetTrivialTypeSizeInBytes()) {
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
                    case TypeKind::Int8:          return RANGE(i8, Num->SignedIntValue);
                    case TypeKind::Int16:         return RANGE(i16, Num->SignedIntValue);
                    case TypeKind::Int32:         return RANGE(i32, Num->SignedIntValue);
                    case TypeKind::Int64:         return RANGE(i64, Num->SignedIntValue);
                    case TypeKind::UInt8:  return POS_RANGE(u8, Num->SignedIntValue);
                    case TypeKind::UInt16: return POS_RANGE(u16, Num->SignedIntValue);
                    case TypeKind::UInt32: return POS_RANGE(u32, Num->SignedIntValue);
                    case TypeKind::UInt64: return POS_RANGE(u64, Num->SignedIntValue);
                    case TypeKind::Char:	      return RANGE(i8, Num->SignedIntValue);
                    }
                } else {
                    switch (ToTy->GetKind()) {
                    case TypeKind::Int8:          return RANGE(i8, Num->UnsignedIntValue);
                    case TypeKind::Int16:         return RANGE(i16, Num->UnsignedIntValue);
                    case TypeKind::Int32:         return RANGE(i32, Num->UnsignedIntValue);
                    case TypeKind::Int64:         return RANGE(i64, Num->UnsignedIntValue);
                    case TypeKind::UInt8:  return POS_RANGE(u8, Num->UnsignedIntValue);
                    case TypeKind::UInt16: return POS_RANGE(u16, Num->UnsignedIntValue);
                    case TypeKind::UInt32: return POS_RANGE(u32, Num->UnsignedIntValue);
                    case TypeKind::UInt64: return POS_RANGE(u64, Num->UnsignedIntValue);
                    case TypeKind::Char:	      return RANGE(i8, Num->UnsignedIntValue);
                    }
                }
            }
            return false;
        }
        return false;
#undef RANGE
#undef POS_RANGE
    }
    case TypeKind::Int:
    case TypeKind::UInt: {
        if (FromTy->IsSystemInt()) return true;
        if (!FromTy->IsInt()) return false;
        return FromTy->GetTrivialTypeSizeInBytes() <= 4;
    }
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
        return FromTy == Context.CStrType || FromTy->Equals(Context.CharPtrType);
    }
    case TypeKind::Pointer: {
        if (FromTy == Context.NullType)
            return true;
        else if (FromTy->GetKind() == TypeKind::Array) {
            PointerType* ToPtrTy     = ToTy->AsPointerTy();
            ArrayType*   FromArrayTy = FromTy->AsArrayTy();
            if (FromArrayTy->GetDepthLevel() == 1) {
                return ToPtrTy->GetElementType()->Equals(FromArrayTy->GetElementType()) ||
                       ToPtrTy->Equals(Context.VoidPtrType);
            }
            return false;
        } else if (FromTy->IsPointer()) {
            if (ToTy->Equals(Context.VoidPtrType)) {
                return true; // Can always assign to void*
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
        if (Context.StdAnyStruct) {
            StructDecl* Struct = ToTy->AsStructType()->GetStruct();
            if (Struct == Context.StdAnyStruct) {
                // Still need to make sure the type is sensible.
                switch (FromTy->GetKind()) {
                case TypeKind::Null:
                    return false;
                case TypeKind::Array: {
                    ArrayType* ArrayTy = FromTy->AsArrayTy();
                    Type* BaseTy = ArrayTy->GetBaseType();
                    TypeKind Kind = BaseTy->GetKind();
                    if (Kind == TypeKind::Null) {
                        return false;
                    } else if (Kind == TypeKind::Import || Kind == TypeKind::FuncRef ||
                               Kind == TypeKind::StructRef || Kind == TypeKind::EnumRef ||
                               Kind == TypeKind::EmptyArrayElm) {
                        return false;
                    }
                    break;
                }
                case TypeKind::Import:
                case TypeKind::FuncRef:
                case TypeKind::StructRef:
                case TypeKind::EnumRef:
                case TypeKind::Void:
                case TypeKind::EmptyArrayElm:
                    return false;
                }

                return true;
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
    case TypeKind::UInt:
    case TypeKind::Char:
        if (FromTy->IsNumber() || FromTy->IsPointer() || FromTy->GetKind() == TypeKind::Bool) {
            // Allow pointers/numbers/bools to cast to integers.
            return true;
        }
        return false;
    case TypeKind::Float32:
    case TypeKind::Float64:
        return FromTy->IsNumber();
    case TypeKind::Pointer:
    case TypeKind::CStr:
        if (FromTy->IsNumber() || FromTy->IsPointer()) {
            // Allow numbers/pointers to cast to pointers.
            return true;
        }
        return IsAssignableTo(ToTy, FromTy, nullptr);
    case TypeKind::Struct:
        // Prevent any craziness that might come with casting to Any
        return false;
    default:
        return IsAssignableTo(ToTy, FromTy, nullptr);
    }
}

bool arco::SemAnalyzer::ViolatesConstAssignment(VarDecl* DestVar, Expr* Assignment) {
    return ViolatesConstAssignment(DestVar->Ty, DestVar->HasConstAddress, Assignment);
}

bool arco::SemAnalyzer::ViolatesConstAssignment(Type* DestTy, bool DestConstAddress, Expr* Assignment) {
    TypeKind DestTyKind = DestTy->GetKind();
    return !DestConstAddress           &&
           Assignment->HasConstAddress &&
           (DestTyKind == TypeKind::Pointer || DestTyKind == TypeKind::CStr ||
            DestTyKind == TypeKind::Array   || DestTyKind == TypeKind::Function);
}

bool arco::SemAnalyzer::FixupType(Type* Ty, bool AllowDynamicArrays) {
    if (Ty->GetKind() == TypeKind::Array) {
        return FixupArrayType(Ty->AsArrayTy(), AllowDynamicArrays);
    } else if (Ty->GetKind() == TypeKind::Struct) {
        return FixupStructType(Ty->AsStructType());
    } else if (Ty->GetKind() == TypeKind::Pointer) {
        PointerType* PointerTy = Ty->AsPointerTy();
        Type* ElementType = PointerTy->GetElementType();
        if (!FixupType(ElementType, AllowDynamicArrays)) {
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
    } else if (Ty->GetKind() == TypeKind::Slice) {
        SliceType* SliceTy = Ty->AsSliceTy();
        Type* ElementType = SliceTy->GetElementType();
        if (!FixupType(ElementType, AllowDynamicArrays)) {
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
    } else if (Ty->GetKind() == TypeKind::Function) {
        FunctionType* FuncTy = Ty->AsFunctionType();
        for (auto ParamTy : FuncTy->ParamTypes) {
            if (!FixupType(ParamTy.Ty)) {
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

bool arco::SemAnalyzer::FixupArrayType(ArrayType* ArrayTy, bool AllowDynamic) {
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
    
    SourceLoc ErrorLoc = ArrayTy->GetLengthExprErrorLoc();

    if (!LengthExpr->IsFoldable && !AllowDynamic) {
        Error(ErrorLoc, "Could not compute the length of the array at compile time");
        return false;
    } else if (!LengthExpr->Ty->IsInt()) {
        Error(ErrorLoc, "The length of the array is expected to be an integer");
        return false;
    }

    IRGenerator IRGen(Context);
    
    if (LengthExpr->IsFoldable) {
        llvm::ConstantInt* LLInt =
            llvm::cast<llvm::ConstantInt>(IRGen.GenRValue(LengthExpr));

        if (LLInt->isZero()) {
            Error(ErrorLoc, "The length of the array cannot be zero");
            return false;
        } else if (LengthExpr->Ty->IsSigned() && LLInt->isNegative()) {
            Error(ErrorLoc, "The length of the array cannot be negative");
            return false;
        }

        ArrayTy->AssignLength(LLInt->getZExtValue());
    }

    // NOTE: At this point the element type should have it's uniqueTypeId
    // correctly set.
    Type* ElementType = ArrayTy->GetElementType();
    if (!FixupType(ElementType, AllowDynamic)) {
        return false;
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

bool arco::SemAnalyzer::FixupStructType(StructType* StructTy) {
    Identifier StructName = StructTy->GetStructName();
    Decl* FoundDecl = nullptr;

    auto Itr = FScope->Imports.find(StructName);
    if (Itr != FScope->Imports.end() && Itr->second.Decl) {
        FoundDecl = Itr->second.Decl;
    }

    if (!FoundDecl) {
        auto Itr = Mod->DefaultNamespace->Decls.find(StructName);
        if (Itr != Mod->DefaultNamespace->Decls.end() && Itr->second->IsStructLike()) {
            FoundDecl = Itr->second;
        }
    }

    if (!FoundDecl && FScope->UniqueNSpace) {
        auto Itr = FScope->UniqueNSpace->Decls.find(StructName);
        if (Itr != FScope->UniqueNSpace->Decls.end() && Itr->second->IsStructLike()) {
            FoundDecl = Itr->second;
        }
    }

    if (!FoundDecl) {
        Error(StructTy->GetErrorLoc(), "Could not find struct, or enum by name '%s'", StructTy->GetStructName());
        return false;
    }

    SemAnalyzer Analyzer(Context, FoundDecl);
    if (FoundDecl->Is(AstKind::STRUCT_DECL)) {
        StructDecl* Struct = static_cast<StructDecl*>(FoundDecl);
        StructTy->AssignStruct(Struct);
        StructTy->SetUniqueId(Struct->UniqueTypeId);
        Analyzer.CheckStructDecl(Struct);
    } else if (FoundDecl->Is(AstKind::ENUM_DECL)) {
        EnumDecl* Enum = static_cast<EnumDecl*>(FoundDecl);
        StructTy->AssignEnum(Enum);
        StructTy->SetUniqueId(Enum->UniqueTypeId);
        Analyzer.CheckEnumDecl(Enum);
    } else {
        assert(!"Not handled");
    }
    
    return true;
}

void arco::SemAnalyzer::CheckModifibility(Expr* LValue) {
    if (!IsLValue(LValue)) {
        Error(LValue, "Expected to be a modifiable value");
    } else {
        if (LValue->HasConstAddress) {
            // We only want to prevent modification of underlying memory.
            if (LValue->Is(AstKind::FIELD_ACCESSOR)) {
                Error(LValue, "Cannot modify field of const memory");
                return;
            }

            TypeKind K = LValue->Ty->GetKind();
            if (K != TypeKind::Pointer &&
                K != TypeKind::CStr &&
                K != TypeKind::Array
                ) {
                Error(LValue, "Cannot modify a variable with const memory");
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
    for (const FuncDecl* Func1 : FuncList) {
        for (const FuncDecl* Func2 : FuncList) {
            if (Func1 == Func2) continue;
            if (Func1->Params.size() != Func2->Params.size()) continue;
            if (std::equal(Func1->Params.begin(),
                            Func1->Params.end(),
                            Func2->Params.begin(),
                            [](const VarDecl* Param1, const VarDecl* Param2) {
                return Param1->Ty->Equals(Param2->Ty);
            })) {
                FileScope* FScope = Func1->FScope;
                Logger Log(FScope->Path.c_str(), FScope->Buffer);
                Log.BeginError(Func1->Loc, 
                    "Duplicate declaration of %s '%s'",
                    Func1->IsConstructor ? "constructor" : "function",
                    Func1->Name);
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
    }
}

void arco::SemAnalyzer::DisplayErrorForTypeMismatch(const char* ErrMsg, SourceLoc ErrorLoc,
                                                    Expr* FromExpr, Type* ToTy) {
    Log.BeginError(ErrorLoc, ErrMsg, FromExpr->Ty->ToString(), ToTy->ToString());
    DisplayNoteInfoForTypeMismatch(FromExpr, ToTy);
    Log.EndError();
}
