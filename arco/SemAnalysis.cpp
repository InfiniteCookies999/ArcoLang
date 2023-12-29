#include "SemAnalysis.h"

#include "Context.h"
#include "IRGen.h"

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

void arco::SemAnalyzer::ResolveStructImports(FileScope* FScope) {
	for (auto& [ModName, StructImport] : FScope->StructImports) {
		auto Itr = StructImport.Mod->Structs.find(StructImport.StructName);
		if (Itr == StructImport.Mod->Structs.end()) {
			Logger Log(FScope->Path.c_str(), FScope->Buffer);
			Log.BeginError(StructImport.ErrorLoc, "Could not find struct '%s' in module '%s'",
				StructImport.StructName, ModName);
			Log.EndError();
			StructImport.Struct = nullptr;
			continue;
		}

		StructImport.Struct = Itr->second;
	}
}

void arco::SemAnalyzer::CheckForDuplicateFuncDeclarations(Module* Mod) {
	for (const auto& [Name, FuncList] : Mod->Funcs) {
		CheckForDuplicateFuncs(FuncList);
	}
	for (const auto& [Name, Struct] : Mod->Structs) {
		CheckForDuplicateFuncs(Struct->Constructors);
		for (const auto& [Name, FuncList] : Struct->Funcs) {
			CheckForDuplicateFuncs(FuncList);
		}
	}
}

void arco::SemAnalyzer::CheckFuncDecl(FuncDecl* Func) {
	if (Func->ParsingError) return;
	if (Func->HasBeenChecked) return;

	Func->HasBeenChecked = true;

	Context.UncheckedDecls.erase(Func);

	CFunc   = Func;
	CStruct = Func->Struct;
	FScope  = Func->FScope;

	// -- DEBUG
	// llvm::outs() << "Checking function: " << Func->Name << "\n";

	CheckFuncParamTypes(Func);

	Scope FuncScope;
	CheckScopeStmts(Func->Scope, FuncScope);
}

void arco::SemAnalyzer::CheckStructDecl(StructDecl* Struct) {
	if (Struct->ParsingError) return;
	if (Struct->HasBeenChecked) return;

	Struct->HasBeenChecked = true;

	Context.UncheckedDecls.erase(Struct);

	FScope  = Struct->FScope;
	CStruct = Struct;

	for (VarDecl* Field : Struct->Fields) {
		CheckVarDecl(Field);
		if (Field->Assignment) {
			Struct->FieldsHaveAssignment = true;
		}
	}
}

void arco::SemAnalyzer::CheckFuncParamTypes(FuncDecl* Func) {
	if (Func->ParamTypesChecked) return;

	Func->ParamTypesChecked = true;

	CFunc   = Func;
	CStruct = Func->Struct;
	FScope  = Func->FScope;

	if (!FixupType(Func->RetTy)) {
		Func->RetTy = Context.ErrorType;
	}

	llvm::SmallVector<VarDecl*, 2> Params = Func->Params;
	for (VarDecl* Param : Params) {
		if (!FixupType(Param->Ty)) {
			Param->Ty = Context.ErrorType;
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
		CheckIdentRef(static_cast<IdentRef*>(Node), false, Mod);
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
		case AstKind::FUNC_CALL:
		case AstKind::BREAK:
		case AstKind::CONTINUE:
		case AstKind::NESTED_SCOPE:
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
		default:
			Error(Stmt, "Incomplete statement");
			continue;
		}

		CheckNode(Stmt);
	}

	LocScope = LocScope->Parent;
}

void arco::SemAnalyzer::CheckVarDecl(VarDecl* Var) {
	if (Var->ParsingError) return;
	if (Var->HasBeenChecked) return;

	Var->IsBeingChecked = true;
	Var->HasBeenChecked = true;

	FScope = Var->FScope;

	if (Var->IsGlobal) {
		Context.UncheckedDecls.erase(Var);
		CGlobal = Var;
	}
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

	if (!FixupType(Var->Ty)) {
		VAR_YIELD(, true);
	}
	
	if (Var->IsGlobal) {
		Context.RequestGen(Var);
	}

	if (Var->Assignment) {
		CheckNode(Var->Assignment);
		YIELD_ERROR_WHEN(Var, Var->Assignment);
	
		if (Var->Ty->GetKind() == TypeKind::Array &&
			!static_cast<ArrayType*>(Var->Ty)->GetLengthExpr()) {

			if (Var->Assignment->IsNot(AstKind::ARRAY)) {
				VAR_YIELD(Error(Var, "Expected array declaration for implicit array type"), true);
			}

			ArrayType* ImplicitArrayType = static_cast<ArrayType*>(Var->Ty);
			ArrayType* FromArrayType     = static_cast<ArrayType*>(Var->Assignment->Ty);
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
					FromArrayType     = static_cast<ArrayType*>(FromArrayType->GetElementType());
					ImplicitArrayType = static_cast<ArrayType*>(ImplicitArrayType->GetElementType());
					ImplicitArrayType->AssignLength(FromArrayType->GetLength());
				} else {
					break;
				}
			}

		} else if (IsAssignableTo(Var->Ty, Var->Assignment)) {
			CreateCast(Var->Assignment, Var->Ty);
		} else {
			VAR_YIELD(
				Error(Var, "Cannot assign value of type '%s' to variable of type '%s'",
				      Var->Assignment->Ty->ToString(),
					  Var->Ty->ToString()),
				false);
		}
	} else {
		// No assignment.

		StructDecl* StructForTy = nullptr;
		if (Var->Ty->GetKind() == TypeKind::Struct) {
			StructForTy = static_cast<StructType*>(Var->Ty)->GetStruct();
		} else if (Var->Ty->GetKind() == TypeKind::Array) {
			ArrayType* ArrayTy = static_cast<ArrayType*>(Var->Ty);
			Type* BaseTy = ArrayTy->GetBaseType();
			if (BaseTy->GetKind() == TypeKind::Struct) {
				StructForTy = static_cast<StructType*>(BaseTy)->GetStruct();
			}
		}

		if (StructForTy) {
			if (!StructForTy->Constructors.empty() && !StructForTy->DefaultConstructor) {
				VAR_YIELD(Error(Var, "No default constructor to initialize the variable"), false);
			} else if (StructForTy->DefaultConstructor) {
				Context.RequestGen(StructForTy->DefaultConstructor);
			}
		}
	}

	VAR_YIELD(, false);
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
		CheckCondition(Loop->Cond, "Loop");
	}

	++LoopDepth;
	Scope LoopScope;
	CheckScopeStmts(Loop->Scope, LoopScope);
	--LoopDepth;
}

void arco::SemAnalyzer::CheckRangeLoop(RangeLoopStmt* Loop) {
	for (VarDecl* Decl : Loop->Decls) {
		CheckNode(Decl);
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

static Type* DetermineTypeFromIntTypes(ArcoContext& Context, Type* LTy, Type* RTy) {
	bool IsSigned = LTy->IsSigned() || RTy->IsSigned();
	if (LTy->IsSystemInt() && RTy->IsSystemInt()) {
		return IsSigned ? Context.IntType : Context.UIntType;
	} else if (LTy->IsSystemInt()) {
		// Want to take on the type of the explicit type.
		return Type::GetIntTypeBasedOnByteSize(RTy->GetTrivialTypeSizeInBytes(), IsSigned, Context);
	} else if (RTy->IsSystemInt()) {
		// Want to take on the type of the explicit type.
		return Type::GetIntTypeBasedOnByteSize(LTy->GetTrivialTypeSizeInBytes(), IsSigned, Context);
	} else {
		ulen LargerMemSize = max(LTy->GetTrivialTypeSizeInBytes(), RTy->GetTrivialTypeSizeInBytes());
		return Type::GetIntTypeBasedOnByteSize(LargerMemSize, IsSigned, Context);
	}
}

static Type* DetermineTypeFromNumberTypes(ArcoContext& Context, Type* LTy, Type* RTy) {
	if (LTy->IsInt() && RTy->IsInt()) {
		return DetermineTypeFromIntTypes(Context, LTy, RTy);
	} else {
		assert(!"Not handling non-integer numbers yet!");
		return nullptr;
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
				Error(BinOp, "Cannot assign value of type '%s' to variable of type '%s'",
					RTy->ToString(), LTy->ToString());
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
					BinOp->Ty = PointerType::Create(static_cast<ArrayType*>(LTy)->GetElementType(), Context);
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
					BinOp->Ty = PointerType::Create(static_cast<ArrayType*>(RTy)->GetElementType(), Context);
				}
			}

		} else {
			// Not pointer arithmetic
			Type* ToType = DetermineTypeFromNumberTypes(Context, LTy, RTy);
			
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

		Type* ToType = DetermineTypeFromNumberTypes(Context, LTy, RTy);

		if (BinOp->Op == '/' && BinOp->RHS->IsFoldable) {
			IRGenerator IRGen(Context);
			llvm::Constant* LLInt = llvm::cast<llvm::Constant>(IRGen.GenRValue(BinOp->RHS));
			if (LLInt->isZeroValue()) {
				Error(BinOp, "Division by zero");
			}
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
			llvm::Constant* LLInt = llvm::cast<llvm::Constant>(IRGen.GenRValue(BinOp->RHS));
			if (LLInt->isZeroValue()) {
				Error(BinOp, "Division by zero");
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

			Type* ToType = DetermineTypeFromIntTypes(Context, LTy, RTy);

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

			Type* ToType = DetermineTypeFromNumberTypes(Context, LTy, RTy);

			CreateCast(BinOp->LHS, ToType);
			CreateCast(BinOp->RHS, ToType);
			BinOp->Ty = Context.BoolType;
		}
		break;
	}
	case TokenKind::AMP_AMP: case TokenKind::BAR_BAR: {
		assert(!"not handling yet!");
		break;
	}
	default:
		assert(!"Failed to implement binary operator check");
		break;
	}

#undef OPERATOR_CANNOT_APPLY
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
		if (!IsLValue(UniOp->Value)) {
			Error(UniOp, "Operator '%s' requires the value to be modifiable",
				Token::TokenKindToString(UniOp->Op, Context));
		}

		UniOp->HasConstAddress = UniOp->Value->HasConstAddress;
		UniOp->Ty = PointerType::Create(ValTy, Context);
		break;
	}
	case '*': {
		if (!ValTy->IsPointer()) {
			OPERATOR_CANNOT_APPLY(ValTy);
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
	default:
		assert(!"Unhandled unary check");
		break;
	}
}

void arco::SemAnalyzer::CheckIdentRef(IdentRef* IRef,
	                                  bool ExpectsFuncCall,
	                                  Module* ModToLookup,
	                                  StructDecl* StructToLookup) {

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
			
			auto Itr = ModToLookup->Funcs.find(IRef->Ident);
			if (Itr != ModToLookup->Funcs.end()) {
				IRef->Funcs   = &Itr->second;
				IRef->RefKind = IdentRef::RK::Funcs;
			}
		}
	};

	auto SearchForVars = [=]() {
		if (StructToLookup) {
			auto Itr = std::find_if(
				StructToLookup->Fields.begin(),
				StructToLookup->Fields.end(),
			[=](VarDecl* Field) {
				return Field->Name == IRef->Ident;
			});
			if (Itr != StructToLookup->Fields.end()) {
				IRef->Var     = *Itr;
				IRef->RefKind = IdentRef::RK::Var;
			}
		} else {
			auto Itr = ModToLookup->GlobalVars.find(IRef->Ident);
			if (Itr != ModToLookup->GlobalVars.end()) {
				IRef->Var     = Itr->second;
				IRef->RefKind = IdentRef::RK::Var;
			}
		}
	};

	// If it expects a function then we search the
	// function first otherwise we search for a variable
	// first.
	if (!IRef->IsFound() && ExpectsFuncCall) {
		SearchForFuncs();
		if (!IRef->IsFound())
			SearchForVars();
	} else if (!IRef->IsFound()) {
		SearchForVars();
		if (!IRef->IsFound())
			SearchForFuncs();
	}

	if (!IRef->IsFound() && ModToLookup == Mod) {
		auto Itr = FScope->ModImports.find(IRef->Ident);
		if (Itr != FScope->ModImports.end()) {
			IRef->Mod     = Itr->second;
			IRef->RefKind = IdentRef::RK::Import;
		}
	}

	IRef->IsFoldable = false;

	switch (IRef->RefKind) {
	case IdentRef::RK::Var: {
		VarDecl* VarRef = IRef->Var;
		IRef->Ty = VarRef->Ty;
		if (VarRef->IsGlobal || VarRef->IsField()) {
			EnsureChecked(IRef->Loc, VarRef);
		}

		// TODO: In the future could check if the variable is marked const as well.
		IRef->HasConstAddress = VarRef->Ty->GetKind() == TypeKind::CStr;
		break;
	}
	case IdentRef::RK::Import: {
		IRef->Ty = Context.ImportType;
		break;
	}
	case IdentRef::RK::Funcs: {
		// TODO: Set type information.
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
		CheckIdentRef(static_cast<IdentRef*>(Site), false, Mod);
	} else {
		CheckNode(Site);
	}
	YIELD_ERROR_WHEN(FieldAcc, Site);
	FieldAcc->HasConstAddress = Site->HasConstAddress;
	FieldAcc->IsFoldable      = Site->IsFoldable;

	// Checking for .length operator
	if (Site->Ty->GetKind() == TypeKind::Array) {
		if (FieldAcc->Ident == Context.LengthIdentifier) {
			FieldAcc->IsArrayLength = true;
			FieldAcc->Ty = Context.IntType;
			return;
		}
	}

	if (Site->Ty == Context.ImportType) {
		IdentRef* IRef = static_cast<IdentRef*>(Site);
		CheckIdentRef(FieldAcc, ExpectsFuncCall, IRef->Mod);
		return;
	}

	if (!(Site->Ty->GetKind() == TypeKind::Struct ||
		  (Site->Ty->GetKind() == TypeKind::Pointer &&
		  static_cast<PointerType*>(Site->Ty)->GetElementType()->GetKind() == TypeKind::Struct)
		 )) {
		Error(FieldAcc, "Cannot access field of type '%s'", Site->Ty->ToString());
		YIELD_ERROR(FieldAcc);
	}

	StructType* StructTy;
	if (Site->Ty->GetKind() == TypeKind::Struct) {
		StructTy = static_cast<StructType*>(Site->Ty);
	} else {
		StructTy = static_cast<StructType*>(
			static_cast<PointerType*>(Site->Ty)->GetElementType());
	}

	CheckIdentRef(FieldAcc, ExpectsFuncCall, Mod, StructTy->GetStruct());
}

void arco::SemAnalyzer::CheckThisRef(ThisRef* This) {
	if (!CStruct) {
		Error(This, "Cannot use 'this' outside a struct scope");
		YIELD_ERROR(This);
	}
	This->IsFoldable = false;
	This->Ty = StructType::Create(CStruct, Context);
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
		CheckIdentRef(static_cast<IdentRef*>(Call->Site), true, Mod);
		break;
	case AstKind::FIELD_ACCESSOR:
		CheckFieldAccessor(static_cast<FieldAccessor*>(Call->Site), true);
		break;
	default:
		CheckNode(Call->Site);
		break;
	}
	YIELD_ERROR_WHEN(Call, Call->Site);

	FuncsList* Canidates = static_cast<IdentRef*>(Call->Site)->Funcs;

	Call->CalledFunc = CheckCallToCanidates(Call->Loc, Canidates, Call->Args);
	if (!Call->CalledFunc) {
		YIELD_ERROR(Call);
	}

	for (ulen i = 0; i < Call->Args.size(); i++) {
		Expr*    Arg   = Call->Args[i].E;
		VarDecl* Param = Call->CalledFunc->Params[i];
		CreateCast(Arg, Param->Ty);
	}

	Call->IsFoldable = false;
	Call->Ty = Call->CalledFunc->RetTy;

}

arco::FuncDecl* arco::SemAnalyzer::CheckCallToCanidates(SourceLoc ErrorLoc,
	                                                    FuncsList* Canidates,
	                                                    llvm::SmallVector<NonNamedValue, 2>& Args) {
	FuncDecl* Selected = FindBestFuncCallCanidate(Canidates, Args);
	if (!Selected) {
		DisplayErrorForNoMatchingFuncCall(ErrorLoc, Canidates, Args);
		return nullptr;
	}

	for (ulen i = 0; i < Args.size(); i++) {
		Expr*    Arg   = Args[i].E;
		VarDecl* Param = Selected->Params[i];
		CreateCast(Arg, Param->Ty);
	}

	Context.RequestGen(Selected);
	
	return Selected;
}

arco::FuncDecl* arco::SemAnalyzer::FindBestFuncCallCanidate(FuncsList* Canidates,
	                                                        const llvm::SmallVector<NonNamedValue, 2>& Args) {
	if (!Canidates) return nullptr;

	FuncDecl* Selection = nullptr;

	// TODO: Should select canidates based on if signs match or not?

	ulen LeastConflicts = std::numeric_limits<ulen>::max();
	for (ulen i = 0; i < Canidates->size(); i++) {
		FuncDecl* Canidate = (*Canidates)[i];
		if (!Canidate->ParamTypesChecked) {
			SemAnalyzer Analyzer(Context, Canidate);
			Analyzer.CheckFuncParamTypes(Canidate);
		}

		ulen NumConflicts = 0;
		if (!CompareAsCanidate(Canidate, Args, NumConflicts)) {
			continue;
		}

		if (NumConflicts < LeastConflicts) {
			Selection = Canidate;
			LeastConflicts = NumConflicts;
		}
	}
	return Selection;
}

bool arco::SemAnalyzer::CompareAsCanidate(FuncDecl* Canidate,
	                                      const llvm::SmallVector<NonNamedValue, 2>& Args,
	                                      ulen& NumConflicts) {
	if (Canidate->Params.size() != Args.size()) {
		return false;
	}

	for (ulen i = 0; i < Args.size(); i++) {
		Expr* Arg = Args[i].E;
		VarDecl* Param = Canidate->Params[i];
		if (!IsAssignableTo(Param->Ty, Arg)) {
			return false;
		}
		if (!Param->Ty->Equals(Arg->Ty)) {
			++NumConflicts;
		}
	}

	return true;
}

void arco::SemAnalyzer::DisplayErrorForNoMatchingFuncCall(SourceLoc ErrorLoc,
	                                                      FuncsList* Canidates,
			                                              const llvm::SmallVector<NonNamedValue, 2>& Args) {
	
	const char* CallType = (*Canidates)[0]->IsConstructor ? "constructor" : "function";

	if (Canidates && Canidates->size() == 1) {
		// Single canidate so explicit details about
		// how there is a mismatch between the call and
		// the function is given.
		
		bool EncounteredError = false;

		FuncDecl* SingleCanidate = (*Canidates)[0];

		const auto& Params = SingleCanidate->Params;
		for (ulen ArgCount = 0; ArgCount < Args.size(); ArgCount++) {
			if (ArgCount >= Params.size()) {
				if (EncounteredError)  Log.EndError();
				Log.BeginError(ErrorLoc, "Too many arguments for %s call", CallType);
				EncounteredError = true;
				break;
			}
			
			Expr*    Arg   = Args[ArgCount].E;
			VarDecl* Param = Params[ArgCount];

			if (!IsAssignableTo(Param->Ty, Arg->Ty, Arg)) {
				if (EncounteredError)  Log.EndError();
				Log.BeginError(Args[ArgCount].ExpandedLoc,
					"Cannot assign argument %s of type '%s' to parameter of type '%s'",
					ArgCount+1,
					Arg->Ty->ToString(), Param->Ty->ToString());
				EncounteredError = true;
			}
		}

		if (!EncounteredError) {
			Log.BeginError(ErrorLoc, "Could not find function for call");
		}

		// Displaying the function.
		std::string FuncDec = SingleCanidate->Name.Text.str();
		FuncDec += "(";
		for (ulen i = 0; i < SingleCanidate->Params.size(); i++) {
			VarDecl* Param = SingleCanidate->Params[i];
			FuncDec += Param->Ty->ToString();
			if (i != SingleCanidate->Params.size() - 1) {
				FuncDec += ", ";
			}
		}
		FuncDec += ")";

		Log.AddNoteLine([=](llvm::raw_ostream& OS) {
			OS << "Expected call to function declaration: " << FuncDec;
		});
		Log.EndError();

	} else {
		std::string FuncDef = "(";
		for (ulen i = 0; i < Args.size(); i++) {
			FuncDef += Args[i].E->Ty->ToString();
			if (i+1 != Args.size()) FuncDef += ", ";
		}
		FuncDef += ")";

		Error(ErrorLoc, "Could not find %s with parameter types '%s'",
			  CallType, FuncDef);
	}
}

void arco::SemAnalyzer::CheckArray(Array* Arr) {

	Type* ElmTypes = nullptr;
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

		if (!ElmTypes) {
			ElmTypes = Elm->Ty;
		} else if (ElmAreArrs) {
			// The sub-arrays must have exactly the same type.
			if (!ElmTypes->Equals(Elm->Ty)) {
				Error(Elm, "Array has incompatible sub-array elements");
			}
		} else {
			if (!IsAssignableTo(ElmTypes, Elm->Ty, nullptr)) {
				// Maybe the reserve is allowed.
				if (!IsAssignableTo(Elm->Ty, ElmTypes, nullptr)) {
					Error(Elm, "Array has incompatible elements. Array type: '%s', element type '%s'",
						ElmTypes->ToString(), Elm->Ty->ToString());
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

	// TODO: Expand to support access cstr types.
	TypeKind Kind = Access->Site->Ty->GetKind();
	if (!(Kind == TypeKind::Array || Kind == TypeKind::Pointer)) {
		Error(Access, "Cannot index non-array or pointer type. Type was '%s'",
			Access->Site->Ty->ToString());
		YIELD_ERROR(Access);
	}

	Access->IsFoldable = false;
	Access->Ty = static_cast<ContainerType*>(Access->Site->Ty)->GetElementType();

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
	StructType* StructTy = static_cast<StructType*>(StructInit->Ty);
	if (!FixupStructType(StructTy)) {
		YIELD_ERROR(StructInit);
	}

	StructDecl* Struct = StructTy->GetStruct();

	// TODO: May want to allow if the fields are foldable!
	StructInit->IsFoldable = false;

	bool ArgsHaveErrors = false;
	for (ulen i = 0; i < StructInit->Args.size(); i++) {
		NonNamedValue Value = StructInit->Args[i];
		CheckNode(Value.E);
		if (Value.E->Ty == Context.ErrorType) {
			ArgsHaveErrors = true;
		}
	}

	if (!Struct->Constructors.empty()) {
		// Calling constructor!

		if (ArgsHaveErrors) {
			return;
		}

		FuncDecl* CalledConstructor =
			CheckCallToCanidates(
				StructInit->Loc,
				&Struct->Constructors,
				StructInit->Args
				);

		StructInit->CalledConstructor = CalledConstructor;
		return;
	}

	for (ulen i = 0; i < StructInit->Args.size(); i++) {
		NonNamedValue Value = StructInit->Args[i];
		
		if (i >= Struct->Fields.size()) {
			Error(Value.ExpandedLoc, "Too many fields in initializer");
			return;
		}

		if (Value.E->Ty == Context.ErrorType) {
			continue;
		}

		Type* FieldTy = Struct->Fields[i]->Ty;
		if (!IsAssignableTo(FieldTy, Value.E)) {
			Error(Value.ExpandedLoc, "Cannot assign value of type '%s' to field of type '%s'",
				Value.E->Ty->ToString(), FieldTy->ToString());
		} else {
			CreateCast(Value.E, FieldTy);
		}
	}
}

void arco::SemAnalyzer::CheckHeapAlloc(HeapAlloc* Alloc) {
	Alloc->IsFoldable = false;

	// TODO: Need to make sure that when allocating structs that they
	// either have an available default constructor or that they
	// are provided the appropriate arguments for an alternative constructor.

	Type* TypeToAlloc = Alloc->TypeToAlloc;

	if (!FixupType(TypeToAlloc, true)) {
		YIELD_ERROR(Alloc);
	}

	if (TypeToAlloc->GetKind() == TypeKind::Array) {
		Alloc->Ty = PointerType::Create(static_cast<ArrayType*>(TypeToAlloc)->GetBaseType(), Context);
	} else {
		Alloc->Ty = PointerType::Create(TypeToAlloc, Context);
	}
}

void arco::SemAnalyzer::CheckCondition(Expr* Cond, const char* PreErrorText) {
	CheckNode(Cond);
	if (Cond->Ty == Context.ErrorType) return;
	if (!(Cond->Ty == Context.BoolType)) {
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
	switch (ToTy->GetKind()) {
	case TypeKind::Int:
	case TypeKind::UnsignedInt:
	case TypeKind::Int8:
	case TypeKind::Int16:
	case TypeKind::Int32:
	case TypeKind::Int64:
	case TypeKind::UnsignedInt8:
	case TypeKind::UnsignedInt16:
	case TypeKind::UnsignedInt32:
	case TypeKind::UnsignedInt64:
	case TypeKind::Char: {
		if (FromTy->IsInt()) {
			// TODO: Improvements!
			return true;
		}
		return false;
	}
	case TypeKind::CStr: {
		if (FromTy == Context.NullType)
			return true;
		else if (FromTy->GetKind() == TypeKind::Array) {
			ArrayType* FromArrayTy = static_cast<ArrayType*>(FromTy);
			return FromArrayTy->GetElementType() == Context.CharType;
		}
		return FromTy == Context.CStrType || FromTy->Equals(Context.CharPtrType);
	}
	case TypeKind::Pointer: {
		if (FromTy == Context.NullType)
			return true;
		else if (FromTy->GetKind() == TypeKind::Array) {
			PointerType* ToPtrTy     = static_cast<PointerType*>(ToTy);
			ArrayType*   FromArrayTy = static_cast<ArrayType*>(FromTy);
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
	case TypeKind::Array: {
		if (FromTy->GetKind() != TypeKind::Array) {
			return false;
		}

		ArrayType* ToArrayType   = static_cast<ArrayType*>(ToTy);
		ArrayType* FromArrayType = static_cast<ArrayType*>(FromTy);
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
				ToArrayType   = static_cast<ArrayType*>(ToTy);
				FromArrayType = static_cast<ArrayType*>(FromTy);
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
	case TypeKind::UnsignedInt8:
	case TypeKind::UnsignedInt16:
	case TypeKind::UnsignedInt32:
	case TypeKind::UnsignedInt64:
	case TypeKind::Int:
	case TypeKind::UnsignedInt:
		if (FromTy->IsNumber() || FromTy->IsPointer() || FromTy->GetKind() == TypeKind::Bool) {
			// Allow pointers/numbers/bools to cast to integers.
			return true;
		}
		return false;
	case TypeKind::Pointer:
		if (FromTy->IsNumber() || FromTy->IsPointer()) {
			// Allow numbers/pointers to cast to pointers.
			return true;
		}
		return IsAssignableTo(ToTy, FromTy, nullptr);
	default:
		return IsAssignableTo(ToTy, FromTy, nullptr);
	}
}

bool arco::SemAnalyzer::FixupType(Type* Ty, bool AllowDynamicArrays) {
	if (Ty->GetKind() == TypeKind::Array) {
		return FixupArrayType(static_cast<ArrayType*>(Ty), AllowDynamicArrays);
	} else if (Ty->GetKind() == TypeKind::Struct) {
		return FixupStructType(static_cast<StructType*>(Ty));
	} else if (Ty->GetKind() == TypeKind::Pointer) {
		return FixupType(static_cast<PointerType*>(Ty)->GetElementType(), AllowDynamicArrays);
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

	if (!FixupType(ArrayTy->GetElementType(), AllowDynamic)) {
		return false;
	}
	return true;
}

bool arco::SemAnalyzer::FixupStructType(StructType* StructTy) {
	Identifier StructName = StructTy->GetStructName();
	StructDecl* Struct;
	auto Itr = FScope->StructImports.find(StructName);
	if (Itr == FScope->StructImports.end()) {
		auto Itr2 = Mod->Structs.find(StructName);
		if (Itr2 == Mod->Structs.end()) {
			Error(StructTy->GetErrorLoc(), "Could not find struct by name '%s'", StructTy->GetStructName());
			return false;
		}
		Struct = Itr2->second;
	} else {
		Struct = Itr->second.Struct;
	}
	if (!Struct) {
		// This happens due to their being a valid import for the struct
		// but the import does not properly map to a struct.
		return false;
	}
	SemAnalyzer Analyzer(Context, Struct);
	Analyzer.CheckStructDecl(Struct);
	StructTy->AssignStruct(Struct);
	return true;
}

void arco::SemAnalyzer::CheckModifibility(Expr* LValue) {
	if (!IsLValue(LValue)) {
		Error(LValue, "Expected to be a modifiable value");
	} else {
		if (LValue->HasConstAddress) {
			// We only want to prevent modification of underlying memory.
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
