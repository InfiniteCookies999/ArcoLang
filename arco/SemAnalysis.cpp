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

void arco::SemAnalyzer::CheckFuncDecl(FuncDecl* Func) {
	if (Func->ParsingError) return;

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
	case AstKind::NUMBER_LITERAL:
	case AstKind::STRING_LITERAL:
	case AstKind::NULLPTR:
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

		CheckNode(Stmt);
	}

	LocScope = LocScope->Parent;
}

void arco::SemAnalyzer::CheckVarDecl(VarDecl* Var) {
	if (Var->ParsingError) return;

	if (!FixupType(Var->Ty)) {
		YIELD_ERROR(Var);
	}
	
	if (Var->Assignment) {
		CheckNode(Var->Assignment);
		YIELD_ERROR_WHEN(Var, Var->Assignment);
	
		if (Var->Ty->GetKind() == TypeKind::Array &&
			!static_cast<ArrayType*>(Var->Ty)->GetLengthExpr()) {

			if (Var->Assignment->IsNot(AstKind::ARRAY)) {
				Error(Var, "Expected array declaration for implicit array type");
				YIELD_ERROR(Var);
			}

			ArrayType* ImplicitArrayType = static_cast<ArrayType*>(Var->Ty);
			ArrayType* FromArrayType     = static_cast<ArrayType*>(Var->Assignment->Ty);
			if (ImplicitArrayType->GetDepthLevel() != FromArrayType->GetDepthLevel()) {
				Error(Var, "Incompatible depth with initializer array for implicit array type");
				YIELD_ERROR(Var);
			}

			if (!IsAssignableTo(ImplicitArrayType->GetBaseType(),
				                FromArrayType->GetBaseType(),
				                nullptr)) {
				Error(Var,
					  "Cannot assign array with element types '%s' to implicit array element types '%s'",
					FromArrayType->GetBaseType(),  
					ImplicitArrayType->GetBaseType());
				YIELD_ERROR(Var);
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
			Error(Var, "Cannot assign value of type '%s' to variable of type '%s'",
				Var->Assignment->Ty->ToString(), Var->Ty->ToString());
		}
	}
}

void arco::SemAnalyzer::CheckReturn(ReturnStmt* Return) {
	LocScope->FoundTerminal  = true;
	LocScope->AllPathsReturn = true;
	
	if (Return->Value) {
		CheckNode(Return->Value);
		YIELD_IF_ERROR(Return->Value);
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
				// TODO: Will also want to make sure it is not global!
				if (!Var->IsParam() && !Var->IsField()) {
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
		if (LTy->GetKind() == TypeKind::Pointer || LTy->GetKind() == TypeKind::Null) {
			assert(!"pointer arithmetic not supported yet!");
		} else if (RTy->GetKind() == TypeKind::Pointer || RTy->GetKind() == TypeKind::Null) {
			assert(!"pointer arithmetic not supported yet!");
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

		UniOp->Ty = ValTy;
		break;
	}
	case '&': {
		if (!IsLValue(UniOp->Value)) {
			Error(UniOp, "Operator '%s' requires the value to be modifiable",
				Token::TokenKindToString(UniOp->Op, Context));
		}

		UniOp->Ty = PointerType::Create(ValTy, Context);
		break;
	}
	case '*': {
		if (!ValTy->IsPointer()) {
			OPERATOR_CANNOT_APPLY(ValTy);
		}

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
			// TODO: Search for global variables!
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

	if (Site->Ty->GetKind() != TypeKind::Struct) {
		Error(FieldAcc, "Cannot access field of type '%s'", Site->Ty->ToString());
		YIELD_ERROR(FieldAcc);
	}

	StructType* StructTy = static_cast<StructType*>(Site->Ty);
	StructDecl* Struct = StructTy->GetStruct();

	CheckIdentRef(FieldAcc, ExpectsFuncCall, Mod, Struct);
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

	Call->CalledFunc = FindBestFuncCallCanidate(Canidates, Call->Args);
	if (!Call->CalledFunc) {
		DisplayErrorForNoMatchingFuncCall(Call, Canidates);
		YIELD_ERROR(Call);
	}

	for (ulen i = 0; i < Call->Args.size(); i++) {
		Expr*    Arg   = Call->Args[i].E;
		VarDecl* Param = Call->CalledFunc->Params[i];
		CreateCast(Arg, Param->Ty);
	}

	Call->IsFoldable = false;
	Call->Ty = Call->CalledFunc->RetTy;

	Context.RequestGen(Call->CalledFunc);

}

arco::FuncDecl* arco::SemAnalyzer::FindBestFuncCallCanidate(FuncsList* Canidates,
	                                                        llvm::SmallVector<NonNamedValue, 2>& Args) {
	if (!Canidates) return nullptr;

	FuncDecl* Selection = nullptr;

	// TODO: Should select canidates based on if signs match or not?

	ulen LeastConflicts = std::numeric_limits<ulen>::max();
	for (ulen i = 0; i < Canidates->size(); i++) {
		FuncDecl* Canidate = (*Canidates)[i];
		CheckFuncParamTypes(Canidate);

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
	                                      llvm::SmallVector<NonNamedValue, 2>& Args,
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

void arco::SemAnalyzer::DisplayErrorForNoMatchingFuncCall(FuncCall* Call, FuncsList* Canidates) {
	
	const char* CallType = "function";

	bool EncounteredError = false;
	if (Canidates && Canidates->size() == 1) {
		// Single canidate so explicit details about
		// how there is a mismatch between the call and
		// the function is given.
		
		FuncDecl* SingleCanidate = (*Canidates)[0];

		const llvm::SmallVector<VarDecl*, 2>&      Params = SingleCanidate->Params;
		const llvm::SmallVector<NonNamedValue, 2>& Args   = Call->Args;
		for (ulen ArgCount = 0; Args.size(); ArgCount++) {
			if (ArgCount >= Params.size()) {
				Error(Call, "Too many arguments for %s call", CallType);
				EncounteredError = true;
				break;
			}
			
			Expr*    Arg   = Args[ArgCount].E;
			VarDecl* Param = Params[ArgCount];

			if (!IsAssignableTo(Param->Ty, Arg->Ty, Arg)) {
				Error(Args[ArgCount].ExpandedLoc,
					"Cannot assign argument %s of type '%s' to parameter of type '%s'",
					ArgCount+1,
					Arg->Ty->ToString(), Param->Ty->ToString());
				EncounteredError = true;
			}
		}

	}
	
	if (!EncounteredError) {
		std::string FuncDef = "(";
		for (ulen i = 0; i < Call->Args.size(); i++) {
			FuncDef += Call->Args[i].E->Ty->ToString();
			if (i+1 != Call->Args.size()) FuncDef += ", ";
		}
		FuncDef += ")";

		Error(Call, "Could not find function with parameter types '%s'",
			FuncDef);
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

	Cast->IsFoldable = Cast->Value->IsFoldable;
	YIELD_ERROR_WHEN(Cast, Cast->Value);

}

void arco::SemAnalyzer::CheckStructInitializer(StructInitializer* StructInit) {
	StructType* StructTy = static_cast<StructType*>(StructInit->Ty);
	if (!FixupStructType(StructTy)) {
		YIELD_ERROR(StructInit);
	}

	StructDecl* Struct = StructTy->GetStruct();

	// TODO: May want to allow if the fields are foldable!
	StructInit->IsFoldable = false;

	// TODO: Would want to check if the structure has a constructor here.

	for (ulen i = 0; i < StructInit->Args.size(); i++) {
		NonNamedValue Value = StructInit->Args[i];
		CheckNode(Value.E);

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
		return FromTy == Context.CStrType;
	}
	case TypeKind::Pointer: {
		if (FromTy == Context.NullType)
			return true;
		else if (FromTy->GetKind() == TypeKind::Array) {
			PointerType* ToPtrTy     = static_cast<PointerType*>(ToTy);
			ArrayType*   FromArrayTy = static_cast<ArrayType*>(FromTy);
			return ToPtrTy->GetElementType()->Equals(FromArrayTy->GetElementType());
		}
		return ToTy->Equals(FromTy);
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

bool arco::SemAnalyzer::FixupType(Type* Ty) {
	if (Ty->GetKind() == TypeKind::Array) {
		return FixupArrayType(static_cast<ArrayType*>(Ty));
	} else if (Ty->GetKind() == TypeKind::Struct) {
		return FixupStructType(static_cast<StructType*>(Ty));
	}
	return true;
}

bool arco::SemAnalyzer::FixupArrayType(ArrayType* ArrayTy) {
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

	if (!LengthExpr->IsFoldable) {
		Error(ErrorLoc, "Could not compute the length of the array at compile time");
		return false;
	} else if (!LengthExpr->Ty->IsInt()) {
		Error(ErrorLoc, "The length of the array is expected to be an integer");
		return false;
	}

	IRGenerator IRGen(Context);
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

	if (!FixupType(ArrayTy->GetElementType())) {
		return false;
	}
	return true;
}

bool arco::SemAnalyzer::FixupStructType(StructType* StructTy) {
	auto Itr = Mod->Structs.find(StructTy->GetStructName());
	if (Itr == Mod->Structs.end()) {
		Error(StructTy->GetErrorLoc(), "Could not find struct by name '%s'", StructTy->GetStructName());
		return false;
	}
	StructDecl* Struct = Itr->second;
	SemAnalyzer Analyzer(Context, Struct);
	Analyzer.CheckStructDecl(Struct);
	StructTy->AssignStruct(Struct);
	return true;
}

void arco::SemAnalyzer::CheckModifibility(Expr* LValue) {
	if (!IsLValue(LValue)) {
		Error(LValue, "Expected to be a modifiable value");
	}
	// TODO: In future check for constness
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
