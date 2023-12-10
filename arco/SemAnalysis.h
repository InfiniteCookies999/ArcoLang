#ifndef ARCO_SEM_ANALYSIS_H
#define ARCO_SEM_ANALYSIS_H

#include "AST.h"
#include "Logger.h"

namespace arco {

	class ArcoContext;

	class SemAnalyzer {
	public:

		explicit SemAnalyzer(ArcoContext& Context, Module& Mod, Decl* D);

		void CheckFuncDecl(FuncDecl* Func);
		
		void CheckStructDecl(StructDecl* Struct);

	private:
		ArcoContext& Context;
		Module&      Mod;
		Logger       Log;

		FuncDecl* CFunc;

		// Every time a loop is entered this is incremented,
		// and decremented when existed
		ulen LoopDepth = 0;

		struct Scope {
			Scope* Parent = nullptr;
			// This refers to anything that
			// prevents the usual flow of
			// execution such as 'break',
			// 'continue', 'return'
			bool FoundTerminal = false;
			// Keeping track of returns
			// as a way of find out
			// if a function definitatively
			// returns.
			bool AllPathsReturn = false;
		}  * LocScope = nullptr;

		void CheckFuncParamTypes(FuncDecl* Func);

		void CheckNode(AstNode* Node);

		//===-------------------------------===//
		// Statements
		//===-------------------------------===//

		void CheckScopeStmts(LexScope& LScope, Scope& NewScope);
		void CheckVarDecl(VarDecl* Var);
		void CheckReturn(ReturnStmt* Return);
		void CheckLoopControl(LoopControlStmt* LoopControl);
		void CheckPredicateLoop(PredicateLoopStmt* Loop);
		void CheckRangeLoop(RangeLoopStmt* Loop);
		bool CheckIf(IfStmt* If);
		bool CheckNestedScope(NestedScopeStmt* NestedScope);

		//===-------------------------------===//
		// Expressions
		//===-------------------------------===//

		void CheckBinaryOp(BinaryOp* BinOp);
		void CheckUnaryOp(UnaryOp* UniOp);
		void CheckIdentRef(IdentRef* IRef, bool ExpectsFuncCall, StructDecl* StructToLookup = nullptr);
		void CheckFieldAccessor(FieldAccessor* FieldAcc, bool ExpectsFuncCall);
		void CheckFuncCall(FuncCall* Call);
		FuncDecl* FindBestFuncCallCanidate(FuncsList* Canidates,
			                               llvm::SmallVector<NonNamedValue, 2>& Args);
		bool CompareAsCanidate(FuncDecl* Canidate,
			                   llvm::SmallVector<NonNamedValue, 2>& Args);
		void DisplayErrorForNoMatchingFuncCall(FuncCall* Call, FuncsList* Canidates);
		void CheckArray(Array* Arr);
		void CheckArrayAccess(ArrayAccess* Access);
		void CheckTypeCast(TypeCast* Cast);
		void CheckStructInitializer(StructInitializer* StructInit);

		void CheckCondition(Expr* Cond, const char* PreErrorText);

		void CreateCast(Expr* E, Type* ToType);

		bool IsAssignableTo(Type* ToTy, Expr* FromExpr);
		bool IsAssignableTo(Type* ToTy, Type* FromTy, Expr* FromExpr);

		bool FixupType(Type* Ty);
		bool FixupArrayType(ArrayType* ArrayTy);
		bool FixupStructType(StructType* StructTy);

		void Error(SourceLoc Loc, const char* Msg) {
			Log.BeginError(Loc, Msg);
			Log.EndError();
		}

		template<typename... TArgs>
		void Error(SourceLoc Loc, const char* Fmt, TArgs&&... Args) {
			Log.BeginError(Loc, Fmt, std::forward<TArgs>(Args)...);
			Log.EndError();
		}

		void Error(AstNode* Node, const char* Msg) {
			Error(Node->Loc, Msg);
		}

		template<typename... TArgs>
		void Error(AstNode* Node, const char* Fmt, TArgs&&... Args) {
			Error(Node->Loc, Fmt, std::forward<TArgs>(Args)...);
		}
	};

}

#endif // ARCO_SEM_ANALYSIS_H