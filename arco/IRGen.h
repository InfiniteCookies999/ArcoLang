#ifndef ARCO_IRGEN_H
#define ARCO_IRGEN_H

#include <llvm/IR/IRBuilder.h>

#include "AST.h"

namespace arco {

	class ArcoContext;

	llvm::Type* GenType(ArcoContext& Context, Type* Ty);
	llvm::StructType* GenStructType(ArcoContext& Context, StructDecl* Struct);

	class IRGenerator {
	public:

		explicit IRGenerator(ArcoContext& Context);

		void GenFunc(FuncDecl* Func);

		void GenImplicitDefaultConstructorBody(StructDecl* Struct);

		llvm::Value* GenRValue(Expr* E);

	private:
		ArcoContext&       Context;
		llvm::LLVMContext& LLContext;
		llvm::Module&      LLModule;
		llvm::IRBuilder<>  Builder;
	
		// Current LLVM function being processed.
		FuncDecl*       CFunc  = nullptr;
		llvm::Function* LLFunc = nullptr;

		// The basic block jumped to at the end of the function.
		llvm::BasicBlock* LLFuncEndBB;

		// The address that the return value gets placed into.
		llvm::Value* LLRetAddr = nullptr;

		// The 'this' pointer of the struct.
		llvm::Value* LLThis = nullptr;

		// The exit points of loops currently being processed
		llvm::SmallVector<llvm::BasicBlock*, 4> LoopBreakStack;

		// The restart points of the next loop iteration
		llvm::SmallVector<llvm::BasicBlock*, 4> LoopContinueStack;

		bool EncounteredReturn = false;

		void GenFuncDecl(FuncDecl* Func);
		void GenFuncBody(FuncDecl* Func);

		llvm::Value* GenNode(AstNode* Node);

		//===-------------------------------===//
		// Statements
		//===-------------------------------===//

		llvm::Value* GenVarDecl(VarDecl* Var);
		llvm::Value* GenReturn(ReturnStmt* Ret);
		llvm::Value* GenLoopControl(LoopControlStmt* LoopControl);
		llvm::Value* GenPredicateLoop(PredicateLoopStmt* Loop);
		llvm::Value* GenRangeLoop(RangeLoopStmt* Loop);
		llvm::Value* GenIf(IfStmt* If);
		llvm::Value* GenNestedScope(NestedScopeStmt* NestedScope);

		//===-------------------------------===//
		// Expressions
		//===-------------------------------===//

		llvm::Value* GenBinaryOp(BinaryOp* BinOp);
		llvm::Value* GenUnaryOp(UnaryOp* UniOp);
		llvm::Value* GenNumberLiteral(NumberLiteral* Number);
		llvm::Value* GenStringLiteral(StringLiteral* String);
		llvm::Value* GenIdentRef(IdentRef* IRef);
		llvm::Value* GenFieldAccessor(FieldAccessor* FieldAcc);
		llvm::Value* GenFuncCall(FuncCall* Call, llvm::Value* LLAddr);
		llvm::Value* GenArray(Array* Arr, llvm::Value* LLAddr);
		llvm::Constant* GenConstArray(Array* Arr, ArrayType* DestTy);
		void FillArrayViaGEP(Array* Arr, llvm::Value* LLAddr, ArrayType* DestTy);
		llvm::Value* GenArrayAccess(ArrayAccess* Access);
		llvm::Value* GenTypeCast(TypeCast* Cast);
		llvm::Value* GenStructInitializer(StructInitializer* StructInit, llvm::Value* LLAddr);

		llvm::Value* GenAdd(llvm::Value* LLLHS, llvm::Value* LLRHS, Type* Ty);
		llvm::Value* GenSub(llvm::Value* LLLHS, llvm::Value* LLRHS, Type* Ty);
		llvm::Value* GenMul(llvm::Value* LLLHS, llvm::Value* LLRHS, Type* Ty);
		llvm::Value* GenDiv(llvm::Value* LLLHS, llvm::Value* LLRHS, Type* Ty);

		void GenLoopCondJump(llvm::BasicBlock* LLCondBB,
			                 llvm::BasicBlock* LLBodyBB,
			                 llvm::BasicBlock* LLEndBB,
			                 Expr* Cond);
		llvm::Value* GenCond(Expr* Cond);
		void GenBlock(llvm::BasicBlock* LLBB, ScopeStmts& Stmts);

		llvm::Value* GenCast(Type* ToType, Type* FromType, llvm::Value* LLValue);
		llvm::Value* CreateLoad(llvm::Value* LLAddr);

		llvm::Constant* GenConstValue(Type* Ty);
		llvm::Constant* GenZeroedValue(Type* Ty);

		void StructArrayCallDefaultConstructors(Type* BaseTy,
			                                    llvm::Value* LLArrStartPtr,
			                                    llvm::Value* LLTotalLinearLength);
		void GenInternalArrayLoop(Type* BaseTy,
			                      llvm::Value* LLArrStartPtr,
			                      llvm::Value* LLTotalLinearLength,
			                      const std::function<void(llvm::PHINode*, Type*)>& CodeGenCallback);

		/// Converts the llvm array type to its eqv. pointer type.
		///
		llvm::Value* DecayArray(llvm::Value* LLArray);
		llvm::Value* ArrayToPointer(llvm::Value* LLArray);
		llvm::Value* MultiDimensionalArrayToPointerOnly(llvm::Value* LLArray, ArrayType* ArrTy);

		inline llvm::Value* CreateInBoundsGEP(llvm::Value* LLAddr, llvm::ArrayRef<llvm::Value*> IdxList);
		inline llvm::Value* GetArrayIndexAddress(llvm::Value* LLArray, llvm::Value* LLIndex);
		inline llvm::Value* CreateStructGEP(llvm::Value* LLAddr, ulen Idx);

		llvm::GlobalVariable* GenLLVMGlobalVariable(llvm::StringRef Name, llvm::Type* LLType);
		llvm::GlobalVariable* GenConstGlobalArray(llvm::Constant* LLArray, bool DSOLocal = true);

		inline ulen SizeOfTypeInBytes(llvm::Type* LLType);

		llvm::Value* GenReturnValueForOptimizedStructAsInt(llvm::Value* LLRetVal);

		void GenReturnByStoreToElisionRetSlot(Expr* Value);

		void CopyStructObject(llvm::Value* LLToAddr, llvm::Value* LLFromAddr, StructDecl* Struct);

		/// This will only unconditionally branch to the given
		/// block as long as the current block does not already
		/// end in a branch (terminal).
		/// 
		/// This can happen for example in the following code
		/// if a < 5 {
		///     return;
		/// }
		///
		/// There is no reason for the if statement to branch to the
		/// end of its if statement 'if.end' if there is a return which
		/// causes it to branch to the end of the function already.
		///
		/// Another code example:
		///
		/// loop i int = 0; i < 55; i++ {
		/// }
		/// 
		/// The conditional block branches to the body of the loop if
		/// the statement is true, so when emitting the body of the loop
		/// it is essential to not branch unconditionally since a conditional
		/// branch was placed right beforehand.
		///
		void GenBranchIfNotTerm(llvm::BasicBlock* LLBB);

		void GenBranchOnCond(Expr* Cond, llvm::BasicBlock* LLTrueBB, llvm::BasicBlock* LLFalseBB);

		void GenAssignment(llvm::Value* LLAddress, Expr* Value);
		void GenDefaultValue(Type* Ty, llvm::Value* LLAddr);

		void CallDefaultConstructor(llvm::Value* LLAddr, StructType* StructTy);
		llvm::Function* GenDefaultConstructorDecl(StructDecl* Struct);

		llvm::Value* CreateUnseenAlloca(llvm::Type* LLTy, const char* Name);

		llvm::Value* GetElisionRetSlotAddr(FuncDecl* Func);

		void GenStoreStructRetFromCall(FuncCall* Call, llvm::Value* LLAddr);

		inline llvm::Type* GenType(Type* Ty) {
			return arco::GenType(Context, Ty);
		}

		inline llvm::StructType* GenStructType(StructDecl* Struct) {
			return arco::GenStructType(Context, Struct);
		}
		
		inline llvm::StructType* GenStructType(StructType* StructTy) {
			return arco::GenStructType(Context, StructTy->GetStruct());
		}
	};

}

#endif // ARCO_IRGEN_H