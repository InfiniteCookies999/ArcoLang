#include "IRGen.h"

#include "Context.h"
#include <unordered_set>

//===-------------------------------===//
// Helper Functions
//===-------------------------------===//

inline llvm::Constant* GetLLInt8(i32 value, llvm::LLVMContext& LLContext) {
	return llvm::ConstantInt::get(
		llvm::IntegerType::getInt8Ty(LLContext), value, true);
}
inline llvm::Constant* GetLLUInt8(u32 value, llvm::LLVMContext& LLContext) {
	return llvm::ConstantInt::get(
		llvm::IntegerType::getInt8Ty(LLContext), value, false);
}
inline llvm::Constant* GetLLInt16(i32 value, llvm::LLVMContext& LLContext) {
	return llvm::ConstantInt::get(
		llvm::IntegerType::getInt16Ty(LLContext), value, true);
}
inline llvm::Constant* GetLLUInt16(u32 value, llvm::LLVMContext& LLContext) {
	return llvm::ConstantInt::get(
		llvm::IntegerType::getInt16Ty(LLContext), value, false);
}
inline llvm::Constant* GetLLInt32(i32 value, llvm::LLVMContext& LLContext) {
	return llvm::ConstantInt::get(
		llvm::IntegerType::getInt32Ty(LLContext), value, true);
}
inline llvm::Constant* GetLLUInt32(u32 value, llvm::LLVMContext& LLContext) {
	return llvm::ConstantInt::get(
		llvm::IntegerType::getInt32Ty(LLContext), value, false);
}
inline llvm::Constant* GetLLInt64(i64 value, llvm::LLVMContext& LLContext) {
	return llvm::ConstantInt::get(
		llvm::IntegerType::getInt64Ty(LLContext), value, true);
}
inline llvm::Constant* GetLLUInt64(u64 value, llvm::LLVMContext& LLContext) {
	return llvm::ConstantInt::get(
		llvm::IntegerType::getInt64Ty(LLContext), value, false);
}

inline llvm::IntegerType* GetSystemIntType(llvm::LLVMContext& LLContext, llvm::Module& LLModule) {
	return llvm::IntegerType::getIntNTy(LLContext, LLModule.getDataLayout().getPointerSizeInBits());
}

inline llvm::Constant* GetSystemUInt(u64 value, llvm::LLVMContext& LLContext, llvm::Module& LLModule) {
	return llvm::ConstantInt::get(GetSystemIntType(LLContext, LLModule), value, false);
}

inline llvm::Constant* GetSystemInt(i64 value, llvm::LLVMContext& LLContext, llvm::Module& LLModule) {
		return llvm::ConstantInt::get(GetSystemIntType(LLContext, LLModule), value, true);
}

struct LLValTypePrinter {
	LLValTypePrinter(const llvm::Value* arg)
		: Arg(arg) {}

	const llvm::Value* Arg;
	void PrintLLType(llvm::raw_ostream& OS, const llvm::Value* LLValue) const {
		LLValue->getType()->print(OS);
	}
};

struct LLTypePrinter {
	LLTypePrinter(const llvm::Type* arg)
		: Arg(arg) {}

	const llvm::Type* Arg;
	void PrintLLType(llvm::raw_ostream& OS, const llvm::Type* LLType) const {
		LLType->print(OS);
	}
};

llvm::raw_ostream& operator<<(llvm::raw_ostream& OS, const LLValTypePrinter& Printer) {
	Printer.PrintLLType(OS, Printer.Arg);
	return OS;
}

llvm::raw_ostream& operator<<(llvm::raw_ostream& OS, const LLTypePrinter& Printer) {
	Printer.PrintLLType(OS, Printer.Arg);
	return OS;
}



llvm::Type* arco::GenType(ArcoContext& Context, Type* Ty) {
	llvm::LLVMContext& LLContext = Context.LLContext;
	switch (Ty->GetKind()) {
	case TypeKind::Char:
	case TypeKind::Int8:
	case TypeKind::UnsignedInt8:
		return llvm::Type::getInt8Ty(LLContext);
	case TypeKind::Int16:
	case TypeKind::UnsignedInt16:
		return llvm::Type::getInt16Ty(LLContext);
	case TypeKind::Int32:
	case TypeKind::UnsignedInt32:
		return llvm::Type::getInt32Ty(LLContext);
	case TypeKind::Int64:
	case TypeKind::UnsignedInt64:
		return llvm::Type::getInt64Ty(LLContext);
	case TypeKind::Int:
	case TypeKind::UnsignedInt:
		return GetSystemIntType(LLContext, Context.LLArcoModule);
	case TypeKind::Void:
		return llvm::Type::getVoidTy(LLContext);
	case TypeKind::CStr:
		return llvm::Type::getInt8PtrTy(LLContext);
	case TypeKind::Pointer: {
		PointerType* PtrTy = static_cast<PointerType*>(Ty);
		if (PtrTy->GetElementType() == Context.VoidType) {
			return llvm::Type::getInt8PtrTy(LLContext);
		} else {
			return llvm::PointerType::get(GenType(Context, PtrTy->GetElementType()), 0);
		}
	}
	case TypeKind::Array: {
		ArrayType* ArrayTy = static_cast<ArrayType*>(Ty);
		return llvm::ArrayType::get(GenType(Context, ArrayTy->GetElementType()), ArrayTy->GetLength());
	}
	case TypeKind::Struct:
		return GenStructType(Context, static_cast<StructType*>(Ty)->GetStruct());
	default:
		assert(!"Failed to implement case for GenType()");
		return nullptr;
	}
}

llvm::StructType* arco::GenStructType(ArcoContext& Context, StructDecl* Struct) {
	if (Struct->LLStructTy) {
		return Struct->LLStructTy;
	}
	llvm::StructType* LLStructTy = llvm::StructType::create(Context.LLContext);
	Struct->LLStructTy = LLStructTy; // Set early to prevent endless recursive

	std::vector<llvm::Type*> LLStructFieldTypes;
	LLStructFieldTypes.resize(Struct->Fields.size());
	if (Struct->Fields.empty()) {
		LLStructFieldTypes.push_back(llvm::Type::getInt8Ty(Context.LLContext));
	} else {
		for (VarDecl* Field : Struct->Fields) {
			LLStructFieldTypes[Field->FieldIdx] = GenType(Context, Field->Ty);	
		}
	}
	LLStructTy->setBody(LLStructFieldTypes);
	LLStructTy->setName(Struct->Name.Text);

	return LLStructTy;
}

arco::IRGenerator::IRGenerator(ArcoContext& Context)
	: Context(Context),
	  LLContext(Context.LLContext),
	  LLModule(Context.LLArcoModule),
	  Builder(Context.LLContext)
{
}

void arco::IRGenerator::GenFunc(FuncDecl* Func) {

	// -- DEBUG
	// llvm::outs() << "generating function: " << Func->Name << '\n';

	GenFuncDecl(Func);
	GenFuncBody(Func);

}

void arco::IRGenerator::GenImplicitDefaultConstructorBody(StructDecl* Struct) {
	LLFunc = Struct->LLDefaultConstructor;

	llvm::BasicBlock* LLEntryBlock = llvm::BasicBlock::Create(LLContext, "func.entry", LLFunc);
	Builder.SetInsertPoint(LLEntryBlock);

	// Allocating "this" pointer
	llvm::Value* LLThisAddr = Builder.CreateAlloca(LLFunc->getArg(0)->getType(), nullptr, "this.addr");

	// Storing "this" pointer
	Builder.CreateStore(LLFunc->getArg(0), LLThisAddr);

	LLThis = CreateLoad(LLThisAddr);
	LLThis->setName("this");

	for (VarDecl* Field : Struct->Fields) {
		llvm::Value* LLFieldAddr = CreateStructGEP(LLThis, Field->FieldIdx);
		if (Field->Assignment) {
			GenAssignment(LLFieldAddr, Field->Assignment);
		} else {
			GenDefaultValue(Field->Ty, LLFieldAddr);
		}
	}

	Builder.CreateRetVoid();

}

void arco::IRGenerator::GenFuncDecl(FuncDecl* Func) {
	if (Func->LLFunction) return;
	
	llvm::Type* LLRetTy;
	LLRetTy = Func == Context.MainEntryFunc ? llvm::Type::getInt32Ty(LLContext)
		                                    : GenType(Func->RetTy);
	llvm::Twine LLFuncName = Func->Name.Text;
	llvm::Twine LLFullFuncName = (Func == Context.MainEntryFunc ||
		                          (Func->Mods & ModKinds::NATIVE))
		                       ? LLFuncName
		                       : LLFuncName.concat(".arco");
	
	llvm::SmallVector<llvm::Type*, 4> LLParamTypes;
	for (VarDecl* Param : Func->Params) {
		Type* Ty = Param->Ty;
		if (Ty->GetKind() == TypeKind::Array) {
			// Arrays are decayed when passed.
			LLParamTypes.push_back(
				llvm::PointerType::get(GenType(static_cast<ArrayType*>(Ty)->GetElementType()), 0)
			);
		} else {
			LLParamTypes.push_back(GenType(Ty));
		}
	}

	llvm::FunctionType* LLFuncType =
		llvm::FunctionType::get(LLRetTy, LLParamTypes, false);

	llvm::Function* LLFunc = llvm::Function::Create(
		LLFuncType,
		llvm::Function::ExternalLinkage,
		LLFullFuncName,
		LLModule
	);

	if (Func->Mods & ModKinds::NATIVE) {
#ifdef _WIN32
		LLFunc->setDLLStorageClass(llvm::GlobalValue::DLLImportStorageClass);
		LLFunc->setCallingConv(llvm::CallingConv::X86_StdCall);
#endif
	} else {
		// Will resolve the symbol within the same compilation unit.
		LLFunc->setDSOLocal(true);
	}

	for (ulen i = 0; i < Func->Params.size(); i++) {
		LLFunc->getArg(i)->setName(llvm::Twine(Func->Params[i]->Name.Text).concat(".param"));
	}

	Func->LLFunction = LLFunc;

}

void arco::IRGenerator::GenFuncBody(FuncDecl* Func) {
	if (Func->Mods & ModKinds::NATIVE) return;

	CFunc  = Func;
	LLFunc = Func->LLFunction;

	// Entry block for the function.
	llvm::BasicBlock* LLEntryBlock = llvm::BasicBlock::Create(LLContext, "func.entry", Func->LLFunction);
	Builder.SetInsertPoint(LLEntryBlock);

	if (Func->NumReturns > 1) {
		LLFuncEndBB = llvm::BasicBlock::Create(LLContext, "func.return");
		if (Func == Context.MainEntryFunc) {
			LLRetAddr = Builder.CreateAlloca(GenType(Context.Int32Type), nullptr, "ret.val");
		}
	}

	// Allocating space for the variables
	//
	for (VarDecl* Var : Func->AllocVars) {
		llvm::Type* LLTy;
		if (Var->IsParam() && Var->Ty->GetKind() == TypeKind::Array) {
			ArrayType* ArrayTy = static_cast<ArrayType*>(Var->Ty);
			LLTy = llvm::PointerType::get(GenType(ArrayTy->GetElementType()), 0);
		} else {
			LLTy = GenType(Var->Ty);
		}

		llvm::Value* LLAlloca = Builder.CreateAlloca(LLTy);
		LLAlloca->setName(Var->Name.Text);
		Var->LLAddress = LLAlloca;
	}

	// Storing the incoming parameters
	//
	ulen LLParamIndex = 0;
	for (VarDecl* Param : Func->Params) {
		Builder.CreateStore(LLFunc->getArg(LLParamIndex++), Param->LLAddress);
	}

	// Generating the statements of the function.
	for (AstNode* Stmt : Func->Scope.Stmts) {
		GenNode(Stmt);
	}

	// Branching to LLFuncEndBB if its needed.
	if (Func->NumReturns > 1 && Builder.GetInsertBlock()->empty()) {
		// If the current block is empty we can just use
		// the current block as the ending block instead.
		LLFuncEndBB->replaceAllUsesWith(Builder.GetInsertBlock());
		delete LLFuncEndBB;
	} else if (Func->NumReturns > 1) {
		LLFunc->getBasicBlockList().push_back(LLFuncEndBB); // Because the parent was not originally set
		GenBranchIfNotTerm(LLFuncEndBB);
		Builder.SetInsertPoint(LLFuncEndBB);
	}

	if (Func->NumReturns > 1) {
		if (LLRetAddr) {
			Builder.CreateRet(CreateLoad(LLRetAddr));
		} else {
			Builder.CreateRetVoid();
		}
	} else if (Func->NumReturns == 1 && Func->RetTy == Context.VoidType) {
		if (!EncounteredReturn) {
			// Implicit void return.
			if (Func == Context.MainEntryFunc) {
				Builder.CreateRet(GetLLInt32(0, LLContext));
			} else {
				Builder.CreateRetVoid();
			}
		}
	}
}

llvm::Value* arco::IRGenerator::GenNode(AstNode* Node) {
	switch (Node->Kind) {
	case AstKind::VAR_DECL:
		return GenVarDecl(static_cast<VarDecl*>(Node));
	case AstKind::RETURN:
		return GenReturn(static_cast<ReturnStmt*>(Node));
	case AstKind::CONTINUE:
	case AstKind::BREAK:
		return GenLoopControl(static_cast<LoopControlStmt*>(Node));
	case AstKind::PREDICATE_LOOP:
		return GenPredicateLoop(static_cast<PredicateLoopStmt*>(Node));
	case AstKind::RANGE_LOOP:
		return GenRangeLoop(static_cast<RangeLoopStmt*>(Node));
	case AstKind::IF:
		return GenIf(static_cast<IfStmt*>(Node));
	case AstKind::NESTED_SCOPE:
		return GenNestedScope(static_cast<NestedScopeStmt*>(Node));
	case AstKind::BINARY_OP:
		return GenBinaryOp(static_cast<BinaryOp*>(Node));
	case AstKind::UNARY_OP:
		return GenUnaryOp(static_cast<UnaryOp*>(Node));
	case AstKind::NUMBER_LITERAL:
		return GenNumberLiteral(static_cast<NumberLiteral*>(Node));
	case AstKind::STRING_LITERAL:
		return GenStringLiteral(static_cast<StringLiteral*>(Node));
	case AstKind::NULLPTR:
		return llvm::Constant::getNullValue(GenType(static_cast<NullPtr*>(Node)->CastTy));
	case AstKind::IDENT_REF:
		return GenIdentRef(static_cast<IdentRef*>(Node));
	case AstKind::FIELD_ACCESSOR:
		return GenFieldAccessor(static_cast<FieldAccessor*>(Node));
	case AstKind::FUNC_CALL:
		return GenFuncCall(static_cast<FuncCall*>(Node));
	case AstKind::ARRAY:
		return GenArray(static_cast<Array*>(Node), nullptr);
	case AstKind::ARRAY_ACCESS:
		return GenArrayAccess(static_cast<ArrayAccess*>(Node));
	case AstKind::TYPE_CAST:
		return GenTypeCast(static_cast<TypeCast*>(Node));
	case AstKind::STRUCT_INITIALIZER:
		return GenStructInitializer(static_cast<StructInitializer*>(Node), nullptr);
	default:
		assert(!"Unimplemented GenNode() case!");
		return nullptr;
	}
}

llvm::Value* arco::IRGenerator::GenRValue(Expr* E) {
	llvm::Value* LLValue = GenNode(E);
	
	switch (E->Kind) {
	case AstKind::IDENT_REF:
	case AstKind::ARRAY_ACCESS:
	case AstKind::FIELD_ACCESSOR: {
		
		if (E->Is(AstKind::FIELD_ACCESSOR)) {
			FieldAccessor* FieldAcc = static_cast<FieldAccessor*>(E);
			if (FieldAcc->IsArrayLength) {
				// Array lengths are not memory so no reason to load.
				break;
			}
		}

		// Want to make sure not to load an array
		// since arrays should always be taken as
		// l-values.
		if (E->Ty->GetKind() != TypeKind::Array) {
			LLValue = CreateLoad(LLValue);
		}
		break;
	}
	}
	
	if (E->CastTy) {
		LLValue = GenCast(E->CastTy, E->Ty, LLValue);
	}
	return LLValue;
}

//===-------------------------------===//
// Statements
//===-------------------------------===//

llvm::Value* arco::IRGenerator::GenVarDecl(VarDecl* Var) {
	if (Var->Assignment) {
		GenAssignment(Var->LLAddress, Var->Assignment);
	} else {
		GenDefaultValue(Var->Ty, Var->LLAddress);
	}
	return Var->LLAddress;
}

llvm::Value* arco::IRGenerator::GenReturn(ReturnStmt* Ret) {
	EncounteredReturn = true;

	if (CFunc->NumReturns == 1) {
		// Only a single return so the instruction has full control over
		// returning and must create the return instructions.
	
		llvm::Value* LLRetValue = nullptr;

		if (!Ret->Value && CFunc == Context.MainEntryFunc) {
			// Default to returning zero for the main function.
			LLRetValue = GetLLInt32(0, LLContext);
		} else if (Ret->Value) {
			LLRetValue = GenRValue(Ret->Value);
		}
	
		if (LLRetValue) {
			if (CFunc == Context.MainEntryFunc &&
				Ret->Value                     &&
				Ret->Value->Ty != Context.Int32Type) {
				LLRetValue = GenCast(Context.Int32Type, Ret->Value->Ty, LLRetValue);
			}
			Builder.CreateRet(LLRetValue);
		} else {
			Builder.CreateRetVoid();
		}
	} else {
		Builder.CreateBr(LLFuncEndBB);
	}

	return nullptr;
}

llvm::Value* arco::IRGenerator::GenLoopControl(LoopControlStmt* LoopControl) {
	if (LoopControl->Kind == AstKind::BREAK) {
		llvm::BasicBlock* LoopExit = LoopBreakStack[LoopBreakStack.size() - 1 - (LoopControl->LoopCount-1)];
		Builder.CreateBr(LoopExit);
	} else {
		llvm::BasicBlock* LoopRestart = LoopContinueStack[LoopBreakStack.size() - 1 - (LoopControl->LoopCount-1)];
		Builder.CreateBr(LoopRestart);
	}
	return nullptr;
}

llvm::Value* arco::IRGenerator::GenPredicateLoop(PredicateLoopStmt* Loop) {
	llvm::BasicBlock* LLEndBB  = llvm::BasicBlock::Create(LLContext, "loop.end", LLFunc);
	llvm::BasicBlock* LLBodyBB = llvm::BasicBlock::Create(LLContext, "loop.body", LLFunc);
	llvm::BasicBlock* LLCondBB = llvm::BasicBlock::Create(LLContext, "loop.cond", LLFunc);
	
	LoopBreakStack.push_back(LLEndBB);
	LoopContinueStack.push_back(LLCondBB);

	// Generating the condition block
	GenLoopCondJump(LLCondBB, LLBodyBB, LLEndBB, Loop->Cond);	

	GenBlock(LLBodyBB, Loop->Scope.Stmts);

	LoopBreakStack.pop_back();
	LoopContinueStack.pop_back();

	// Unconditionally branch back to the condition block
	GenBranchIfNotTerm(LLCondBB);

	// Finally continuing forward into a new block after the loop
	GenBranchIfNotTerm(LLEndBB);
	Builder.SetInsertPoint(LLEndBB);
	
	return nullptr;
}

llvm::Value* arco::IRGenerator::GenRangeLoop(RangeLoopStmt* Loop) {

	llvm::BasicBlock* LLEndBB  = llvm::BasicBlock::Create(LLContext, "loop.end", LLFunc);
	llvm::BasicBlock* LLBodyBB = llvm::BasicBlock::Create(LLContext, "loop.body", LLFunc);
	llvm::BasicBlock* LLIncBB  = !Loop->Incs.empty()
		                            ? llvm::BasicBlock::Create(LLContext, "loop.inc", LLFunc)
	                                : nullptr;
	llvm::BasicBlock* LLCondBB     = llvm::BasicBlock::Create(LLContext, "loop.cond", LLFunc);
	llvm::BasicBlock* LLContinueBB = LLIncBB ? LLIncBB : LLCondBB;

	LoopBreakStack.push_back(LLEndBB);
	LoopContinueStack.push_back(LLContinueBB);

	for (VarDecl* Decl : Loop->Decls) {
		GenNode(Decl);
	}

	// Generating the condition block
	GenLoopCondJump(LLCondBB, LLBodyBB, LLEndBB, Loop->Cond);

	GenBlock(LLBodyBB, Loop->Scope.Stmts);

	LoopBreakStack.pop_back();
	LoopContinueStack.pop_back();

	// Unconditionally branch back to the condition or inc. block
	// to restart the loop.
	GenBranchIfNotTerm(LLContinueBB);

	// Creating the code for the inc. block if needed
	if (LLIncBB) {

		Builder.SetInsertPoint(LLIncBB);

		for (Expr* Inc : Loop->Incs) {
			GenNode(Inc);
		}

		// Jumping directly into the loop condition
		Builder.CreateBr(LLCondBB); // No need to check for terminal since expressions cannot jump.
	}

	GenBranchIfNotTerm(LLEndBB);
	Builder.SetInsertPoint(LLEndBB);

	return nullptr;
}

llvm::Value* arco::IRGenerator::GenIf(IfStmt* If) {
	llvm::BasicBlock* LLThenBB = llvm::BasicBlock::Create(LLContext, "if.then", LLFunc);
	llvm::BasicBlock* LLEndBB  = llvm::BasicBlock::Create(LLContext, "if.end", LLFunc);
	llvm::BasicBlock* LLElseBB = LLEndBB;
	if (If->Else) {
		LLElseBB = llvm::BasicBlock::Create(LLContext, "if.else", LLFunc);
	}

	GenBranchOnCond(If->Cond, LLThenBB, LLElseBB);

	GenBlock(LLThenBB, If->Scope.Stmts);

	// Jump out of the body of the if statement
	GenBranchIfNotTerm(LLEndBB);

	// Generating the else statement if it exist
	if (AstNode* Else = If->Else) {
		Builder.SetInsertPoint(LLElseBB);
		GenNode(Else); // Pushes and pops its own scope.
	}

	// Finally continuing forward into a new block after the if
	GenBranchIfNotTerm(LLEndBB);
	Builder.SetInsertPoint(LLEndBB);

	return nullptr;
}

llvm::Value* arco::IRGenerator::GenNestedScope(NestedScopeStmt* NestedScope) {
	GenBlock(nullptr, NestedScope->Scope.Stmts);
	return nullptr;
}

//===-------------------------------===//
// Expressions
//===-------------------------------===//

llvm::Value* arco::IRGenerator::GenBinaryOp(BinaryOp* BinOp) {
	switch (BinOp->Op) {
	case '=': {
		llvm::Value* LLValue = GenRValue(BinOp->RHS);
		llvm::Value* LLAddress = GenNode(BinOp->LHS);
		Builder.CreateStore(LLValue, LLAddress);
		return LLAddress;
	}
	//
	// Arithmetic
	//
	case '+': {
		llvm::Value* LLLHS = GenRValue(BinOp->LHS);
		llvm::Value* LLRHS = GenRValue(BinOp->RHS);

		return GenAdd(LLLHS, LLRHS, BinOp->Ty);
	}
	case '-': {
		llvm::Value* LLLHS = GenRValue(BinOp->LHS);
		llvm::Value* LLRHS = GenRValue(BinOp->RHS);

		return GenSub(LLLHS, LLRHS, BinOp->Ty);
	}
	case '*': {
		llvm::Value* LLLHS = GenRValue(BinOp->LHS);
		llvm::Value* LLRHS = GenRValue(BinOp->RHS);

		return GenMul(LLLHS, LLRHS, BinOp->Ty);
	}
	case '/': {
		llvm::Value* LLLHS = GenRValue(BinOp->LHS);
		llvm::Value* LLRHS = GenRValue(BinOp->RHS);

		return GenDiv(LLLHS, LLRHS, BinOp->Ty);
	}
	case '%': {
		llvm::Value* LLLHS = GenRValue(BinOp->LHS);
		llvm::Value* LLRHS = GenRValue(BinOp->RHS);

		if (BinOp->Ty->IsSigned()) {
			return Builder.CreateSRem(LLLHS, LLRHS);
		}
		return Builder.CreateURem(LLLHS, LLRHS);
	}
	case TokenKind::PLUS_EQ: { // +=
		llvm::Value* LLLHS = GenNode(BinOp->LHS);
		llvm::Value* LLRHS = GenRValue(BinOp->RHS);

		llvm::Value* LLLHSRV = CreateLoad(LLLHS);
		llvm::Value* V = BinOp->Ty->IsInt() ? Builder.CreateAdd(LLLHSRV, LLRHS)
											: Builder.CreateFAdd(LLLHSRV, LLRHS);
		Builder.CreateStore(V, LLLHS);
		return V;
	}
	case TokenKind::MINUS_EQ: { // -=
		llvm::Value* LLLHS = GenNode(BinOp->LHS);
		llvm::Value* LLRHS = GenRValue(BinOp->RHS);

		llvm::Value* LLLHSRV = CreateLoad(LLLHS);
		llvm::Value* V = BinOp->Ty->IsInt() ? Builder.CreateSub(LLLHSRV, LLRHS)
											: Builder.CreateFSub(LLLHSRV, LLRHS);
		Builder.CreateStore(V, LLLHS);
		return V;
	}
	case TokenKind::STAR_EQ: { // *=
		llvm::Value* LLLHS = GenNode(BinOp->LHS);
		llvm::Value* LLRHS = GenRValue(BinOp->RHS);
		llvm::Value* LLLHSRV = CreateLoad(LLLHS);
		llvm::Value* V = BinOp->Ty->IsInt() ? Builder.CreateMul(LLLHSRV, LLRHS)
			                                : Builder.CreateFMul(LLLHSRV, LLRHS);
		Builder.CreateStore(V, LLLHS);
		return V;
	}
	case TokenKind::SLASH_EQ: { // /=
		llvm::Value* LLLHS = GenNode(BinOp->LHS);
		llvm::Value* LLRHS = GenRValue(BinOp->RHS);
		llvm::Value* LLLHSRV = CreateLoad(LLLHS);
		llvm::Value* V;
		if (BinOp->Ty->IsInt()) {
			V = BinOp->Ty->IsSigned() ? Builder.CreateSDiv(LLLHSRV, LLRHS)
			                          : Builder.CreateUDiv(LLLHSRV, LLRHS);
		} else {
			V = Builder.CreateFDiv(LLLHSRV, LLRHS);
		}
		Builder.CreateStore(V, LLLHS);
		return V;
	}
	case TokenKind::MOD_EQ: { // %=
		llvm::Value* LLLHS = GenNode(BinOp->LHS);
		llvm::Value* LLRHS = GenRValue(BinOp->RHS);
		llvm::Value* LLLHSRV = CreateLoad(LLLHS);
		llvm::Value* V = BinOp->Ty->IsSigned() ? Builder.CreateSRem(LLLHSRV, LLRHS)
			                                   : Builder.CreateURem(LLLHSRV, LLRHS);
		Builder.CreateStore(V, LLLHS);
		return V;
	}
	//
	// Bitwise
	//
	case '&': {
		llvm::Value* LLLHS = GenRValue(BinOp->LHS);
		llvm::Value* LLRHS = GenRValue(BinOp->RHS);
		return Builder.CreateAnd(LLLHS, LLRHS);
	}
	case '^': {
		llvm::Value* LLLHS = GenRValue(BinOp->LHS);
		llvm::Value* LLRHS = GenRValue(BinOp->RHS);
		return Builder.CreateXor(LLLHS, LLRHS);
	}
	case '|': {
		llvm::Value* LLLHS = GenRValue(BinOp->LHS);
		llvm::Value* LLRHS = GenRValue(BinOp->RHS);
		llvm::Value* LLValue = Builder.CreateOr(LLLHS, LLRHS);
		return LLValue;
	}
	case TokenKind::LT_LT: { // <<
		llvm::Value* LLLHS = GenRValue(BinOp->LHS);
		llvm::Value* LLRHS = GenRValue(BinOp->RHS);
		llvm::Value* LLValue = Builder.CreateShl(LLLHS, LLRHS);
		return LLValue;
	}
	case TokenKind::GT_GT: { // >>
		llvm::Value* LLLHS = GenRValue(BinOp->LHS);
		llvm::Value* LLRHS = GenRValue(BinOp->RHS);
		if (BinOp->Ty->IsSigned()) {
			// Arithmetic shift so that the sign is considered
			// seperately from the bits.
			return Builder.CreateAShr(LLLHS, LLRHS);
		} else {
			return Builder.CreateLShr(LLLHS, LLRHS);
		}
	}
	case TokenKind::AMP_EQ: { // &=
		llvm::Value* LLLHS = GenNode(BinOp->LHS);
		llvm::Value* LLRHS = GenRValue(BinOp->RHS);
		llvm::Value* LLLHSRV = CreateLoad(LLLHS);
		llvm::Value* V = Builder.CreateAnd(LLLHSRV, LLRHS);
		Builder.CreateStore(V, LLLHS);
		return V;
	}
	case TokenKind::CRT_EQ: { // ^=
		llvm::Value* LLLHS = GenNode(BinOp->LHS);
		llvm::Value* LLRHS = GenRValue(BinOp->RHS);
		llvm::Value* LLLHSRV = CreateLoad(LLLHS);
		llvm::Value* V = Builder.CreateXor(LLLHSRV, LLRHS);
		Builder.CreateStore(V, LLLHS);
		return V;
	}
	case TokenKind::BAR_EQ: { // |=
		llvm::Value* LLLHS = GenNode(BinOp->LHS);
		llvm::Value* LLRHS = GenRValue(BinOp->RHS);
		llvm::Value* LLLHSRV = CreateLoad(LLLHS);
		llvm::Value* V = Builder.CreateOr(LLLHSRV, LLRHS);
		Builder.CreateStore(V, LLLHS);
		return V;
	}
	case TokenKind::LT_LT_EQ: { // <<=
		llvm::Value* LLLHS = GenNode(BinOp->LHS);
		llvm::Value* LLRHS = GenRValue(BinOp->RHS);
		llvm::Value* LLLHSRV = CreateLoad(LLLHS);
		llvm::Value* V = Builder.CreateShl(LLLHSRV, LLRHS);
		Builder.CreateStore(V, LLLHS);
		return V;
	}
	case TokenKind::GT_GT_EQ: { // >>=
		llvm::Value* LLLHS = GenNode(BinOp->LHS);
		llvm::Value* LLRHS = GenRValue(BinOp->RHS);
		llvm::Value* LLLHSRV = CreateLoad(LLLHS);
		
		llvm::Value* V;
		if (BinOp->Ty->IsSigned()) {
			// Arithmetic shift so that the sign is considered
			// seperately from the bits.
			V = Builder.CreateAShr(LLLHSRV, LLRHS);
		} else {
			V = Builder.CreateLShr(LLLHSRV, LLRHS);
		}
		Builder.CreateStore(V, LLLHS);
		return V;
	}
	//
	// Conditionals
	//
	case '<': {
		llvm::Value* LLLHS = GenRValue(BinOp->LHS);
		llvm::Value* LLRHS = GenRValue(BinOp->RHS);

		if (BinOp->LHS->Ty->IsSigned() ||
			BinOp->RHS->Ty->IsSigned()) {
			return Builder.CreateICmpSLT(LLLHS, LLRHS);
		} else {
			return Builder.CreateICmpULT(LLLHS, LLRHS);
		}
	}
	case '>': {
		llvm::Value* LLLHS = GenRValue(BinOp->LHS);
		llvm::Value* LLRHS = GenRValue(BinOp->RHS);

		if (BinOp->LHS->Ty->IsSigned() ||
			BinOp->RHS->Ty->IsSigned()) {
			return Builder.CreateICmpSGT(LLLHS, LLRHS);
		} else {
			return Builder.CreateICmpUGT(LLLHS, LLRHS);
		}
	}
	case TokenKind::EQ_EQ: {
		llvm::Value* LLLHS = GenRValue(BinOp->LHS);
		llvm::Value* LLRHS = GenRValue(BinOp->RHS);
		return Builder.CreateICmpEQ(LLLHS, LLRHS);
	}
	default:
		assert(!"Failed to implement GenBinaryOp() case!");
		return nullptr;
	}
}

llvm::Value* arco::IRGenerator::GenUnaryOp(UnaryOp* UniOp) {
	switch (UniOp->Op) {
	case '&': {
		// When GenRValue is called it makes sure
		// not to shave off the pointer value for
		// this operator. Because of that all this
		// needs to do is return the l-value.
		return GenNode(UniOp->Value);
	}
	case '-': {
		llvm::Value* LLValue = GenRValue(UniOp->Value);
		if (UniOp->Ty->IsInt()) {
			return Builder.CreateNeg(LLValue);
		} else {
			return Builder.CreateFNeg(LLValue);
		}
	}
	case '+':
		return GenRValue(UniOp->Value);
	default:
		assert(!"Failed to implement GenUnaryOp() case!");
		return nullptr;
	}
}

llvm::Value* arco::IRGenerator::GenNumberLiteral(NumberLiteral* Number) {
	return GetLLInt64(Number->SignedIntValue, LLContext);
}

llvm::Value* arco::IRGenerator::GenStringLiteral(StringLiteral* String) {
	llvm::SmallVector<llvm::Constant*, 4> LLElements;
	LLElements.reserve(String->Characters.size() + 1);
	for (char Char : String->Characters) {
		LLElements.push_back(GetLLUInt8(Char, LLContext));
	}
	// Null termination
	LLElements.push_back(GetLLUInt8('\0', LLContext));

	llvm::ArrayType* LLArrType =
		llvm::ArrayType::get(llvm::Type::getInt8Ty(LLContext), LLElements.size());
	llvm::Constant* LLConstArray = llvm::ConstantArray::get(LLArrType, LLElements);

	llvm::GlobalVariable* LLGArray = GenConstGlobalArray(LLConstArray);

	return DecayArray(LLGArray);
}

llvm::Value* arco::IRGenerator::GenIdentRef(IdentRef* IRef) {
	if (IRef->Var->IsField()) {
		return CreateStructGEP(LLThis, IRef->Var->FieldIdx);
	} else {
		return IRef->Var->LLAddress;
	}
}

llvm::Value* arco::IRGenerator::GenFieldAccessor(FieldAccessor* FieldAcc) {
	if (FieldAcc->IsArrayLength) {
		return GetSystemInt(
			static_cast<ArrayType*>(FieldAcc->Site->Ty)->GetLength(),
			LLContext,
			LLModule
		);
	}

	llvm::Value* LLSite = GenNode(FieldAcc->Site);

	return CreateStructGEP(LLSite, FieldAcc->Var->FieldIdx);
}

llvm::Value* arco::IRGenerator::GenFuncCall(FuncCall* Call) {

	GenFuncDecl(Call->CalledFunc);

	llvm::SmallVector<llvm::Value*, 2> LLArgs;

	for (ulen i = 0; i < Call->Args.size(); i++) {
		Expr* Arg = Call->Args[i].E;
		llvm::Value* LLArg = GenRValue(Arg);
		if (Arg->Ty->GetKind() == TypeKind::Array) {
			// If not already decayed, decay.
			if (LLArg->getType()->isPointerTy() &&
				LLArg->getType()->getPointerElementType()->isArrayTy()
				) {
				LLArg = DecayArray(LLArg);
			} else {
				// TODO: Load?
			}
		}
		LLArgs.push_back(LLArg);
	}

	llvm::Function* LLCalledFunc = Call->CalledFunc->LLFunction;

	llvm::Value* LLRetValue = Builder.CreateCall(LLCalledFunc, LLArgs);
	if (LLRetValue->getType() != llvm::Type::getVoidTy(LLContext)) {
		LLRetValue->setName("ret.val");
	}
	return LLRetValue;
}

llvm::Value* arco::IRGenerator::GenArray(Array* Arr, llvm::Value* LLAddr) {

	ArrayType* DestTy = static_cast<ArrayType*>(Arr->Ty);
	if (Arr->CastTy) {
		DestTy = static_cast<ArrayType*>(Arr->CastTy);
	}

	if (!LLAddr) {
		LLAddr = CreateUnseenAlloca(GenType(DestTy), "tmp.array.addr");
	}

	if (Arr->IsFoldable) {
		// For the sake of efficiency we memcpy the array over
		// into the destination.

		llvm::Value* LLGArray = GenConstGlobalArray(GenConstArray(Arr, DestTy));
	
		ulen TotalLinearLength = DestTy->GetTotalLinearLength();
	
		// TODO: Alignment!!
		llvm::MaybeAlign LLAlignment = llvm::MaybeAlign();
		Builder.CreateMemCpy(
			LLAddr  , LLAlignment,
			LLGArray, LLAlignment,
			TotalLinearLength * SizeOfTypeInBytes(GenType(DestTy->GetBaseType()))
		);

	} else {
		FillArrayViaGEP(Arr, LLAddr, DestTy);
	}

	return LLAddr;
}

llvm::Constant* arco::IRGenerator::GenConstArray(Array* Arr, ArrayType* DestTy) {

	bool ElmsAreArrs = DestTy->GetElementType()
		                     ->GetKind() == TypeKind::Array;

	llvm::SmallVector<llvm::Constant*, 4> LLElements;
	for (ulen i = 0; i < DestTy->GetLength(); i++) {
		llvm::Value* LLElmValue;
		if (i < Arr->Elements.size()) {
			Expr* Elm = Arr->Elements[i];
			if (ElmsAreArrs) {
				LLElmValue = GenConstArray(
					static_cast<Array*>(Elm),
					static_cast<ArrayType*>(DestTy->GetElementType())
				);
			} else {
				LLElmValue = GenRValue(Elm);
				// May need to perform another cast to ensure the element cast
				// to the type of the array.
				Type* ElmTy = Elm->CastTy ? Elm->CastTy : Elm->Ty;
				if (!ElmTy->Equals(DestTy->GetElementType())) {
					LLElmValue = GenCast(DestTy->GetElementType(), ElmTy, LLElmValue);
				}
			}
		} else {
			LLElmValue = GenConstValue(DestTy->GetElementType());
		}
		LLElements.push_back(llvm::cast<llvm::Constant>(LLElmValue));
	}

	llvm::ArrayType* LLArrType =
		llvm::ArrayType::get(GenType(DestTy->GetElementType()), DestTy->GetLength());
	return llvm::ConstantArray::get(LLArrType, LLElements);
}

void arco::IRGenerator::FillArrayViaGEP(Array* Arr, llvm::Value* LLAddr, ArrayType* DestTy) {
	
	bool ElmsAreArrs = DestTy->GetElementType()
		                     ->GetKind() == TypeKind::Array;

	for (ulen i = 0; i < DestTy->GetLength(); i++) {
		llvm::Value* LLAddrAtIndex = GetArrayIndexAddress(LLAddr, GetSystemUInt(i, LLContext, LLModule));
		if (i < Arr->Elements.size()) {
			Expr* Elm = Arr->Elements[i];
			if (ElmsAreArrs) {
				FillArrayViaGEP(
					static_cast<Array*>(Arr),
					LLAddrAtIndex,
					static_cast<ArrayType*>(DestTy->GetElementType())
				);
			} else {
				// TODO: Additional casting information!!
				GenAssignment(LLAddrAtIndex, Elm);
			}
		} else {
			// TODO: Default Assign instead!
			Builder.CreateStore(GenZeroedValue(DestTy->GetElementType()), LLAddrAtIndex);
		}
	}
}

llvm::Value* arco::IRGenerator::GenArrayAccess(ArrayAccess* Access) {

	llvm::Value* LLSite  = GenNode(Access->Site);
	llvm::Value* LLIndex = GenRValue(Access->Index);

	llvm::Type* LLSiteType = LLSite->getType()->getPointerElementType();
	llvm::Value* LLAccess;
	if (LLSiteType->isPointerTy()) {
		LLSite = CreateLoad(LLSite);
		LLAccess = CreateInBoundsGEP(LLSite, LLIndex);
	} else {
		LLAccess = GetArrayIndexAddress(LLSite, LLIndex);
	}

	if (Access->Site->Ty->GetKind() == TypeKind::Array) {
		LLAccess->setName("arr.access");
	} else {
		LLAccess->setName("ptr.access");
	}

	return LLAccess;
}

llvm::Value* arco::IRGenerator::GenTypeCast(TypeCast* Cast) {
	return GenCast(Cast->Ty, Cast->Value->Ty, GenRValue(Cast->Value));
}

llvm::Value* arco::IRGenerator::GenStructInitializer(StructInitializer* StructInit, llvm::Value* LLAddr) {
	StructType* StructTy = static_cast<StructType*>(StructInit->Ty);
	StructDecl* Struct = StructTy->GetStruct();

	if (!LLAddr) {
		LLAddr = CreateUnseenAlloca(GenStructType(StructTy), "tmp.structinit.addr");
	}

	std::unordered_set<ulen> ConsumedFieldIdxs;
	for (ulen i = 0; i < StructInit->Args.size(); i++) {
		NonNamedValue Value = StructInit->Args[i];
		
		llvm::Value* LLFieldAddr = CreateStructGEP(LLAddr, Struct->Fields[i]->FieldIdx);
		GenAssignment(LLFieldAddr, Value.E);
		ConsumedFieldIdxs.insert(i);
	}

	for (VarDecl* Field : Struct->Fields) {
		if (ConsumedFieldIdxs.find(Field->FieldIdx) == ConsumedFieldIdxs.end()) {
			llvm::Value* LLFieldAddr = CreateStructGEP(LLAddr, Field->FieldIdx);
			if (Field->Assignment) {
				GenAssignment(LLFieldAddr, Field->Assignment);
			} else {
				// TODO: assign default value
			}
		}
	}

	return LLAddr;
}

llvm::Value* arco::IRGenerator::GenAdd(llvm::Value* LLLHS, llvm::Value* LLRHS, Type* Ty) {
	return Builder.CreateAdd(LLLHS, LLRHS);
}

llvm::Value* arco::IRGenerator::GenSub(llvm::Value* LLLHS, llvm::Value* LLRHS, Type* Ty) {
	return Builder.CreateSub(LLLHS, LLRHS);
}

llvm::Value* arco::IRGenerator::GenMul(llvm::Value* LLLHS, llvm::Value* LLRHS, Type* Ty) {
	return Builder.CreateMul(LLLHS, LLRHS);
}

llvm::Value* arco::IRGenerator::GenDiv(llvm::Value* LLLHS, llvm::Value* LLRHS, Type* Ty) {
	if (Ty->IsSigned()) {
		return Builder.CreateSDiv(LLLHS, LLRHS);
	} else {
		return Builder.CreateUDiv(LLLHS, LLRHS);
	}
}

void arco::IRGenerator::GenLoopCondJump(llvm::BasicBlock* LLCondBB,
	                                    llvm::BasicBlock* LLBodyBB,
	                                    llvm::BasicBlock* LLEndBB,
	                                    Expr* Cond) {
	// Jumping directly into the loop condition
	Builder.CreateBr(LLCondBB);
	Builder.SetInsertPoint(LLCondBB);

	llvm::Value* LLCond = Cond ? GenCond(Cond) : llvm::ConstantInt::getTrue(LLContext);
	Builder.CreateCondBr(LLCond, LLBodyBB, LLEndBB);
}

llvm::Value* arco::IRGenerator::GenCond(Expr* Cond) {
	return GenRValue(Cond);
}

void arco::IRGenerator::GenBlock(llvm::BasicBlock* LLBB, ScopeStmts& Stmts) {
	if (LLBB) {
		// Unconditionally jump into the next block.
		GenBranchIfNotTerm(LLBB);
		Builder.SetInsertPoint(LLBB);
	}
	for (AstNode* Stmt : Stmts) {
		GenNode(Stmt);
	}
}

llvm::Value* arco::IRGenerator::GenCast(Type* ToType, Type* FromType, llvm::Value* LLValue) {
	
	llvm::Type* LLCastType = GenType(ToType);
	switch (ToType->GetKind()) {
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
	case TypeKind::Char:
		//  --- TO Integers ---
		if (FromType->IsInt()) {
			// Int to Int
			ulen ToSize   = ToType->GetSizeInBytes(LLModule);
			ulen FromSize = FromType->GetSizeInBytes(LLModule);

			if (ToSize < FromSize) {
				// Signed and unsigned downcasting uses trunc
				return Builder.CreateTrunc(LLValue, LLCastType);
			} else {
				if (ToType->IsSigned()) {
					// Signed upcasting
					return Builder.CreateSExt(LLValue, LLCastType);
				} else {
					// Unsigned upcasting
					return Builder.CreateZExt(LLValue, LLCastType);
				}
			}
		}
		goto missingCaseLab;
	case TypeKind::Pointer: {
		//  --- TO Pointers ---
		if (FromType->GetKind() == TypeKind::Null) {
			return LLValue; // Already handled during generation
		} else if (FromType->GetKind() == TypeKind::Array) {
			// TODO:!!
			return DecayArray(LLValue);
		}
		goto missingCaseLab;
	}
	case TypeKind::CStr: {
		if (FromType->GetKind() == TypeKind::Null) {
			return LLValue; // Already handled during generation
		} else if (FromType->GetKind() == TypeKind::Array) {
			// TODO:!!
			return DecayArray(LLValue);
		}
		goto missingCaseLab;
	}
	default: {
missingCaseLab:
		llvm::outs() << FromType->ToString() << " => " << ToType->ToString() << "\n";
		assert(!"Missing cast case");
		return nullptr;
	}
	}
}

llvm::Value* arco::IRGenerator::CreateLoad(llvm::Value* LLAddr) {
	return Builder.CreateLoad(LLAddr->getType()->getPointerElementType(), LLAddr);
}

llvm::Constant* arco::IRGenerator::GenConstValue(Type* Ty) {
	return GenZeroedValue(Ty);
}

llvm::Constant* arco::IRGenerator::GenZeroedValue(Type* Ty) {
	switch (Ty->GetKind()) {
	case TypeKind::Int8: case TypeKind::Char:
		return GetLLInt8(0, LLContext);
	case TypeKind::UnsignedInt8:    return GetLLUInt8(0, LLContext);
	case TypeKind::Int16:           return GetLLInt16(0, LLContext);
	case TypeKind::UnsignedInt16:   return GetLLUInt16(0, LLContext);
	case TypeKind::Int32:           return GetLLInt32(0, LLContext);
	case TypeKind::UnsignedInt32:   return GetLLUInt32(0, LLContext);
	case TypeKind::Int64:           return GetLLInt64(0, LLContext);
	case TypeKind::UnsignedInt64:   return GetLLUInt64(0, LLContext);
	case TypeKind::Int:             return GetSystemInt(0, LLContext, LLModule);
	case TypeKind::UnsignedInt:     return GetSystemUInt(0, LLContext, LLModule);
	case TypeKind::Pointer:
	case TypeKind::CStr:
		return llvm::Constant::getNullValue(GenType(Ty));
	case TypeKind::Array:
		return llvm::ConstantAggregateZero::get(GenType(Ty));
	default:
		assert(!"Failed to implement GenZeroedValue() case!");
		return nullptr;
	}
}

void arco::IRGenerator::StructArrayCallDefaultConstructors(Type* BaseTy,
	                                                       llvm::Value* LLArrStartPtr,
	                                                       llvm::Value* LLTotalLinearLength) {
	GenInternalArrayLoop(BaseTy, LLArrStartPtr, LLTotalLinearLength,
		[this](llvm::PHINode* LLElmAddr, Type* BaseTy) {
			CallDefaultConstructor(LLElmAddr, static_cast<StructType*>(BaseTy));
		});
}

void arco::IRGenerator::GenInternalArrayLoop(Type* BaseTy,
	                                         llvm::Value* LLArrStartPtr,
	                                         llvm::Value* LLTotalLinearLength,
	                                         const std::function<void(llvm::PHINode*, Type*)>& CodeGenCallback) {
	
	llvm::BasicBlock* BeforeLoopBB = Builder.GetInsertBlock();
	llvm::Value* LLEndOfArrPtr = CreateInBoundsGEP(LLArrStartPtr, { LLTotalLinearLength });

	llvm::BasicBlock* LoopBB    = llvm::BasicBlock::Create(LLContext, "array.loop", LLFunc);
	llvm::BasicBlock* LoopEndBB = llvm::BasicBlock::Create(LLContext, "array.end", LLFunc);

	Builder.CreateBr(LoopBB);
	Builder.SetInsertPoint(LoopBB);

	// Pointer used to traverse through the array
	llvm::PHINode* LLArrPtr = Builder.CreatePHI(llvm::PointerType::get(GenType(BaseTy), 0), 0, "obj.loop.ptr");

	// Incoming value to the start of the array from the incoming block
	LLArrPtr->addIncoming(LLArrStartPtr, BeforeLoopBB);

	CodeGenCallback(LLArrPtr, BaseTy);

	// Move to the next element in the array
	llvm::Value* LLNextElementPtr = CreateInBoundsGEP(LLArrPtr, { GetSystemUInt(1, LLContext, LLModule) });

	// Checking if all objects have been looped over
	llvm::Value* LLLoopEndCond = Builder.CreateICmpEQ(LLNextElementPtr, LLEndOfArrPtr);
	Builder.CreateCondBr(LLLoopEndCond, LoopEndBB, LoopBB);

	// The value must come from the block that 'LLNextCount' is created
	// in which would be whatever the current block is.
	llvm::BasicBlock* LLCurBlock = Builder.GetInsertBlock();
	LLArrPtr->addIncoming(LLNextElementPtr, LLCurBlock);

	// End of loop
	Builder.SetInsertPoint(LoopEndBB);

}

llvm::Value* arco::IRGenerator::DecayArray(llvm::Value* LLArray) {
	llvm::Value* LLValue = CreateInBoundsGEP(LLArray,
				{ GetSystemUInt(0, LLContext, LLModule), GetSystemUInt(0, LLContext, LLModule) });
	LLValue->setName("array.decay");
	return LLValue;
}

llvm::Value* arco::IRGenerator::MultiDimensionalArrayToPointerOnly(llvm::Value* LLArray, ArrayType* ArrTy) {
	ulen Depth = ArrTy->GetDepthLevel();
	llvm::SmallVector<llvm::Value*, 4> LLIdxs;
	for (ulen i = 0; i < Depth + 1; i++) {
		LLIdxs.push_back(GetSystemUInt(0, LLContext, LLModule));
	}
	return CreateInBoundsGEP(LLArray, LLIdxs);
}

inline llvm::Value* arco::IRGenerator::CreateInBoundsGEP(llvm::Value* LLAddr, llvm::ArrayRef<llvm::Value*> IdxList) {
	return Builder.CreateInBoundsGEP(
		              LLAddr->getType()->getScalarType()->getPointerElementType(),
		              LLAddr, IdxList);
}

inline llvm::Value* arco::IRGenerator::GetArrayIndexAddress(llvm::Value* LLArray, llvm::Value* LLIndex) {
	return CreateInBoundsGEP(
		LLArray, { GetSystemUInt(0, LLContext, LLModule), LLIndex });
}

inline llvm::Value* arco::IRGenerator::CreateStructGEP(llvm::Value* LLAddr, ulen Idx) {
	return Builder.CreateConstInBoundsGEP2_32(
		LLAddr->getType()->getScalarType()->getPointerElementType(), LLAddr, 0, Idx);
}

llvm::GlobalVariable* arco::IRGenerator::GenLLVMGlobalVariable(llvm::StringRef Name, llvm::Type* LLType) {
	LLModule.getOrInsertGlobal(Name, LLType);
	return LLModule.getNamedGlobal(Name);
}

llvm::GlobalVariable* arco::IRGenerator::GenConstGlobalArray(llvm::Constant* LLArray, bool DSOLocal) {

	std::string LLName = std::string("__global.array.")
		                       + std::to_string(Context.NumGeneratedGlobalVars++);

	llvm::GlobalVariable* LLGlobalVar = GenLLVMGlobalVariable(LLName, LLArray->getType());
	LLGlobalVar->setInitializer(LLArray);

	LLGlobalVar->setConstant(true);
	LLGlobalVar->setDSOLocal(DSOLocal);

	return LLGlobalVar;
}

inline ulen arco::IRGenerator::SizeOfTypeInBytes(llvm::Type* LLType) {
	const llvm::DataLayout& LLDataLayout = LLModule.getDataLayout();
	llvm::TypeSize LLTypeSize = LLDataLayout.getTypeAllocSize(LLType);
	return LLTypeSize.getFixedSize();
}

void arco::IRGenerator::GenBranchIfNotTerm(llvm::BasicBlock* LLBB) {
	// Avoiding back-to-back branching.
	llvm::BasicBlock* CurBB = Builder.GetInsertBlock();
	if (!CurBB->getTerminator()) {
		// Unconditionally branch
		Builder.CreateBr(LLBB);
	}
}

void arco::IRGenerator::GenBranchOnCond(Expr* Cond, llvm::BasicBlock* LLTrueBB, llvm::BasicBlock* LLFalseBB) {
	// See: https://github.com/llvm/llvm-project/blob/839ac62c5085d895d3165bc5024db623a7a78813/clang/lib/CodeGen/CodeGenFunction.cpp
	// EmitBranchOnBoolExpr

	Builder.CreateCondBr(GenCond(Cond), LLTrueBB, LLFalseBB);
}

void arco::IRGenerator::GenAssignment(llvm::Value* LLAddress, Expr* Value) {
	if (Value->Is(AstKind::ARRAY)) {
		GenArray(static_cast<Array*>(Value), LLAddress);
	} else if (Value->Is(AstKind::STRUCT_INITIALIZER)) {
		GenStructInitializer(static_cast<StructInitializer*>(Value), LLAddress);
	} else {
		llvm::Value* LLAssignment = GenRValue(Value);
		Builder.CreateStore(LLAssignment, LLAddress);
	}
}

void arco::IRGenerator::GenDefaultValue(Type* Ty, llvm::Value* LLAddr) {
	if (Ty->GetKind() == TypeKind::Struct) {
		StructType* StructTy = static_cast<StructType*>(Ty);
		StructDecl* Struct = StructTy->GetStruct();

		if (Struct->FieldsHaveAssignment) {
			CallDefaultConstructor(LLAddr, StructTy);
		} else {
			ulen TotalLinearLength = SizeOfTypeInBytes(GenStructType(StructTy));
			llvm::Align LLAlignment = llvm::Align();
			Builder.CreateMemSet(
				LLAddr,
				GetLLUInt8(0, LLContext),
				GetLLUInt64(TotalLinearLength, LLContext),
				LLAlignment
			);
		}
	} else if (Ty->GetKind() == TypeKind::Array) {
		ArrayType* ArrTy = static_cast<ArrayType*>(Ty);
		Type* BaseTy = ArrTy->GetBaseType();
		if (BaseTy->GetKind() == TypeKind::Struct) {
			StructDecl* Struct = static_cast<StructType*>(BaseTy)->GetStruct();
			if (Struct->FieldsHaveAssignment) {
				// Cannot simply memset the array to zero must call the default constructor.
				llvm::Value* LLArrStartPtr = MultiDimensionalArrayToPointerOnly(LLAddr, ArrTy);
				llvm::Value* LLTotalLinearLength = GetSystemUInt(ArrTy->GetTotalLinearLength(), LLContext, LLModule);
				StructArrayCallDefaultConstructors(BaseTy, LLArrStartPtr, LLTotalLinearLength);
				return;
			}
		}
		
		// Memset to zero.
		ulen TotalLinearLength = ArrTy->GetTotalLinearLength();
		TotalLinearLength *= SizeOfTypeInBytes(GenType(BaseTy));
		llvm::Align LLAlignment = llvm::Align();
		Builder.CreateMemSet(
			LLAddr,
			GetLLUInt8(0, LLContext),
			GetLLUInt64(TotalLinearLength, LLContext),
			LLAlignment
		);
		
	} else {
		Builder.CreateStore(GenZeroedValue(Ty), LLAddr);
	}
}

void arco::IRGenerator::CallDefaultConstructor(llvm::Value* LLAddr, StructType* StructTy) {
	llvm::Function* LLDefaultConstructor = GenDefaultConstructorDecl(StructTy->GetStruct());
	Builder.CreateCall(LLDefaultConstructor, { LLAddr });
}

llvm::Function* arco::IRGenerator::GenDefaultConstructorDecl(StructDecl* Struct) {
	if (Struct->LLDefaultConstructor) {
		return Struct->LLDefaultConstructor;
	}

	llvm::FunctionType* LLFuncType = llvm::FunctionType::get(
		llvm::Type::getVoidTy(LLContext),
		{ llvm::PointerType::get(GenStructType(Struct), 0) },
		false
	);

	llvm::Function* LLFunc = llvm::Function::Create(
		LLFuncType,
		llvm::Function::ExternalLinkage,
		std::string("default.constructor.") + Struct->Name.Text,
		LLModule
	);
	LLFunc->setDSOLocal(true);

	Context.DefaultConstrucorsNeedingCreated.push(Struct);

	Struct->LLDefaultConstructor = LLFunc;
	return Struct->LLDefaultConstructor;
}

llvm::Value* arco::IRGenerator::CreateUnseenAlloca(llvm::Type* LLTy, const char* Name) {
	llvm::BasicBlock* BackupInsertBlock = Builder.GetInsertBlock();
	llvm::BasicBlock* LLEntryBlock = &LLFunc->getEntryBlock();
	if (LLEntryBlock->getInstList().empty()) {
		Builder.SetInsertPoint(LLEntryBlock);
	} else {
		Builder.SetInsertPoint(&LLEntryBlock->getInstList().front());
	}
	llvm::Value* LLAddr = Builder.CreateAlloca(LLTy, nullptr);
	LLAddr->setName(Name);
	Builder.SetInsertPoint(BackupInsertBlock);
	return LLAddr;
}
