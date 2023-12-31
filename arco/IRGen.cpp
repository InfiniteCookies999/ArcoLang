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
	case TypeKind::Bool:
		return llvm::Type::getInt1Ty(LLContext);
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

void arco::IRGenerator::GenGlobalVar(VarDecl* Global) {

	GenGlobalVarDecl(Global);

	llvm::GlobalVariable* LLGVar =
		llvm::cast<llvm::GlobalVariable>(Global->LLAddress);
	auto [NoFurtherInit, InitValue] = GenGlobalVarInitializeValue(Global);

	LLGVar->setInitializer(InitValue);

	if (!NoFurtherInit) {
		Context.GlobalPostponedAssignments.push_back(Global);
	}
}

void arco::IRGenerator::GenImplicitDefaultConstructorBody(StructDecl* Struct) {
	LLFunc = Struct->LLDefaultConstructor;

	llvm::BasicBlock* LLEntryBlock = llvm::BasicBlock::Create(LLContext, "func.entry", LLFunc);
	Builder.SetInsertPoint(LLEntryBlock);

	// Allocating "this" pointer
	llvm::Value* LLThisAddr = Builder.CreateAlloca(LLFunc->getArg(0)->getType(), nullptr, "this");

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

void arco::IRGenerator::GenGlobalInitFuncBody() {
	llvm::BasicBlock* LLEntryBlock = llvm::BasicBlock::Create(LLContext, "entry.block", Context.LLInitGlobalFunc);

	LLFunc = Context.LLInitGlobalFunc;	
	Builder.SetInsertPoint(LLEntryBlock);

	// Iterator since it is modifiable during generation.
	auto Itr = Context.GlobalPostponedAssignments.begin();
	while (Itr != Context.GlobalPostponedAssignments.end()) {
		VarDecl* Global = *Itr;
		if (Global->Assignment) {
			GenAssignment(Global->LLAddress, Global->Assignment);
		} else {
			if (Global->Ty->GetKind() == TypeKind::Struct) {
				CallDefaultConstructor(Global->LLAddress, static_cast<StructType*>(Global->Ty));
			} else if (Global->Ty->GetKind() == TypeKind::Array &&
				   static_cast<ArrayType*>(Global->Ty)->GetBaseType()->GetKind() == TypeKind::Struct) {
				ArrayType* ArrayTy = static_cast<ArrayType*>(Global->Ty);
				llvm::Value* LLArrStartPtr = MultiDimensionalArrayToPointerOnly(Global->LLAddress, ArrayTy);
				llvm::Value* LLTotalLinearLength = GetSystemUInt(ArrayTy->GetTotalLinearLength(), LLContext, LLModule);
				StructArrayCallDefaultConstructors(ArrayTy->GetBaseType(), LLArrStartPtr, LLTotalLinearLength);
			}
		}
		++Itr;
	}
	
	Builder.CreateRetVoid();
}

void arco::IRGenerator::GenFuncDecl(FuncDecl* Func) {
	if (Func->LLFunction) return;

	if (Func->Mods & ModKinds::NATIVE) {
		auto Itr = Context.LLVMIntrinsicsTable.find(Func->Name);
		if (Itr != Context.LLVMIntrinsicsTable.end()) {
			Func->LLVMIntrinsicID = Itr->second;
			return;
		}
	}
	
	// TODO: Native functions will not need to return data structures
	//       in the same way.

	llvm::Type* LLRetTy;
	if (Func->RetTy->GetKind() == TypeKind::Struct) {
		StructType* StructTy = static_cast<StructType*>(Func->RetTy);
		
		ulen SizeInBytes = SizeOfTypeInBytes(GenStructType(StructTy));
		if (SizeInBytes <= LLModule.getDataLayout().getPointerSize()) {
			// Return type is optimized to fit into an integer.
			auto NextPow2 = [](ulen V) {
				--V;
				V |= V >> 1;
				V |= V >> 2;
				V |= V >> 4;
				V |= V >> 8;
				V |= V >> 16;
				V++;
				return V;
			};
			Func->UsesOptimizedIntRet = true;
			LLRetTy = llvm::Type::getIntNTy(LLContext, NextPow2(SizeInBytes) * 8); // *8 because bits

		} else {
			// Copy elision case by passing return value as param.
			Func->UsesParamRetSlot = true;
			LLRetTy = llvm::Type::getVoidTy(LLContext);
		}
	} else {
		LLRetTy = Func == Context.MainEntryFunc ? llvm::Type::getInt32Ty(LLContext)
		                                        : GenType(Func->RetTy);
	}

	llvm::Twine LLFuncName = Func->Name.Text;
	llvm::Twine LLFullFuncName = (Func == Context.MainEntryFunc ||
		                          (Func->Mods & ModKinds::NATIVE))
		                       ? LLFuncName
		                       : LLFuncName.concat(".arco");
	
	llvm::SmallVector<llvm::Type*, 4> LLParamTypes;
	ulen ImplicitParams = 0;

	if (Func->Struct) {
		// Member functions recieve pointers to the struct they
		// are contained inside of.

		LLParamTypes.push_back(llvm::PointerType::get(GenStructType(Func->Struct), 0));
		++ImplicitParams;
	}
	if (Func->UsesParamRetSlot) {
		LLParamTypes.push_back(llvm::PointerType::get(GenType(Func->RetTy), 0));
		++ImplicitParams;
	}
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

	Func->LLFunction = LLFunc;
	
	if (Func->UsesParamRetSlot) {
		GetElisionRetSlotAddr(Func)->setName("ret.addr");
	}
	for (ulen i = 0; i < Func->Params.size(); i++) {
		LLFunc->getArg(i + ImplicitParams)->setName(llvm::Twine(Func->Params[i]->Name.Text).concat(".param"));
	}

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
		} else if (Func->RetTy->GetKind() != TypeKind::Void &&
			       !Func->UsesParamRetSlot) {
			LLRetAddr = Builder.CreateAlloca(GenType(Func->RetTy), nullptr, "ret.val");
		}
	}

	// Allocating space for the variables
	//
	for (VarDecl* Var : Func->AllocVars) {
		if (Func->UsesParamRetSlot && Func->NumReturns == 1 && Var->IsLocalRetValue) {
			// RVO case in which although the variable was declared as inside
			// of the function since it is returned by the function and the
			// function uses a parameter for a return slot the variable may use
			// the address of the return slot instead.
			Var->LLAddress = GetElisionRetSlotAddr(CFunc);
		} else {
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
	}

	// Storing the incoming parameters
	//
	ulen LLParamIndex = 0;
	if (Func->Struct) {
		// Member function pointer.
		llvm::Value* LLThisAddr = Builder.CreateAlloca(LLFunc->getArg(0)->getType(), nullptr, "this.addr");
		Builder.CreateStore(LLFunc->getArg(LLParamIndex++), LLThisAddr);
		LLThis = CreateLoad(LLThisAddr);
		LLThis->setName("this");
	}
	if (Func->UsesParamRetSlot) {
		++LLParamIndex;
	}
	for (VarDecl* Param : Func->Params) {
		Builder.CreateStore(LLFunc->getArg(LLParamIndex++), Param->LLAddress);
	}

	if (Func->IsConstructor) {
		GenConstructorBodyFieldAssignments(Func->Struct);
	}

	if (Func == Context.MainEntryFunc) {
		Context.LLInitGlobalFunc = GenGlobalInitFuncDecl();
		Builder.CreateCall(Context.LLInitGlobalFunc);
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
		if (Func->UsesOptimizedIntRet) {
			Builder.CreateRet(GenReturnValueForOptimizedStructAsInt(LLRetAddr));
		} else if (LLRetAddr) {
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

llvm::Function* arco::IRGenerator::GenGlobalInitFuncDecl() {
	llvm::FunctionType* LLFuncType =
		llvm::FunctionType::get(llvm::Type::getVoidTy(LLContext), false);
	llvm::Function* LLInitFunc =
		llvm::Function::Create(
			LLFuncType,
			llvm::Function::ExternalLinkage,
			"__arco.init.globals",
			LLModule
		);
	return LLInitFunc;
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
	case AstKind::BOOL_LITERAL: {
		BoolLiteral* B = static_cast<BoolLiteral*>(Node);
		if (B->TOF) return llvm::ConstantInt::getTrue(LLContext);
		else        return llvm::ConstantInt::getFalse(LLContext);
	}
	case AstKind::IDENT_REF:
		return GenIdentRef(static_cast<IdentRef*>(Node));
	case AstKind::FIELD_ACCESSOR:
		return GenFieldAccessor(static_cast<FieldAccessor*>(Node));
	case AstKind::THIS_REF:
		return LLThis;
	case AstKind::FUNC_CALL:
		return GenFuncCall(static_cast<FuncCall*>(Node), nullptr);
	case AstKind::ARRAY:
		return GenArray(static_cast<Array*>(Node), nullptr);
	case AstKind::ARRAY_ACCESS:
		return GenArrayAccess(static_cast<ArrayAccess*>(Node));
	case AstKind::TYPE_CAST:
		return GenTypeCast(static_cast<TypeCast*>(Node));
	case AstKind::STRUCT_INITIALIZER:
		return GenStructInitializer(static_cast<StructInitializer*>(Node), nullptr);
	case AstKind::HEAP_ALLOC:
		return GenHeapAlloc(static_cast<HeapAlloc*>(Node));
	default:
		assert(!"Unimplemented GenNode() case!");
		return nullptr;
	}
}

void arco::IRGenerator::GenGlobalVarDecl(VarDecl* Global) {
	if (Global->LLAddress) return; // Do not generate it twice

	std::string Name = "__global." + Global->Name.Text.str();
	Name += "." + std::to_string(Context.NumGeneratedGlobalVars++);

	llvm::GlobalVariable* LLGVar = GenLLVMGlobalVariable(Name, GenType(Global->Ty));
	Global->LLAddress = LLGVar;

	if (Global->Mods & ModKinds::NATIVE) {
#ifdef _WIN32
		LLGVar->setDLLStorageClass(llvm::GlobalValue::DLLImportStorageClass);
#endif
	} else {
		LLGVar->setDSOLocal(true);
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
	case AstKind::UNARY_OP: {
		if (static_cast<UnaryOp*>(E)->Op == '*') {
			LLValue = CreateLoad(LLValue);
		}
		break;
	}
	case AstKind::STRUCT_INITIALIZER: {
		// Since an unseen allocation must have been generated to
		// create the allocation of the struct the struct is currently
		// a l-value.
		LLValue = CreateLoad(LLValue);
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
		
		if (CFunc->RetTy->GetKind() == TypeKind::Struct) {
			if (CFunc->UsesOptimizedIntRet) {
				LLRetValue = GenReturnValueForOptimizedStructAsInt(GenNode(Ret->Value));
			}
			// Check for cases in which the value needs to be copied into the elision
			// return address.
			else if (
				(Ret->Value->Is(AstKind::IDENT_REF) &&
				 !static_cast<IdentRef*>(Ret->Value)->Var->IsLocalRetValue) ||
				(Ret->Value->Is(AstKind::UNARY_OP))  // dereferencing another object need to copy.
					) {
				llvm::Value* LLToAddr   = GetElisionRetSlotAddr(CFunc);
				llvm::Value* LLFromAddr = GenNode(Ret->Value);

				// TODO: In the future if the object has a move constructor that would
				// need to be called instead of copying.
				CopyStructObject(LLToAddr, LLFromAddr, static_cast<StructType*>(CFunc->RetTy)->GetStruct());

			} else {
				GenReturnByStoreToElisionRetSlot(Ret->Value);
			}
		} else if (!Ret->Value && CFunc == Context.MainEntryFunc) {
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

		// Multiple returns exists. All returns jump to the LLFuncEndBB.
		// Storing the return value into LLRetAddr and letting LLFuncEndBB
		// handle the code for returning the value.

		if (CFunc->RetTy->GetKind() == TypeKind::Struct) {

			// Even if the function uses optimized integer returning still need
			// to copy to the return address. If it does use optimized integer
			// returning the bitcasting will then occur at the LLFuncEndBB block.

			llvm::Value* LLToAddr = CFunc->UsesParamRetSlot
				                          ? GetElisionRetSlotAddr(CFunc)
				                          : LLRetAddr;
			llvm::Value* LLFromAddr = GenNode(Ret->Value);
			// TODO: Move constructors if supported

			CopyStructObject(LLToAddr, LLFromAddr, static_cast<StructType*>(CFunc->RetTy)->GetStruct());

		} else if (!Ret->Value && CFunc == Context.MainEntryFunc) {
			Builder.CreateStore(GetLLInt32(0, LLContext), LLRetAddr);
		} else if (Ret->Value) {
			Builder.CreateStore(GenRValue(Ret->Value), LLRetAddr);
		}

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
		llvm::Value* LLAddress = GenNode(BinOp->LHS);
		GenAssignment(LLAddress, BinOp->RHS);
		return LLAddress;
	}
	//
	// Arithmetic
	//
	case '+': {
		llvm::Value* LLLHS = GenRValue(BinOp->LHS);
		llvm::Value* LLRHS = GenRValue(BinOp->RHS);
		if (BinOp->Ty->IsPointer()) {
			// Pointer arithmetic

			llvm::Value* PtrAddr, *Add;
			bool LArray = BinOp->LHS->Ty->GetKind() == TypeKind::Array;
			bool RArray = BinOp->RHS->Ty->GetKind() == TypeKind::Array;

			if (LArray || RArray) {
				PtrAddr = LArray ? ArrayToPointer(LLLHS) : LLRHS;
				Add     = LArray ? LLRHS : LLLHS;
			} else {
				PtrAddr = BinOp->LHS->Ty->IsPointer() ? LLLHS : LLRHS;
				Add     = BinOp->LHS->Ty->IsPointer() ? LLRHS : LLLHS;
			}
 
			return CreateInBoundsGEP(PtrAddr, { Add });
		} else {
			return GenAdd(LLLHS, LLRHS, BinOp->Ty);
		}
	}
	case '-': {
		llvm::Value* LLLHS = GenRValue(BinOp->LHS);
		llvm::Value* LLRHS = GenRValue(BinOp->RHS);

		if (BinOp->Ty->IsPointer()) {

			llvm::Value* PtrAddr = LLLHS;
			if (BinOp->LHS->Ty->GetKind() == TypeKind::Array) {
				PtrAddr = ArrayToPointer(PtrAddr);
			}

			llvm::Value* Sub = Builder.CreateNeg(LLRHS);
			return CreateInBoundsGEP(PtrAddr, { Sub });
		} else {
			return GenSub(LLLHS, LLRHS, BinOp->Ty);
		}
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

		if (BinOp->Ty->IsPointer()) {
			// Pointer arithmetic
			llvm::Value* V = CreateInBoundsGEP(CreateLoad(LLLHS), { LLRHS });
			Builder.CreateStore(V, LLLHS);
			return V;
		} else {
			llvm::Value* LLLHSRV = CreateLoad(LLLHS);
			llvm::Value* V = BinOp->Ty->IsInt() ? Builder.CreateAdd(LLLHSRV, LLRHS)
												: Builder.CreateFAdd(LLLHSRV, LLRHS);
			Builder.CreateStore(V, LLLHS);
			return V;
		}
	}
	case TokenKind::MINUS_EQ: { // -=
		llvm::Value* LLLHS = GenNode(BinOp->LHS);
		llvm::Value* LLRHS = GenRValue(BinOp->RHS);

		if (BinOp->Ty->IsPointer()) {
			llvm::Value* V = CreateInBoundsGEP(CreateLoad(LLLHS), { Builder.CreateNeg(LLRHS) });
			Builder.CreateStore(V, LLLHS);
			return V;
		} else {
			llvm::Value* LLLHSRV = CreateLoad(LLLHS);
			llvm::Value* V = BinOp->Ty->IsInt() ? Builder.CreateSub(LLLHSRV, LLRHS)
												: Builder.CreateFSub(LLLHSRV, LLRHS);
			Builder.CreateStore(V, LLLHS);
			return V;
		}
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
	case TokenKind::EQ_EQ: {
		TypeKind LK = BinOp->LHS->Ty->GetKind();
		TypeKind RK = BinOp->RHS->Ty->GetKind();
		if (LK == TypeKind::Null && RK == TypeKind::Null) {
			return llvm::ConstantInt::getTrue(LLContext);
		} else {
			llvm::Value* LLLHS = GenRValue(BinOp->LHS);
			llvm::Value* LLRHS = GenRValue(BinOp->RHS);
			return Builder.CreateICmpEQ(LLLHS, LLRHS);
		}
	}
	case TokenKind::EXL_EQ: {
		TypeKind LK = BinOp->LHS->Ty->GetKind();
		TypeKind RK = BinOp->RHS->Ty->GetKind();
		if (LK == TypeKind::Null && RK == TypeKind::Null) {
			return llvm::ConstantInt::getFalse(LLContext);
		} else {
			llvm::Value* LLLHS = GenRValue(BinOp->LHS);
			llvm::Value* LLRHS = GenRValue(BinOp->RHS);
			return Builder.CreateICmpNE(LLLHS, LLRHS);
		}
	}
	case '<': {
		TypeKind LK = BinOp->LHS->Ty->GetKind();
		TypeKind RK = BinOp->RHS->Ty->GetKind();
		if (LK == TypeKind::Null && RK == TypeKind::Null) {
			return llvm::ConstantInt::getFalse(LLContext);
		} else {
			llvm::Value* LLLHS = GenRValue(BinOp->LHS);
			llvm::Value* LLRHS = GenRValue(BinOp->RHS);

			if (BinOp->LHS->Ty->IsSigned() ||
				BinOp->RHS->Ty->IsSigned()) {
				return Builder.CreateICmpSLT(LLLHS, LLRHS);
			} else {
				return Builder.CreateICmpULT(LLLHS, LLRHS);
			}
		}
	}
	case '>': {
		TypeKind LK = BinOp->LHS->Ty->GetKind();
		TypeKind RK = BinOp->RHS->Ty->GetKind();
		if (LK == TypeKind::Null && RK == TypeKind::Null) {
			return llvm::ConstantInt::getFalse(LLContext);
		} else {
			llvm::Value* LLLHS = GenRValue(BinOp->LHS);
			llvm::Value* LLRHS = GenRValue(BinOp->RHS);

			if (BinOp->LHS->Ty->IsSigned() ||
				BinOp->RHS->Ty->IsSigned()) {
				return Builder.CreateICmpSGT(LLLHS, LLRHS);
			} else {
				return Builder.CreateICmpUGT(LLLHS, LLRHS);
			}	
		}
	}
	case TokenKind::LT_EQ: {
		TypeKind LK = BinOp->LHS->Ty->GetKind();
		TypeKind RK = BinOp->RHS->Ty->GetKind();
		if (LK == TypeKind::Null && RK == TypeKind::Null) {
			return llvm::ConstantInt::getTrue(LLContext);
		} else {
			llvm::Value* LLLHS = GenRValue(BinOp->LHS);
			llvm::Value* LLRHS = GenRValue(BinOp->RHS);

			if (BinOp->LHS->Ty->IsSigned() ||
				BinOp->RHS->Ty->IsSigned()) {
				return Builder.CreateICmpSLE(LLLHS, LLRHS);
			} else {
				return Builder.CreateICmpULE(LLLHS, LLRHS);
			}	
		}
	}
	case TokenKind::GT_EQ: {
		TypeKind LK = BinOp->LHS->Ty->GetKind();
		TypeKind RK = BinOp->RHS->Ty->GetKind();
		if (LK == TypeKind::Null && RK == TypeKind::Null) {
			return llvm::ConstantInt::getTrue(LLContext);
		} else {
			llvm::Value* LLLHS = GenRValue(BinOp->LHS);
			llvm::Value* LLRHS = GenRValue(BinOp->RHS);

			if (BinOp->LHS->Ty->IsSigned() ||
				BinOp->RHS->Ty->IsSigned()) {
				return Builder.CreateICmpSGE(LLLHS, LLRHS);
			} else {
				return Builder.CreateICmpUGE(LLLHS, LLRHS);
			}	
		}
	}
	default:
		assert(!"Failed to implement GenBinaryOp() case!");
		return nullptr;
	}
}

llvm::Value* arco::IRGenerator::GenUnaryOp(UnaryOp* UniOp) {
	
	auto GetOneValue = [](Type* Ty, llvm::LLVMContext& LLContext, llvm::Module& LLModule) {
		llvm::Value* LLOne = nullptr;
		switch (Ty->GetKind()) {
		case TypeKind::Int8:
		case TypeKind::Char:
			LLOne = GetLLInt8(1, LLContext);
			break;
		case TypeKind::UnsignedInt8:
			LLOne = GetLLUInt8(1, LLContext);
			break;
		case TypeKind::Int16:
			LLOne = GetLLInt16(1, LLContext);
			break;
		case TypeKind::UnsignedInt16:
			LLOne = GetLLUInt16(1, LLContext);
			break;
		case TypeKind::Int32:
			LLOne = GetLLInt32(1, LLContext);
			break;
		case TypeKind::UnsignedInt32:
			LLOne = GetLLUInt32(1, LLContext);
			break;
		case TypeKind::Int64:
			LLOne = GetLLInt64(1, LLContext);
			break;
		case TypeKind::UnsignedInt64:
			LLOne = GetLLUInt64(1, LLContext);
			break;
		case TypeKind::Int:
			LLOne = GetSystemInt(1, LLContext, LLModule);
			break;
		case TypeKind::UnsignedInt:
			LLOne = GetSystemUInt(1, LLContext, LLModule);
			break;
		default: assert(!"unimplementd!"); break;
		}
		return LLOne;
	};
	
	switch (UniOp->Op) {
	case TokenKind::PLUS_PLUS: case TokenKind::POST_PLUS_PLUS: {
		llvm::Value* LLVal  = GenNode(UniOp->Value);
		llvm::Value* LLRVal = CreateLoad(LLVal);
		llvm::Value* IncRes;
		if (UniOp->Ty->IsPointer()) {
			// Pointer arithemtic
			IncRes = CreateInBoundsGEP(LLRVal, { GetLLInt64(1,  LLContext) });
		} else {
			IncRes = Builder.CreateAdd(LLRVal, GetOneValue(UniOp->Value->Ty, LLContext, LLModule), "inc");
		}
		Builder.CreateStore(IncRes, LLVal);
		return UniOp->Op == TokenKind::PLUS_PLUS ? IncRes : LLRVal;
	}
	case TokenKind::MINUS_MINUS: case TokenKind::POST_MINUS_MINUS: {
		llvm::Value* LLVal  = GenNode(UniOp->Value);
		llvm::Value* LLRVal = CreateLoad(LLVal);
		llvm::Value* IncRes;
		if (UniOp->Ty->IsPointer()) {
			// Pointer arithemtic
			IncRes = CreateInBoundsGEP(LLRVal, { GetLLInt64(-1,  LLContext) });
		} else {
			IncRes = Builder.CreateSub(LLRVal, GetOneValue(UniOp->Value->Ty, LLContext, LLModule), "inc");
		}
		Builder.CreateStore(IncRes, LLVal);
		return UniOp->Op == TokenKind::MINUS_MINUS ? IncRes : LLRVal;
	}
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
	case '*': {
		llvm::Value* LLValue = GenNode(UniOp->Value);
		// Need to verify that the the value is actually an l-value.
		//
		// It is possible that it is not in cases in which its a pointer
		// as a result of some operation. Examples:
		// 
		// Example 1:  '*(a + 4)'    (where 'a' is a pointer)
		// Example 2:  'int a = *f()'
		// 
		// TODO: There is probably a cleaner way of doing this.
		//       Some type of IsLValue() function?
		if (UniOp->Value->Is(AstKind::IDENT_REF) || UniOp->Value->Is(AstKind::FIELD_ACCESSOR) ||
			UniOp->Value->Is(AstKind::ARRAY_ACCESS) ||
			(UniOp->Value->Is(AstKind::UNARY_OP) || static_cast<UnaryOp*>(UniOp->Value)->Op == '*')) {
			LLValue = CreateLoad(LLValue);
		}
		return LLValue;
	}
	default:
		assert(!"Failed to implement GenUnaryOp() case!");
		return nullptr;
	}
}

llvm::Value* arco::IRGenerator::GenNumberLiteral(NumberLiteral* Number) {
	switch (Number->Ty->GetKind()) {
	case TypeKind::Char:
	case TypeKind::Int8:           return GetLLInt8(Number->SignedIntValue, LLContext);
	case TypeKind::Int16:          return GetLLInt16(Number->SignedIntValue, LLContext);
	case TypeKind::Int32:          return GetLLInt32(Number->SignedIntValue, LLContext);
	case TypeKind::Int64:          return GetLLInt64(Number->SignedIntValue, LLContext);
	case TypeKind::UnsignedInt8:   return GetLLUInt8(Number->UnsignedIntValue, LLContext);
	case TypeKind::UnsignedInt16:  return GetLLUInt16(Number->UnsignedIntValue, LLContext);
	case TypeKind::UnsignedInt32:  return GetLLUInt32(Number->UnsignedIntValue, LLContext);
	case TypeKind::UnsignedInt64:  return GetLLUInt64(Number->UnsignedIntValue, LLContext);
	case TypeKind::Int:
		return GetSystemInt(Number->SignedIntValue, LLContext, LLModule);
	case TypeKind::UnsignedInt:
		return GetSystemUInt(Number->UnsignedIntValue, LLContext, LLModule);
	default:
		assert(!"Unimplemented GenNumberLiteral() case");
		return nullptr;
	}
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
	VarDecl* Var = IRef->Var;
	if (Var->IsGlobal) {
		GenGlobalVarDecl(Var);
	}
	
	if (Var->IsField()) {
		return CreateStructGEP(LLThis, Var->FieldIdx);
	} else {
		return Var->LLAddress;
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

	Expr* Site = FieldAcc->Site;
	llvm::Value* LLSite;
	if (Site->Is(AstKind::FUNC_CALL) && Site->Ty->GetKind() == TypeKind::Struct) {
		FuncCall* Call = static_cast<FuncCall*>(Site);
		LLSite = CreateUnseenAlloca(GenType(Call->Ty), "tmp.obj");
		GenStoreStructRetFromCall(Call, LLSite);
	} else {
		LLSite = GenNode(Site);
	}

	// Automatically dereference pointers!
	if (Site->Ty->GetKind() == TypeKind::Pointer &&
		Site->IsNot(AstKind::THIS_REF) /* Reference is already loaded. */) {
		LLSite = CreateLoad(LLSite);
		LLSite->setName("ptr.deref");
	}

	if (FieldAcc->RefKind == IdentRef::RK::Funcs) {
		// Calling a member function. Ex.  'a.b()'
		return LLSite;
	} else {
		return CreateStructGEP(LLSite, FieldAcc->Var->FieldIdx);
	}
}

llvm::Value* arco::IRGenerator::GenFuncCall(FuncCall* Call, llvm::Value* LLAddr) {
	return GenFuncCallGeneral(
		Call,
		Call->CalledFunc,
		Call->Args,
		LLAddr
	);
}

llvm::Value* arco::IRGenerator::GenFuncCallGeneral(Expr* CallNode,
	                                               FuncDecl* CalledFunc,
	                                               llvm::SmallVector<NonNamedValue, 2>& Args,
	                                               llvm::Value* LLAddr) {
	GenFuncDecl(CalledFunc);
	
	if (CalledFunc->LLVMIntrinsicID) {
		return GenLLVMIntrinsicCall(CalledFunc, Args);
	}

	ulen NumArgs = Args.size();
	if (CalledFunc->Struct) {
		++NumArgs;
	}
	if (CalledFunc->UsesParamRetSlot) {
		++NumArgs;
	}

	llvm::SmallVector<llvm::Value*, 2> LLArgs;
	LLArgs.resize(NumArgs);

	ulen ArgIdx = 0;
	if (CalledFunc->Struct) {
		if (CalledFunc->IsConstructor) {
			LLArgs[ArgIdx++] = LLAddr;
		} else if (LLThis && CFunc && CalledFunc->Struct == CFunc->Struct) {
			// Calling one member function from another.
			LLArgs[ArgIdx++] = LLThis;
		} else {
			// Calling a member function from a variable so
			// that the variable's address gets passed in.
			FuncCall* Call = static_cast<FuncCall*>(CallNode);
			LLArgs[ArgIdx++] = GenNode(Call->Site);
		}
	}
	if (CalledFunc->UsesParamRetSlot) {
		if (LLAddr) {
			LLArgs[ArgIdx++] = LLAddr;
		} else {
			// Strange, but the user has decided to ignore
			// the return value so a temporary object needs
			// to be created.
			LLAddr = CreateUnseenAlloca(GenType(CallNode->Ty), "ignored.ret");
			LLArgs[ArgIdx++] = LLAddr;
		}
	}

	for (ulen i = 0; i < Args.size(); i++) {
		Expr* Arg = Args[i].E;
		llvm::Value* LLArg = nullptr;
		if (Arg->Is(AstKind::FUNC_CALL) && Arg->Ty->GetKind() == TypeKind::Struct) {
			LLArg = CreateUnseenAlloca(GenType(Arg->Ty), "arg.tmp");
			GenStoreStructRetFromCall(static_cast<FuncCall*>(Arg), LLArg);

			// TODO: If there ends up being further optimizations such that parameters
			// take into account similar constraints to return values where structs
			// get passed as integers/pointers depending on memory size then this will
			// not need to be loaded.
			LLArg = CreateLoad(LLArg);

		} else {
			LLArg = GenRValue(Arg);
			
			if (Arg->Ty->GetKind() == TypeKind::Array) {
				// Arrays are passed as pointers. Cannot simply decay though
				// because the argument might be an already decayed array.
				LLArg = ArrayToPointer(LLArg);
			}
		}
		LLArgs[ArgIdx++] = LLArg;
	}

	llvm::Function* LLCalledFunc = CalledFunc->LLFunction;

	// -- DEBUG
	//llvm::outs() << "Calling function with name: " << CalledFunc->Name << "\n";
	//llvm::outs() << "Types passed to function:\n";
	//for (ulen i = 0; i < LLArgs.size(); i++) {
	//	llvm::outs() << LLValTypePrinter(LLArgs[i]) << "\n";
	//}
	//llvm::outs() << "Types expected by function:\n";
	//for (ulen i = 0; i < LLCalledFunc->arg_size(); i++) {
	//	llvm::outs() << LLValTypePrinter(LLCalledFunc->getArg(i)) << "\n";
	//}
	//llvm::outs() << "\n";

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
		LLAddr = CreateUnseenAlloca(GenType(DestTy), "tmp.array");
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
		LLAddr = CreateUnseenAlloca(GenStructType(StructTy), "tmp.structinit");
	}

	if (StructInit->CalledConstructor) {
		return GenFuncCallGeneral(
			StructInit,
			StructInit->CalledConstructor,
			StructInit->Args,
			LLAddr
		);
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
				GenDefaultValue(Field->Ty, LLFieldAddr);
			}
		}
	}

	return LLAddr;
}

llvm::Value* arco::IRGenerator::GenHeapAlloc(HeapAlloc* Alloc) {
	Type* TypeToAlloc = Alloc->TypeToAlloc;
	if (TypeToAlloc->GetKind() == TypeKind::Array) {
		ArrayType* ArrayTy = static_cast<ArrayType*>(TypeToAlloc);

		// Calculating how much memory needs to be allocated for the array.
		llvm::Value* LLTotalLinearLength = GenRValue(ArrayTy->GetLengthExpr());
		while (ArrayTy->GetElementType()->GetKind() == TypeKind::Array) {
			ArrayTy = static_cast<ArrayType*>(ArrayTy->GetElementType());
			LLTotalLinearLength = 
				Builder.CreateMul(LLTotalLinearLength, GenRValue(ArrayTy->GetLengthExpr()));
		}

		Type* BaseTy = ArrayTy->GetElementType();
		llvm::Value* LLArrStartPtr = GenMalloc(GenType(BaseTy), LLTotalLinearLength);

		if (BaseTy->GetKind() == TypeKind::Struct) {
			// Need to initialize fields so calling the default constructor.
			StructArrayCallDefaultConstructors(BaseTy, LLArrStartPtr, LLTotalLinearLength);
		}

		return LLArrStartPtr;
	} else {
		llvm::Value* LLMalloc = GenMalloc(GenType(TypeToAlloc), nullptr);
		if (TypeToAlloc->GetKind() == TypeKind::Struct) {
			// Need to initialize fields so calling the default constructor.
			CallDefaultConstructor(LLMalloc, static_cast<StructType*>(TypeToAlloc));
		}
		return LLMalloc;
	}
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
			} else if (ToSize > FromSize) {
				if (ToType->IsSigned()) {
					// Signed upcasting
					return Builder.CreateSExt(LLValue, LLCastType);
				} else {
					// Unsigned upcasting
					return Builder.CreateZExt(LLValue, LLCastType);
				}
			} else {
				// They are actually the same type from LLVM's POV just different
				// types in the language.
				return LLValue;
			}
		}
		goto missingCaseLab;
	case TypeKind::Pointer: {
		//  --- TO Pointers ---
		if (FromType->GetKind() == TypeKind::Null) {
			return LLValue; // Already handled during generation
		} else if (FromType->GetKind() == TypeKind::Array) {
			llvm::Value* LLPtrValue = ArrayToPointer(LLValue);
			if (ToType->Equals(Context.VoidPtrType)) {
				LLPtrValue = Builder.CreateBitCast(LLPtrValue,
					                               llvm::Type::getInt8PtrTy(LLContext));
			}
			return LLPtrValue;
		} else if (FromType->IsPointer()) {
			// Pointer to Pointer
			return Builder.CreateBitCast(LLValue, LLCastType);
		} else if (FromType->IsInt()) {
			// Int to Ptr
			return Builder.CreateIntToPtr(LLValue, LLCastType);
		}
		goto missingCaseLab;
	}
	case TypeKind::CStr: {
		if (FromType->GetKind() == TypeKind::Null) {
			return LLValue; // Already handled during generation
		} else if (FromType->GetKind() == TypeKind::Array) {
			return ArrayToPointer(LLValue);
		} else if (FromType->IsPointer()) {
			// Pointer to Pointer
			return Builder.CreateBitCast(LLValue, LLCastType);
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
	case TypeKind::Struct:
		return llvm::ConstantAggregateZero::get(GenType(Ty));
	default:
		assert(!"Failed to implement GenZeroedValue() case!");
		return nullptr;
	}
}

llvm::Value* arco::IRGenerator::GenMalloc(llvm::Type* LLType, llvm::Value* LLArrayLength) {
	
	llvm::Value* LLMalloc = llvm::CallInst::CreateMalloc(
		Builder.GetInsertBlock(),                                      // llvm::BasicBlock *InsertAtEnd
		llvm::Type::getInt64Ty(LLContext),                             // llvm::Type* IntPtrTy
		LLType,                                                        // llvm::Type* AllocTy
		GetSystemUInt(SizeOfTypeInBytes(LLType), LLContext, LLModule), // llvm::Value* AllocSize
		LLArrayLength,
		nullptr,
		""
	);
	Builder.Insert(LLMalloc);
	
	return LLMalloc;
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

llvm::Value* arco::IRGenerator::ArrayToPointer(llvm::Value* LLArray) {

	llvm::Type* LLType = LLArray->getType();
	
	// case 1: [n x BaseType]*    Happens when LLArray is the address to an array.
	// case 2: [n x BaseType]     Could happen if the array is a constantly global array.
	if ((LLType->isPointerTy() && LLType->getPointerElementType()->isArrayTy()) ||
		LLType->isArrayTy()) {
		return DecayArray(LLArray);
	}

	// The array may be represented as:   BaseType**
	// This can happen because when arrays are passed to functions they are decayed
	// and the pointer to that array is stored in a local variable. So LLArray would
	// be the address of the variable storing the pointer to the array.
	if (LLType->isPointerTy() && LLType->getPointerElementType()->isPointerTy()) {
		return CreateLoad(LLArray);
	}

	// Already a pointer!
	return LLArray;
}

llvm::Value* arco::IRGenerator::MultiDimensionalArrayToPointerOnly(llvm::Value* LLArray, ArrayType* ArrTy) {
	// TODO: Won't this be wrong in instances in which the array is already partly decayed?
	// I think this needs to rely explicitly on the LLVM information.
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

llvm::Value* arco::IRGenerator::GenReturnValueForOptimizedStructAsInt(llvm::Value* LLRetVal) {
	if (LLRetVal->getType()->isPointerTy()) {
		// Bitcast the struct type's address value to a integer pointer.
		llvm::Type* LLRetTy = CFunc->LLFunction->getReturnType();
		llvm::Value* LLDestVal = Builder.CreateBitCast(LLRetVal, llvm::PointerType::get(LLRetTy, 0));
		return CreateLoad(LLDestVal);
	} else {
		// Ex.   return func();  // where function returns an optimized integer ret. value
		return LLRetVal;
	}
}

void arco::IRGenerator::GenReturnByStoreToElisionRetSlot(Expr* Value) {
	if (Value->Is(AstKind::STRUCT_INITIALIZER)) {
		// Ex.  'return StructName{ 43, 22 };'
		GenStructInitializer(static_cast<StructInitializer*>(Value),
							 GetElisionRetSlotAddr(CFunc));
	} else if (Value->Is(AstKind::FUNC_CALL)) {
		// Ex.  'fn foo() StructName { return bar(); }
		GenFuncCall(static_cast<FuncCall*>(Value),
			        GetElisionRetSlotAddr(CFunc));
	} // else the remaining case should be that Value is AstKind::IDENT_REF
	// in which the identifier reference should already point to the elision
	// return address.
	//
	// Ex.
	//     'func() A {
	//         a A = A:{ 44, 22 };
	//         a.g = 33;
	//         return a;
	//     }'
	//
	// Where sizeof(A) >= size of architecture's pointer size.
}

void arco::IRGenerator::CopyStructObject(llvm::Value* LLToAddr, llvm::Value* LLFromAddr, StructDecl* Struct) {
	// TODO: In the future if their is a copy constructor call that instead!
	llvm::StructType* LLStructType =  llvm::cast<llvm::StructType>(LLFromAddr->getType()->getPointerElementType());
	const llvm::StructLayout* LLStructLayout = LLModule.getDataLayout().getStructLayout(LLStructType);
	llvm::Align LLAlignment = LLStructLayout->getAlignment();
	Builder.CreateMemCpy(
		LLToAddr, LLAlignment,
		LLFromAddr, LLAlignment,
		SizeOfTypeInBytes(LLStructType)
	);
}

void arco::IRGenerator::GenConstructorBodyFieldAssignments(StructDecl* Struct) {
	for (VarDecl* Field : Struct->Fields) {
		llvm::Value* LLFieldAddr = CreateStructGEP(LLThis, Field->FieldIdx);
		if (Field->Assignment) {
			GenAssignment(LLFieldAddr, Field->Assignment);
		} else {
			GenDefaultValue(Field->Ty, LLFieldAddr);
		}
	}
}

std::tuple<bool, llvm::Constant*> arco::IRGenerator::GenGlobalVarInitializeValue(VarDecl* Global) {
	Type* Ty = Global->Ty;
	Expr* Assignment = Global->Assignment;
	if (Assignment) {
		if (Assignment->IsFoldable) {
			return { true, llvm::cast<llvm::Constant>(GenRValue(Assignment)) };
		} else {
			return { false, GenZeroedValue(Ty) };
		}
	} else {
		// Assignment == nullptr

		if (Ty->GetKind() == TypeKind::Struct ||
			(Ty->GetKind() == TypeKind::Array &&
			 static_cast<ArrayType*>(Ty)->GetBaseType()->GetKind() == TypeKind::Struct)) {
			// TODO: If foldable could get away with not calling the default constructor.
			// Need to call the default constructor.
			return { false, GenZeroedValue(Ty) };
		} else {
			return { true, GenZeroedValue(Ty) };
		}
	}
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
	} else if (Value->Is(AstKind::FUNC_CALL)) {
		FuncCall* Call = static_cast<FuncCall*>(Value);
		if (Call->Ty->GetKind() == TypeKind::Struct) {
			GenStoreStructRetFromCall(Call, LLAddress);
		} else {
			llvm::Value* LLAssignment = GenRValue(Value);
			Builder.CreateStore(LLAssignment, LLAddress);
		}
	} else {
		llvm::Value* LLAssignment = GenRValue(Value);
		// -- DEBUG
		// llvm::outs() << "Address Type: " << LLValTypePrinter(LLAddress) << " Assignment Type: " << LLValTypePrinter(LLAssignment);
		Builder.CreateStore(LLAssignment, LLAddress);
	}
}

void arco::IRGenerator::GenDefaultValue(Type* Ty, llvm::Value* LLAddr) {
	if (Ty->GetKind() == TypeKind::Struct) {
		StructType* StructTy = static_cast<StructType*>(Ty);
		StructDecl* Struct = StructTy->GetStruct();

		if (Struct->FieldsHaveAssignment || Struct->DefaultConstructor) {
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
			if (Struct->FieldsHaveAssignment || Struct->DefaultConstructor) {
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

	if (Struct->DefaultConstructor) {
		GenFuncDecl(Struct->DefaultConstructor);
		Struct->LLDefaultConstructor = Struct->DefaultConstructor->LLFunction;
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

llvm::Value* arco::IRGenerator::GetElisionRetSlotAddr(FuncDecl* Func) {
	return Func->Struct ? Func->LLFunction->getArg(1) : Func->LLFunction->getArg(0);
}

void arco::IRGenerator::GenStoreStructRetFromCall(FuncCall* Call, llvm::Value* LLAddr) {
	llvm::StructType* LLStructTy = GenStructType(static_cast<StructType*>(Call->Ty));

	ulen StructByteSize = SizeOfTypeInBytes(LLStructTy);
	if (StructByteSize <= LLModule.getDataLayout().getPointerSize()) {
		// The return type is small enough to be shoved into an
		// integer so that returned value needs to be reinterpreted
		// as an integer to store the result.
		llvm::Value* LLRetVal = GenFuncCall(Call, nullptr);
		LLAddr = Builder.CreateBitCast(LLAddr, llvm::PointerType::get(LLRetVal->getType(), 0));
		Builder.CreateStore(LLRetVal, LLAddr);
	} else {
		// Needs to be passed as a parameter.
		GenFuncCall(Call, LLAddr);
	}
}

// For reference:
// https://github.com/llvm/llvm-project/blob/main/clang/lib/CodeGen/CGBuiltin.cpp
// https://github.com/google/swiftshader/blob/master/src/Reactor/LLVMReactor.cpp
llvm::Value* arco::IRGenerator::GenLLVMIntrinsicCall(FuncDecl* CalledFunc,
			                                         const llvm::SmallVector<NonNamedValue, 2>& Args) {
	
	switch (CalledFunc->LLVMIntrinsicID) {
	case llvm::Intrinsic::memcpy: {
		llvm::Align LLAlignment = llvm::Align();
		return Builder.CreateMemCpy(
			GenRValue(Args[0].E), LLAlignment,
			GenRValue(Args[1].E), LLAlignment,
			GenRValue(Args[2].E)
		);
	}
	case llvm::Intrinsic::memset: {
		llvm::Align LLAlignment = llvm::Align();
		return Builder.CreateMemSet(
			GenRValue(Args[0].E),
			GenRValue(Args[1].E),
			GenRValue(Args[2].E),
			LLAlignment
		);
	}
	}
	return nullptr;
}
