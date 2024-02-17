#include "EmitDebugInfo.h"

#include "Context.h"
#include "IRGen.h"

arco::DebugInfoEmitter::~DebugInfoEmitter() {
    delete DBuilder;
}

arco::DebugInfoEmitter::DebugInfoEmitter(ArcoContext& Context)
    : DBuilder(new llvm::DIBuilder(Context.LLArcoModule)),
      Context(Context)
{
}

void arco::DebugInfoEmitter::EmitFile(FileScope* FScope) {

    std::string& FullPath = FScope->FullPath;
    auto Itr = FullPath.find_last_of("/");
    std::string FileName  = Itr == std::string::npos ? "" : FullPath.substr(Itr + 1);
    std::string Directory = FullPath.substr(0, FullPath.size() - FileName.size());
    if (!Directory.empty()) {
        // Remove /
        Directory = Directory.substr(0, Directory.size() - 1);
    }

    // TODO: Checksums can be used as a way to verify that the source code
	//       is the same as the executable's debug info for that source code.
	//       Also, this should be able to be disabled as checksums can slow down
	//       compilation time.
    llvm::Optional<llvm::DIFile::ChecksumInfo<llvm::StringRef>> ChecksumInfo;
	llvm::SmallString<64> Checksum;
	llvm::Optional<llvm::DIFile::ChecksumKind> ChecksumKind;
	if (ChecksumKind)
		ChecksumInfo.emplace(*ChecksumKind, Checksum);

    llvm::DIFile* DIUnitFile = DBuilder->createFile(FileName, Directory, ChecksumInfo);

    llvm::StringRef DebugFlags = "";

    DebugUnit = DBuilder->createCompileUnit(
		llvm::dwarf::DW_LANG_C99,
		DIUnitFile,
		"Arco Compiler", // Producer
		false,           // TODO: Is Optimized
		DebugFlags,
		0,
		llvm::StringRef(),
		llvm::DICompileUnit::DebugEmissionKind::FullDebug,
		0,
		false,
		false,
		llvm::DICompileUnit::DebugNameTableKind::None
	);
}

void arco::DebugInfoEmitter::EmitFunc(FuncDecl* Func) {
	llvm::DIScope* Scope = DebugUnit->getFile();

	llvm::SmallVector<llvm::Metadata*, 4> DIFuncTys;
	llvm::Metadata* DIRetTy = EmitType(Func->RetTy);
	
	DIFuncTys.push_back(DIRetTy);
	for (VarDecl* Param : Func->Params) {
		DIFuncTys.push_back(EmitType(Param->Ty));
	}

	llvm::DISubroutineType* DIFuncTy =
		DBuilder->createSubroutineType(DBuilder->getOrCreateTypeArray(DIFuncTys));

	llvm::DISubprogram* DIFunc = DBuilder->createFunction(
		Scope,
		Func->Name.Text,
		Func->LLFunction->getName(), // Linkage name
		DebugUnit->getFile(),
		Func->Loc.LineNumber,
		DIFuncTy,
		Func->Loc.LineNumber, // TODO: scope line
		llvm::DINode::DIFlags::FlagPrototyped,
		llvm::DISubprogram::DISPFlags::SPFlagDefinition
	);
	Func->LLFunction->setSubprogram(DIFunc);
	
	DILexicalScopes.push_back(DIFunc);
}

void arco::DebugInfoEmitter::EmitFuncEnd(FuncDecl* Func) {
	DILexicalScopes.clear();
	DBuilder->finalizeSubprogram(Func->LLFunction->getSubprogram());
}

void arco::DebugInfoEmitter::EmitLocalVar(VarDecl* Var, llvm::IRBuilder<>& IRBuilder) {
	llvm::DIScope* DIScope = DILexicalScopes.back();

	llvm::DILocalVariable* DIVariable = DBuilder->createAutoVariable(
		DIScope,
		Var->Name.Text,
		DebugUnit->getFile(),
		Var->Loc.LineNumber,
		EmitType(Var->Ty),
		true
	);

	DBuilder->insertDeclare(
		Var->LLAddress,
		DIVariable,
		DBuilder->createExpression(),
		llvm::DILocation::get(
			Context.LLContext,
			Var->Loc.LineNumber,
			0, // TODO: Colum,
			DIScope),
		IRBuilder.GetInsertBlock()
	);
}

void arco::DebugInfoEmitter::EmitScopeStart(SourceLoc Loc) {
	llvm::DILexicalBlock* DILexBlock = DBuilder->createLexicalBlock(
		DILexicalScopes.back(),
		DebugUnit->getFile(),
		Loc.LineNumber,
		0   // TODO: column number.
	);
	DILexicalScopes.push_back(DILexBlock);
}

void arco::DebugInfoEmitter::EmitScopeEnd() {
	DILexicalScopes.pop_back();
}

void arco::DebugInfoEmitter::EmitDebugLocation(SourceLoc Loc, llvm::IRBuilder<>& IRBuilder) {
	llvm::Instruction* LLLastInst = &IRBuilder.GetInsertBlock()->back();
	EmitDebugLocation(LLLastInst, Loc);
}

void arco::DebugInfoEmitter::EmitDebugLocation(llvm::Instruction* LLInst, SourceLoc Loc) {
	llvm::DIScope* DIScope = DILexicalScopes.back();
	LLInst->setDebugLoc(
		llvm::DILocation::get(
			Context.LLContext,
			Loc.LineNumber,
			0, // TODO: column number
			DIScope)
	);
}

void arco::DebugInfoEmitter::Finalize() {
	DBuilder->finalize();
}

llvm::DIType* arco::DebugInfoEmitter::EmitType(Type* Ty) {
	auto Itr = Context.LLDITypeCache.find(Ty->GetUniqueId());
	if (Itr != Context.LLDITypeCache.end()) {
		return Itr->second;
	}
	llvm::DIType* LLDITy = EmitFirstSeenType(Ty);
	Context.LLDITypeCache.insert({ Ty->GetUniqueId(), LLDITy });
	return LLDITy;
}

llvm::DIType* arco::DebugInfoEmitter::EmitFirstSeenType(Type* Ty) {
	switch (Ty->GetKind()) {
	case TypeKind::Void:   return nullptr; // Yes this is correct it expects nullptr.
	case TypeKind::Int: {
		ulen PtrSizeInBits = Context.LLArcoModule
			                         .getDataLayout()
			                         .getPointerSizeInBits();
		return DBuilder->createBasicType("int", PtrSizeInBits, llvm::dwarf::DW_ATE_signed);
	}
	case TypeKind::UInt: {
		ulen PtrSizeInBits = Context.LLArcoModule
			                         .getDataLayout()
			                         .getPointerSizeInBits();
		return DBuilder->createBasicType("int", PtrSizeInBits, llvm::dwarf::DW_ATE_unsigned);
	}
	case TypeKind::Int8:    return DBuilder->createBasicType("int8"  , 8 , llvm::dwarf::DW_ATE_signed);
	case TypeKind::Int16:   return DBuilder->createBasicType("int16" , 16, llvm::dwarf::DW_ATE_signed);
	case TypeKind::Int32:   return DBuilder->createBasicType("int32" , 32, llvm::dwarf::DW_ATE_signed);
	case TypeKind::Int64:   return DBuilder->createBasicType("int64" , 64, llvm::dwarf::DW_ATE_signed);
	case TypeKind::UInt8:   return DBuilder->createBasicType("uint8" , 8 , llvm::dwarf::DW_ATE_unsigned);
	case TypeKind::UInt16:  return DBuilder->createBasicType("uint16", 16, llvm::dwarf::DW_ATE_unsigned);
	case TypeKind::UInt32:  return DBuilder->createBasicType("uint32", 32, llvm::dwarf::DW_ATE_unsigned);
	case TypeKind::UInt64:  return DBuilder->createBasicType("uint64", 64, llvm::dwarf::DW_ATE_unsigned);
	case TypeKind::Char:    return DBuilder->createBasicType("char"  , 8,  llvm::dwarf::DW_ATE_signed_char);
	case TypeKind::Bool:    return DBuilder->createBasicType("bool"  , 8, llvm::dwarf::DW_ATE_boolean);
	case TypeKind::Float32: return DBuilder->createBasicType("f32"   , 32, llvm::dwarf::DW_ATE_float);
	case TypeKind::Float64: return DBuilder->createBasicType("f64"   , 32, llvm::dwarf::DW_ATE_float);
	case TypeKind::Pointer:
	case TypeKind::CStr: {
		ulen PtrSizeInBits = Context.LLArcoModule
			                        .getDataLayout()
			                        .getPointerSizeInBits();
		return DBuilder->createPointerType(
			EmitType(Ty->GetPointerElementType(Context)), PtrSizeInBits);
	}
	case TypeKind::Slice: {
		SliceType* SliceTy = Ty->AsSliceTy();
		llvm::StructType* LLSliceTy = llvm::cast<llvm::StructType>(GenType(Context, SliceTy));

		const llvm::StructLayout* LLLayout = Context.LLArcoModule
			                                        .getDataLayout()
			                                        .getStructLayout(LLSliceTy);

		u64 SizeInBits = LLLayout->getSizeInBits();

		llvm::DINode::DIFlags Flags = llvm::DINode::FlagZero;
		llvm::DICompositeType* DIStructTy = DBuilder->createStructType(
			nullptr,
			LLSliceTy->getName(),
			DebugUnit->getFile(),
			0,
			SizeInBits,
			0,
			Flags,
			nullptr,
			llvm::DINodeArray(),
			Context.LLArcoModule.getDataLayout().getPrefTypeAlignment(LLSliceTy) * 8, // *8 because wants bits
			nullptr,
			LLSliceTy->getName()
		);

		llvm::SmallVector<llvm::Metadata*> DIFieldTys;

		llvm::Type* LLLengthType = GenType(Context, Context.IntType);
		SizeInBits = Context.LLArcoModule
							.getDataLayout()
							.getTypeSizeInBits(LLLengthType);
		llvm::DIType* DIMemberLengthType = DBuilder->createMemberType(
			DIStructTy,
			"length",
			DebugUnit->getFile(),
			0,
			SizeInBits,
			Context.LLArcoModule.getDataLayout().getPrefTypeAlignment(LLLengthType) * 8, // *8 because wants bits
			0,
			llvm::DINode::DIFlags::FlagZero,
			EmitType(Context.IntType)
		);
		DIFieldTys.push_back(DIMemberLengthType);

		PointerType* ArrPtrType = PointerType::Create(SliceTy->GetElementType(), Context);
		llvm::Type* LLArrPtrType = GenType(Context, ArrPtrType);
		SizeInBits = Context.LLArcoModule
							.getDataLayout()
							.getTypeSizeInBits(LLArrPtrType);
		llvm::DIType* DIMemberArrPtrType = DBuilder->createMemberType(
			DIStructTy,
			"arrptr",
			DebugUnit->getFile(),
			0,
			SizeInBits,
			Context.LLArcoModule.getDataLayout().getPrefTypeAlignment(LLArrPtrType) * 8, // *8 because wants bits
			LLLayout->getElementOffsetInBits(1),
			llvm::DINode::DIFlags::FlagZero,
			EmitType(ArrPtrType)
		);
		DIFieldTys.push_back(DIMemberArrPtrType);

		DBuilder->replaceArrays(DIStructTy, DBuilder->getOrCreateArray(DIFieldTys));
		return DIStructTy;
	}
	case TypeKind::Array: {
		ArrayType* ArrayTy = Ty->AsArrayTy();
		llvm::DIType* DIBaseTy = EmitType(ArrayTy->GetBaseType());

		ulen PtrSizeInBits = Context.LLArcoModule
			                        .getDataLayout()
			                        .getPointerSizeInBits();

		ArrayType* ArrayTyPtr = ArrayTy;
		llvm::SmallVector<llvm::Metadata*> DISubscriptSizes;
		bool MoreSubscripts = true;
		do {
			auto DISubscriptLengthValue =
				llvm::ConstantAsMetadata::get(llvm::ConstantInt::get(
					llvm::Type::getIntNTy(Context.LLContext, PtrSizeInBits),
					ArrayTy->GetLength()
				));

			DISubscriptSizes.push_back(DBuilder->getOrCreateSubrange(0, DISubscriptLengthValue));
			MoreSubscripts = ArrayTy->GetElementType()->GetKind() == TypeKind::Array;
			if (MoreSubscripts) {
				ArrayTy = ArrayTy->GetElementType()->AsArrayTy();
			}
		} while (MoreSubscripts);

		return DBuilder->createArrayType(
			ArrayTy->GetTotalLinearLength() * Context.LLArcoModule
			                                         .getDataLayout()
			                                         .getTypeSizeInBits(
														  GenType(Context, ArrayTy->GetBaseType())
													 ),
			0, // TODO: Alignment
			DIBaseTy,
			DBuilder->getOrCreateArray(DISubscriptSizes));
	}
	case TypeKind::Function: {
		llvm::SmallVector<llvm::Metadata*, 4> DIFuncTys;
		FunctionType* FuncTy = Ty->AsFunctionType();

		llvm::Metadata* DIRetTy = EmitType(FuncTy->RetTyInfo.Ty);
		DIFuncTys.push_back(DIRetTy);
		for (TypeInfo ParamInfo : FuncTy->ParamTypes) {
			DIFuncTys.push_back(EmitType(ParamInfo.Ty));
		}

		ulen PtrSizeInBits = Context.LLArcoModule
			                        .getDataLayout()
			                        .getPointerSizeInBits();
		llvm::DIType* DIFuncTy = DBuilder->createSubroutineType(
			                           DBuilder->getOrCreateTypeArray(DIFuncTys));
		return DBuilder->createPointerType(DIFuncTy, PtrSizeInBits);
	}
	case TypeKind::Struct: {
		StructDecl* Struct = Ty->AsStructType()->GetStruct();
		
		const llvm::StructLayout* LLLayout = Context.LLArcoModule
			                                        .getDataLayout()
			                                        .getStructLayout(Struct->LLStructTy);

		u64 SizeInBits = LLLayout->getSizeInBits();

		llvm::DINode::DIFlags Flags = llvm::DINode::FlagZero;
		llvm::DICompositeType* DIStructTy = DBuilder->createStructType(
			nullptr,
			Struct->Name.Text,
			DebugUnit->getFile(),
			Struct->Loc.LineNumber,
			SizeInBits,
			0,
			Flags,
			nullptr, // TODO: Derived from?
			llvm::DINodeArray(),
			Context.LLArcoModule.getDataLayout().getPrefTypeAlignment(Struct->LLStructTy) * 8, // *8 because wants bits
			nullptr,
			Struct->LLStructTy->getName()
		);

		llvm::SmallVector<llvm::Metadata*, 16> DIFieldTys;
		u64 BitsOffset = 0;
		for (VarDecl* Field : Struct->Fields) {
			u64 OffsetInBits = LLLayout->getElementOffsetInBits(Field->FieldIdx);
			DIFieldTys.push_back(EmitMemberFieldType(DIStructTy, Field, OffsetInBits));
		}
		DBuilder->replaceArrays(DIStructTy, DBuilder->getOrCreateArray(DIFieldTys));

		return DIStructTy;
	}
	case TypeKind::Enum: {
		// TODO: Is there a way to also carray the name information around?
		EnumDecl* Enum = Ty->AsStructType()->GetEnum();
		Type* IndexType = Enum->IndexingInOrder ? Enum->ValuesType : Context.IntType;
		return EmitType(IndexType);
	}
	default:
		assert(!"Unimplement EmitFirstSeenType() for DI type");
		return nullptr;
	}
}

llvm::DIType* arco::DebugInfoEmitter::EmitMemberFieldType(llvm::DIType* DIScope, VarDecl* Field, u64 BitsOffset) {
	
	llvm::Type* LLType = GenType(Context, Field->Ty);

	u64 SizeInBits = Context.LLArcoModule
			                 .getDataLayout()
			                 .getTypeSizeInBits(LLType);
	

	llvm::DIType* DIMemberType = DBuilder->createMemberType(
		DIScope,
		Field->Name.Text,
		DebugUnit->getFile(),
		Field->Loc.LineNumber,
		SizeInBits,
		Context.LLArcoModule.getDataLayout().getPrefTypeAlignment(LLType) * 8, // *8 because wants bits
		BitsOffset,
		llvm::DINode::DIFlags::FlagZero,
		EmitType(Field->Ty)
	);
	
	return DIMemberType;
}
