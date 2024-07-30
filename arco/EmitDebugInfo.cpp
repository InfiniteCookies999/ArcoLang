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
	if (DebugUnit) { // Do not generate more than once
		return;
	}

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

	// TODO: Note: DW_LANG_C_plus_plus_14 allows for some stuff related to virtual functions like
	// "this adjustments" which we will likely need. Therefore, we will probably need to either use
	// DW_LANG_C_plus_plus_14 at some point or switch to having a custom language definition.
	DebugUnit = DBuilder->createCompileUnit(
		llvm::dwarf::DW_LANG_C99,
		//llvm::dwarf::DW_LANG_C_plus_plus_14,
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

llvm::DISubprogram* arco::DebugInfoEmitter::EmitFunc(FuncDecl* Func, bool ForwardDecl) {

	// Ensure the compilation unit exists for this function.
	EmitFile(Func->FScope);

	llvm::SmallVector<llvm::Metadata*, 4> DIFuncTys;
	// Return type always comes first.
	llvm::Metadata* DIRetTy = EmitType(Func->RetTy);
	DIFuncTys.push_back(DIRetTy);

	// If the struct is a member function then the debugger needs to know about the
	// "this" pointer. NOTE: This should be fixed for virtual functions because they
	// perform "ThisAdjustment". That can be handled in the CreateMethod function provided
	// by the debugger but likely requires the language to be set to DW_LANG_C_Plus_Plus_14
	//
	StructType* StructTy;
	if (Func->Struct) {
		// TODO: Optimization: Could store the struct type in the struct to reduce having
		//                     to perform map lookup to make a new struct type. (May be worse
		//                     or not matter though since it increases the node size).
		//

		             StructTy = Func->GetParentStructType();
		PointerType* LLThisTy = PointerType::Create(StructTy, Context);

		ulen PtrSizeInBits = Context.LLArcoModule
			                        .getDataLayout()
			                        .getPointerSizeInBits();

		// createObjectPointerType adds flags FlagObjectPointer and FlagArtificial which
		// is what we want for "this" pointers.
		//
		llvm::DIType* DIObjectPtrTy = DBuilder->createObjectPointerType(EmitType(LLThisTy));
		DIFuncTys.push_back(DIObjectPtrTy);
	}

	// Creating types for the parameters.
	for (VarDecl* Param : Func->Params) {
		DIFuncTys.push_back(EmitType(Param->Ty));
	}

	// TODO: Need to take into account calling convention here.
	llvm::DISubroutineType* DIFuncTy =
		DBuilder->createSubroutineType(DBuilder->getOrCreateTypeArray(DIFuncTys));

	llvm::DIScope* Scope;

	if (Func->Struct) {
		// Tell it that the scope is that of the structure. This fixes the problem
		// where the function is not able to recognize member variables or the "this"
		// pointer if using DW_LANG_C_Plus_Plus_14.
		//
		Scope = EmitType(StructTy);
	} else {
		Scope = DebugUnit->getFile();
	}


	llvm::DISubprogram* DIFunc = DBuilder->createFunction(
		Scope,
		Func->Name.Text,
		Func->LLFunction->getName(), // Linkage name
		DebugUnit->getFile(),
		Func->Loc.LineNumber,
		DIFuncTy,
		Func->Loc.LineNumber, // TODO: scope line
		llvm::DINode::DIFlags::FlagPrototyped,
		llvm::DISubprogram::DISPFlags::SPFlagDefinition,
		nullptr,
		nullptr //DIForwardDeclaredFunc // There might have been a forward declaration!
	);

	Func->LLFunction->setSubprogram(DIFunc);
	DILexicalScopes.push_back(DIFunc);
	return DIFunc;
}

void arco::DebugInfoEmitter::EmitParam(FuncDecl* Func, VarDecl* Param, llvm::IRBuilder<>& IRBuilder) {

	llvm::DIScope* DIScope = Func->LLFunction->getSubprogram();

	llvm::DILocalVariable* DIVariable = DBuilder->createParameterVariable(
		DIScope,
		Param->Name.Text,
		Param->ParamIdx + 1,
		DebugUnit->getFile(),
		Param->Loc.LineNumber,
		EmitType(Param->Ty),
		true
	);

	DBuilder->insertDeclare(
		//Func->LLFunction->getArg(Param->ParamIdx),
		Param->LLAddress,
		DIVariable,
		DBuilder->createExpression(),
		llvm::DILocation::get(
			Context.LLContext,
			Param->Loc.LineNumber,
			0, // TODO: column
			DIScope),
		IRBuilder.GetInsertBlock()
	);
}

void arco::DebugInfoEmitter::EmitFuncEnd(FuncDecl* Func) {
	DILexicalScopes.clear();
	llvm::DISubprogram* DIFunc = Func->LLFunction->getSubprogram();

	// !! IF THE PROGRAM HAS A FORWARD DECLARATION YOU STILL HAVE TO FINALIZE IT OR THE RETAINED
	// NODES WILL BE TEMPORARY !!
	if (llvm::DISubprogram* DIForwardDecl = DIFunc->getDeclaration()) {
		DBuilder->finalizeSubprogram(DIForwardDecl);
	}
	DBuilder->finalizeSubprogram(DIFunc);
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

void arco::DebugInfoEmitter::EmitThisVar(llvm::Value* LLThisAddr, FuncDecl* Func, llvm::IRBuilder<>& IRBuilder) {
	llvm::DIScope* DIScope = Func->LLFunction->getSubprogram();

	StructType* StructTy  = Func->GetParentStructType();
	PointerType* LLThisTy = PointerType::Create(StructTy, Context);

	llvm::DILocalVariable* DIVariable = DBuilder->createParameterVariable(
		DIScope,
		"this",
		1,
		DebugUnit->getFile(),
		0,
		EmitType(LLThisTy),
		true,
		llvm::DINode::FlagArtificial | llvm::DINode::FlagObjectPointer
	);

	DBuilder->insertDeclare(
		LLThisAddr,
		DIVariable,
		DBuilder->createExpression(),
		llvm::DILocation::get(
			Context.LLContext,
			0,
			0,
			DIScope),
		IRBuilder.GetInsertBlock()
	);
}

void arco::DebugInfoEmitter::EmitGlobalVar(VarDecl* Global, llvm::IRBuilder<>& IRBuilder) {
	// Ensure the compilation unit exists for this function.
	EmitFile(Global->FScope);

	llvm::DIGlobalVariableExpression* DIGVE = DBuilder->createGlobalVariableExpression(
		DebugUnit,
		Global->Name.Text,
		Global->LLAddress->getName(), // Linkage name
		DebugUnit->getFile(),
		Global->Loc.LineNumber,
		EmitType(Global->Ty),
		false, // TODO: Is local to unit?
		true
	);

	llvm::GlobalVariable* LLGlobal = llvm::cast<llvm::GlobalVariable>(Global->LLAddress);
	LLGlobal->addDebugInfo(DIGVE);
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
	if (DebugUnit) { // Only finalize if there is something to finalize.
		DBuilder->finalize();
	}
}

llvm::DIType* arco::DebugInfoEmitter::EmitType(Type* Ty, llvm::DINode::DIFlags DIFlags) {
	if (DIFlags != llvm::DINode::FlagZero) {
		// TODO: should probably still cache.
		return EmitFirstSeenType(Ty, DIFlags);
	}
	auto Itr = Context.LLDITypeCache.find(Ty->GetUniqueId());
	if (Itr != Context.LLDITypeCache.end()) {
		return Itr->second;
	}
	return EmitFirstSeenType(Ty, DIFlags);
}

llvm::DIType* arco::DebugInfoEmitter::EmitFirstSeenType(Type* Ty, llvm::DINode::DIFlags DIFlags) {
#define MAKE_BASIC_TY(Name, BitSize, DT) {                             \
	llvm::DIType* DITy = DBuilder->createBasicType(Name, BitSize, DT); \
	Context.LLDITypeCache.insert({ Ty->GetUniqueId(), DITy });         \
	return DITy;                                                       \
}
	switch (Ty->GetKind()) {
	case TypeKind::Void:   return nullptr; // Yes this is correct it expects nullptr.
	case TypeKind::Int: {
		ulen PtrSizeInBits = Context.LLArcoModule
			                         .getDataLayout()
			                         .getPointerSizeInBits();
		MAKE_BASIC_TY("int", PtrSizeInBits, llvm::dwarf::DW_ATE_signed);
	}
	case TypeKind::Ptrsize: {
		ulen PtrSizeInBits = Context.LLArcoModule
			                         .getDataLayout()
			                         .getPointerSizeInBits();
		MAKE_BASIC_TY("ptrsize", PtrSizeInBits, llvm::dwarf::DW_ATE_unsigned);
	}
	case TypeKind::Int8:    MAKE_BASIC_TY("int8"  , 8 , llvm::dwarf::DW_ATE_signed);
	case TypeKind::Int16:   MAKE_BASIC_TY("int16" , 16, llvm::dwarf::DW_ATE_signed);
	case TypeKind::Int32:   MAKE_BASIC_TY("int32" , 32, llvm::dwarf::DW_ATE_signed);
	case TypeKind::Int64:   MAKE_BASIC_TY("int64" , 64, llvm::dwarf::DW_ATE_signed);
	case TypeKind::UInt8:   MAKE_BASIC_TY("uint8" , 8 , llvm::dwarf::DW_ATE_unsigned);
	case TypeKind::UInt16:  MAKE_BASIC_TY("uint16", 16, llvm::dwarf::DW_ATE_unsigned);
	case TypeKind::UInt32:  MAKE_BASIC_TY("uint32", 32, llvm::dwarf::DW_ATE_unsigned);
	case TypeKind::UInt64:  MAKE_BASIC_TY("uint64", 64, llvm::dwarf::DW_ATE_unsigned);
	case TypeKind::Char:    MAKE_BASIC_TY("char"  , 8,  llvm::dwarf::DW_ATE_signed_char);
	case TypeKind::Bool:    MAKE_BASIC_TY("bool"  , 8, llvm::dwarf::DW_ATE_boolean);
	case TypeKind::Float32: MAKE_BASIC_TY("f32"   , 32, llvm::dwarf::DW_ATE_float);
	case TypeKind::Float64: MAKE_BASIC_TY("f64"   , 64, llvm::dwarf::DW_ATE_float);
	case TypeKind::Pointer:
	case TypeKind::CStr: {
		ulen PtrSizeInBits = Context.LLArcoModule
			                        .getDataLayout()
			                        .getPointerSizeInBits();

		llvm::DIType* DITy;
		if (Ty->GetPointerElementType(Context)->GetKind() == TypeKind::Interface) {
			DITy = DBuilder->createPointerType(
				EmitType(Context.VoidType),
				PtrSizeInBits,
				0 // TODO alignment
				);
		} else {
			DITy = DBuilder->createPointerType(
				EmitType(Ty->GetPointerElementType(Context)),
				PtrSizeInBits,
				0 // TODO alignment
				);
		}

		Context.LLDITypeCache.insert({ Ty->GetUniqueId(), DITy });
		return DITy;
	}
	case TypeKind::Slice: {
		SliceType* SliceTy = Ty->AsSliceTy();
		llvm::StructType* LLSliceTy = llvm::cast<llvm::StructType>(GenType(Context, SliceTy));

		const llvm::StructLayout* LLLayout = Context.LLArcoModule
			                                        .getDataLayout()
			                                        .getStructLayout(LLSliceTy);

		u64 SizeInBits = LLLayout->getSizeInBits();

		llvm::DICompositeType* DIStructTy = DBuilder->createStructType(
			nullptr,
			LLSliceTy->getName(),
			DebugUnit->getFile(),
			0,
			SizeInBits,
			0, //Context.LLArcoModule.getDataLayout().getPrefTypeAlignment(LLSliceTy) * 8, // *8 because wants bits
			DIFlags,
			nullptr,
			llvm::DINodeArray(),
			0,
			nullptr,
			LLSliceTy->getName()
		);
		Context.LLDITypeCache.insert({ Ty->GetUniqueId(), DIStructTy });

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
			0, //Context.LLArcoModule.getDataLayout().getPrefTypeAlignment(LLLengthType) * 8, // *8 because wants bits
			0,
			DIFlags,
			EmitType(Context.IntType)
		);
		DIFieldTys.push_back(DIMemberLengthType);

		Type* ElmTy = SliceTy->GetElementType();
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
			0, //Context.LLArcoModule.getDataLayout().getPrefTypeAlignment(LLArrPtrType) * 8, // *8 because wants bits
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

		llvm::DIType* DIArrayTy = DBuilder->createArrayType(
			static_cast<uint64_t>(ArrayTy->GetTotalLinearLength()) * Context.LLArcoModule
			                                         .getDataLayout()
			                                         .getTypeSizeInBits(
														  GenType(Context, ArrayTy->GetBaseType())
													 ),
			0, // TODO: Alignment
			DIBaseTy,
			DBuilder->getOrCreateArray(DISubscriptSizes));
		Context.LLDITypeCache.insert({ Ty->GetUniqueId(), DIArrayTy });
		return DIArrayTy;
	}
	case TypeKind::Function: {
		llvm::SmallVector<llvm::Metadata*, 4> DIFuncTys;
		FunctionType* FuncTy = Ty->AsFunctionType();

		llvm::Metadata* DIRetTy = EmitType(FuncTy->RetTyInfo.Ty);
		DIFuncTys.push_back(DIRetTy);
		for (TypeInfo ParamInfo : FuncTy->ParamTypes) {
			DIFuncTys.push_back(EmitType(ParamInfo.Ty));
		}

		// Confusing but this shouldnt be able to have circular dependency even since you
		// cannot nest one's own function type within itself.
		ulen PtrSizeInBits = Context.LLArcoModule
			                        .getDataLayout()
			                        .getPointerSizeInBits();
		llvm::DIType* DIFuncTy = DBuilder->createSubroutineType(
			                           DBuilder->getOrCreateTypeArray(DIFuncTys));
		DIFuncTy = DBuilder->createPointerType(DIFuncTy, PtrSizeInBits);
		Context.LLDITypeCache.insert({ Ty->GetUniqueId(), DIFuncTy });
		return DIFuncTy;
	}
	case TypeKind::Struct: {
		StructType* StructTy = Ty->AsStructType();
		StructDecl* Struct = StructTy->GetStruct();

		const llvm::StructLayout* LLLayout = Context.LLArcoModule
			                                        .getDataLayout()
			                                        .getStructLayout(StructTy->LLStructType);

		u64 SizeInBits = LLLayout->getSizeInBits();


		// NOTE: Commenting just to leave this here for future reference in case it is needed:
		//       LLVM creates the type as distinct. I think distinct only refers to internal
		//       storage and does not actually effect debug results. However, I could be wrong
		//       and in case it is needed to make distinct this is a valid approach but it is likely
		//       having to make a copy of the type which is wasteful.
		//
		/*llvm::DICompositeType* DIStructTy = DBuilder->createReplaceableCompositeType(
			llvm::dwarf::DW_TAG_structure_type,
			Struct->Name.Text,
			DebugUnit->getFile(), // TODO: Scope?
			DebugUnit->getFile(),
			Struct->Loc.LineNumber,
			0, // runtime language (clang leaves out so we will leave out)
			SizeInBits,
			0, //Context.LLArcoModule.getDataLayout().getPrefTypeAlignment(Struct->LLStructTy) * 8, // *8 because wants bits
			DIFlags | llvm::DINode::FlagTypePassByValue,
			Struct->LLStructTy->getName(),
			nullptr // annotations
		);*/
		//DIStructTy = DIStructTy->replaceWithDistinct(llvm::TempDICompositeType(DIStructTy));

		llvm::DICompositeType* DIStructTy = DBuilder->createStructType(
			nullptr,
			Struct->Name.Text,
			DebugUnit->getFile(),
			Struct->Loc.LineNumber,
			SizeInBits,
			0, //Context.LLArcoModule.getDataLayout().getPrefTypeAlignment(Struct->LLStructTy) * 8, // *8 because wants bits
			DIFlags | llvm::DINode::FlagTypePassByValue,
			nullptr, // TODO: Derived from?
			llvm::DINodeArray(),
			0, // lang clang ignores so we ignore.
			nullptr,
			StructTy->LLStructType->getName()
		);
		Context.LLDITypeCache.insert({ Ty->GetUniqueId(), DIStructTy });

		llvm::SmallVector<llvm::Metadata*, 16> DIFieldTys;
		u64 BitsOffset = 0;
		for (VarDecl* Field : Struct->Fields) {
			u64 OffsetInBits = LLLayout->getElementOffsetInBits(Field->FieldIdx);
			DIFieldTys.push_back(EmitMemberFieldType(DIStructTy, Field, OffsetInBits));
		}
		// NOTE: Commented out for now since it seems not to matter but the clang
		//       version actual adds the functions as part of the type information.
		//
		// Also have to emit all the member functions as elements.
		/*IRGenerator IRGen(Context);
		for (auto& [Name, FuncList] : Struct->Funcs) {
			for (FuncDecl* Func : FuncList) {
				DIFieldTys.push_back(EmitFunc(Func, true));
			}
		}*/

		DBuilder->replaceArrays(DIStructTy, DBuilder->getOrCreateArray(DIFieldTys));

		return DIStructTy;
	}
	case TypeKind::Enum: {
		// TODO: There is a createEnumerationType function under DIbuilder

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
		0, //Context.LLArcoModule.getDataLayout().getPrefTypeAlignment(LLType) * 8, // *8 because wants bits
		BitsOffset,
		llvm::DINode::DIFlags::FlagZero,
		EmitType(Field->Ty)
	);

	return DIMemberType;
}
