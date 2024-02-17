#include "EmitDebugInfo.h"

#include "Context.h"

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
	default:
		// This really should not happen
		return nullptr;
	}
}
