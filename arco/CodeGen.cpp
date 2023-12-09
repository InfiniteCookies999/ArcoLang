#include "CodeGen.h"

#include "Logger.h"

// LLVM Target
#include <llvm/MC/TargetRegistry.h>
#include <llvm/Support/Host.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Target/TargetOptions.h>
#include <llvm/Target/TargetMachine.h>

// Needed for writing .o files
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/IR/LegacyPassManager.h>

bool arco::InitLLVMNativeTarget() {
	if (llvm::InitializeNativeTarget())           return false;
	if (llvm::InitializeNativeTargetAsmParser())  return false;
	if (llvm::InitializeNativeTargetAsmPrinter()) return false;
	return true;
}

llvm::TargetMachine* arco::CreateLLVMTargetMache() {

	std::string TargetTriple = llvm::sys::getDefaultTargetTriple();

	std::string TargetErrorMsg;
	auto Target = llvm::TargetRegistry::lookupTarget(TargetTriple, TargetErrorMsg);

	if (!Target) {
		Logger::GlobalError(llvm::errs(), "Machine Target Error: %s", TargetErrorMsg);
		return nullptr;
	}

	std::string CPU = "generic";
	std::string Features = "";
	llvm::TargetOptions TargetOpts;

	llvm::Triple TT = llvm::Triple(TargetTriple);

	auto RM = llvm::Optional<llvm::Reloc::Model>();
	llvm::TargetMachine* TargetMachine =
		Target->createTargetMachine(TargetTriple, CPU, Features, TargetOpts, RM,
			llvm::NoneType::None, llvm::CodeGenOpt::None);

	return TargetMachine;
}

void arco::SetTargetToModule(llvm::Module& LLModule, llvm::TargetMachine* LLTargetMachine) {
	auto TargetTriple = llvm::sys::getDefaultTargetTriple();
	LLModule.setTargetTriple(TargetTriple);
	LLModule.setDataLayout(LLTargetMachine->createDataLayout());
}

bool arco::WriteObjFile(const char* FilePath, llvm::Module& LLModule, llvm::TargetMachine* LLTargetMachine) {
	
	std::error_code ErrCode;
	llvm::raw_fd_ostream OSFileDest(FilePath, ErrCode, llvm::sys::fs::OF_None);

	if (ErrCode) {
		// TODO
		Logger::GlobalError(llvm::errs(), "Could not open obj to write: %s",
			ErrCode.message());
		return false;
	}

	llvm::legacy::PassManager Pass;
	if (LLTargetMachine->addPassesToEmitFile(Pass, OSFileDest, nullptr, llvm::CGFT_ObjectFile)) {
		Logger::GlobalError(llvm::errs(), "TheTargetMachine can't emit a file of this type");
		return false;
	}

	Pass.run(LLModule);
	OSFileDest.flush();

	return true;
}
