#ifndef ARCO_COMPILER_H
#define ARCO_COMPILER_H

#include "Prelude.h"

#include <llvm/ADT/SmallVector.h>
#include <llvm/ADT/StringRef.h>

namespace std {
	namespace filesystem {
		class path;
	}
}

namespace arco {

	class ArcoContext;
	class Module;

	struct Source {
		llvm::StringRef ModName;
		llvm::StringRef Path;
	};

	class Compiler {
	public:
		// Displays how long different processes of
		// compilation took.
		bool DisplayTimes  = false;
		// When true it does not compile with
		// the standard library.
		bool StandAlone    = false;
		// When true it displays all of the LLVM IR
		// generated by the compiler.
		//
		// Warning: The amount of LLVM IR generated can
		// be potentially huge.
		bool DisplayLLVMIR = false;
		// Once the program has finished compiling and linking
		// the program will be executed.
		bool RunProgram    = false;
		// Creates debug information that debuggers like gdb
		// can use to place breakpoints and examine variables
		// in the source code as the program executes.
		bool EmitDebugInfo = false;
		// Deletes the object files after linking.
		bool ShouldDeleteObjectFiles = true;

		Compiler();

		~Compiler();

		void Compile(llvm::SmallVector<Source>& Sources);
	
	private:
		ArcoContext& Context;

		void ParseDirectoryFiles(Module* Mod, const std::filesystem::path& DirectoryPath, ulen PrimaryPathLen);

		void ParseFile(Module* Mod, const std::string& RelativePath, const std::string& AbsolutePath);

		const char* GetStdLibPath();

	};
}

#endif // ARCO_COMPILER_H