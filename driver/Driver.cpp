#include "Compiler.h"
#include "Logger.h"
#include <llvm/ADT/StringRef.h>

int main(int argc, char* argv[]) {

	arco::Compiler Compiler;
	
	llvm::SmallVector<arco::Source> Sources;
	for (int i = 1; i < argc; i++) {
		if (argv[i][0] == '-') {
			llvm::StringRef Opt = llvm::StringRef(argv[i] + 1);
			// TODO: Handle command line options
		} else {
			arco::Source Source = {
				"default.program.module",
				argv[i]
			};
			Sources.push_back(Source);
		}
	}

	if (Sources.empty()) {
		arco::Logger::GlobalError(llvm::errs(), "No sources provided");
		return 1;
	}

	Compiler.Compile(Sources);

	return 0;
}