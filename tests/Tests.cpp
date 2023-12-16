#include "TestProcessUtil.h"

#include <Logger.h>
#include <TermColors.h>
#include <Compiler.h>

#include <llvm/Support/raw_ostream.h>

#define SRC(x) ARCO_TEST_SOURCE_DIR x

static ulen Failed = 0;
static ulen Succeeded = 0;
static llvm::SmallVector<const char*, 4> FailedTests;

void RunTest(const char* TestSource, const std::string& ExpectedOutput) {
	llvm::outs() << "Testing: \"" << TestSource << "\"\n";
	llvm::outs() << "----------------------------\n";

	llvm::SmallVector<arco::Source> Sources;
	Sources.push_back(arco::Source{ "default.program.module", SRC("test_utils.arco") });
	Sources.push_back(arco::Source{ "default.program.module", TestSource });

	arco::Compiler Compiler;
	Compiler.StandAlone = true;
	//Compiler.DisplayLLVMIR = true;
	//Compiler.DisplayTimes = true;
	Compiler.Compile(Sources);

	if (!arco::FoundCompileError) {
		auto [StdOutResult, NoErrors] = RunProcess("program");
		if (!NoErrors) {
			llvm::outs() << "Failed to run the compiled program\n";
			FailedTests.push_back(TestSource);
			++Failed;
			return;
		}

		if (StdOutResult == ExpectedOutput) {
			llvm::outs() << "Status:";
			arco::SetTerminalColor(arco::TerminalColorBrightGreen);
			llvm::outs() << " (SUCCESS)";
			arco::SetTerminalColor(arco::TerminalColorDefault);
			llvm::outs() << '\n';
			llvm::outs() << "Program Standard Output: \"";
			llvm::outs() << StdOutResult << "\"\n";
			++Succeeded;
		} else {
			llvm::outs() << "Status:";
			arco::SetTerminalColor(arco::TerminalColorRed);
			llvm::outs() << " (FAIL)";
			arco::SetTerminalColor(arco::TerminalColorDefault);
			llvm::outs() << '\n';
			llvm::outs() << "Program Standard Output:  \"";
			llvm::outs() << StdOutResult << "\"\n";
			llvm::outs() << "Expected Standard Output: \"";
			llvm::outs() << ExpectedOutput << "\"\n";
			FailedTests.push_back(TestSource);
			++Failed;
		}
	} else {
		// The compiler should give some indication as to why
		// it failed to compile if this happens.
		FailedTests.push_back(TestSource);
		++Failed;
	}

	llvm::outs() << "\n\n";

}

int main() {

	RunTest(SRC("hello_world.arco"), "Hello World!");
	RunTest(SRC("binary_operators.arco"), "464");
	RunTest(SRC("hex_literals.arco"), "43809 1048575 0 11259375");
	RunTest(SRC("bin_literals.arco"), "941 32767 0 1404243");
	RunTest(SRC("increments.arco"), "2 0 0 1 1 0 1 0");
	RunTest(SRC("pointer_arithmetic.arco"), "hello worldello worldo worldworld worldello world");
	
	RunTest(SRC("loops/loops1.arco"), "10");
	RunTest(SRC("loops/loops2.arco"), "10");
	RunTest(SRC("loops/loops3.arco"), "10");
	RunTest(SRC("loops/loops4.arco"), "1 3 5 7 9 11 13 15 17 19 21 23 25 27 29 31 33 35 37 39 41 43 45 47 49 ");
	
	RunTest(SRC("arrays/arrays1.arco"), "21 55 11 56 3 ");
	RunTest(SRC("arrays/arrays2.arco"), "412 21 5 6 4 0 ");
	RunTest(SRC("arrays/arrays3.arco"), "412 77 121 45 56 ");
	RunTest(SRC("arrays/arrays4.arco"), "214 452 12 32 11 ");
	RunTest(SRC("arrays/arrays5.arco"), "253 23 0 13 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ");
	RunTest(SRC("arrays/arrays6.arco"), "5 5 5 5 5 5 5 5 ");
	RunTest(SRC("arrays/arrays7.arco"), "1 2 3 4 5 ");
	RunTest(SRC("arrays/arrays8.arco"), "");
	RunTest(SRC("arrays/arrays9.arco"), "24 1 523 0 0 ");
	RunTest(SRC("arrays/arrays10.arco"), "21 55 11 56 3 33 5 1 0 8 3 72 22 ");
	RunTest(SRC("arrays/arrays11.arco"), "14 99 55 1 121 ");
	RunTest(SRC("arrays/arrays12.arco"), "pizza is good");
	RunTest(SRC("arrays/arrays13.arco"), "25 55 25 55 25 55 25 55 25 55 25 55 25 55 25 55 25 55 25 55 25 55 25 55 25 55 25 55 25 55 25 55 25 55 25 55 25 55 25 55 25 55 25 55 25 55 25 55 25 55 ");
	RunTest(SRC("arrays/arrays14.arco"), "1 2 3 4 5 6 7 8 9 10 11 99 ");
	RunTest(SRC("arrays/decay_testing.arco"), "9 14 235 11 5 77 22 76 11 9");
	
	RunTest(SRC("structs/structs1.arco"), "55 44");
	RunTest(SRC("structs/structs2.arco"), "55 44");
	RunTest(SRC("structs/structs3.arco"), "55 44");
	RunTest(SRC("structs/structs4.arco"), "55 44");
	RunTest(SRC("structs/structs5.arco"), "124 66 25");
	RunTest(SRC("structs/structs6.arco"), "124 66 25");
	RunTest(SRC("structs/structs7.arco"), "55 99");
	
	RunTest(SRC("returns/returns1.arco"), "55 55");
	RunTest(SRC("returns/returns2.arco"), "55 55");
	RunTest(SRC("returns/returns3.arco"), "55 55");
	RunTest(SRC("returns/returns4.arco"), "55 55");
	RunTest(SRC("returns/returns5.arco"), "55 55");
	RunTest(SRC("returns/returns6.arco"), "55 55");
	
	RunTest(SRC("member_functions/member_functions1.arco"), "1234");
	RunTest(SRC("member_functions/member_functions2.arco"), "1234");

	//RunTest(SRC("workpad/main.arco"), "");
	//RunTest(SRC("lots_of_errors.arco"), "");

	if (Succeeded + Failed > 0) {
		llvm::outs() << "Passed/Tested (" << Succeeded << "/" << (Succeeded + Failed) << ")\n";
		if (Failed) {
			llvm::outs() << "Failed Tests\n";
			for (const char* Test : FailedTests) {
				llvm::outs() << "\"" << Test << "\"" << '\n';
			}
			return 1;
		}
	}

	return 0;
}