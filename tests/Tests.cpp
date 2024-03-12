
#include <Process.h>
#include <Logger.h>
#include <TermColors.h>
#include <Compiler.h>

#include <llvm/Support/raw_ostream.h>
#include <llvm/ADT/StringSet.h>

#define SRC(x) ARCO_TEST_SOURCE_DIR x

static ulen Failed = 0;
static ulen Succeeded = 0;
static llvm::SmallVector<const char*, 4> FailedTests;
static llvm::StringSet RanTests;

void RunTest(const char* TestSource, const std::string& ExpectedOutput) {
    llvm::outs() << "Testing: \"" << TestSource << "\"\n";
    llvm::outs() << "----------------------------\n";

    // Prevent accidently running a test twice
    if (RanTests.find(TestSource) != RanTests.end()) {
        llvm::outs() << "Status:";
        arco::SetTerminalColor(arco::TerminalColorRed);
        llvm::outs() << " (FAIL)";
        arco::SetTerminalColor(arco::TerminalColorDefault);
        llvm::outs() << " Ran test twice!\n";
        FailedTests.push_back(TestSource);
        ++Failed;
        return;
    }

    RanTests.insert(TestSource);

    llvm::SmallVector<arco::Source> Sources;
    Sources.push_back(arco::Source{ true, "default.program.module", SRC("test_utils.arco") });
    Sources.push_back(arco::Source{ true, "default.program.module", TestSource });

    arco::Compiler Compiler;
    Compiler.StandAlone = true;
    Compiler.Compile(Sources);

    if (!arco::FoundCompileError) {
        std::string StdOutResult;
        int Ok = arco::ExeHiddenProcess("program", StdOutResult);
        if (!Ok) {
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

    // Reset for next test.
    arco::FoundCompileError = false;

    llvm::outs() << "\n\n";

}

int main() {
    
    RunTest(SRC("hello_world.arco"), "Hello World!");
    RunTest(SRC("binary_operators.arco"), "464");
    RunTest(SRC("hex_literals.arco"), "43809 1048575 0 11259375");
    RunTest(SRC("bin_literals.arco"), "941 32767 0 1404243");
    RunTest(SRC("float_literals.arco"), "522157 4000000 4000000 4000000 4000000 16 16 3314 5 0");
    RunTest(SRC("increments.arco"), "2 0 0 1 1 0 1 0");
    RunTest(SRC("pointer_arithmetic.arco"), "hello worldello worldo worldworld worldello world");
    RunTest(SRC("heap_alloc/heap_alloc1.arco"), "315 341 88 341 66 341 66 341 66 341 66");
    RunTest(SRC("heap_alloc/heap_alloc2.arco"), "4 52 9 7 5 23 1 5 5 2 6 11");
    RunTest(SRC("bool_literals.arco"), "ok1ok2ok3ok4");
    RunTest(SRC("var_decl_list.arco"), "5 3 2");
    RunTest(SRC("literal_type_info.arco"), "int8 int16 int32 int64 uint8 uint16 uint32 uint64 ");
    RunTest(SRC("function_type.arco"), "foo1 4 73");
    RunTest(SRC("sizeof_test.arco"), "1 2 4 8 1");
    RunTest(SRC("constant_folding.arco"), "4 2 61 1 52 8");
    RunTest(SRC("nested_blocks.arco"), "5 8 5");
    RunTest(SRC("default_args.arco"), "3 55 9 3 2 9 3 2 1");
    RunTest(SRC("and_operator.arco"), "case1case1");
    RunTest(SRC("or_operator.arco"), "case1case2case3case1case2case3");
    RunTest(SRC("infered_types.arco"), "5 a message! 16 31 53 99 6 8 nice");
    RunTest(SRC("constructor_initializers.arco"), "77 32");
    RunTest(SRC("copy_constructors.arco"), "Begin a created Called!");
    RunTest(SRC("move_constructors.arco"), "Begin a created Called!Called!");
     
    RunTest(SRC("loops/loops1.arco"), "10");
    RunTest(SRC("loops/loops2.arco"), "10");
    RunTest(SRC("loops/loops3.arco"), "10");
    RunTest(SRC("loops/loops4.arco"), "1 3 5 7 9 11 13 15 17 19 21 23 25 27 29 31 33 35 37 39 41 43 45 47 49 ");
    RunTest(SRC("loops/loops5.arco"), "124 346 22 15 77 ");
    RunTest(SRC("loops/loops6.arco"), "124 346 22 15 77 ");
    RunTest(SRC("loops/loops7.arco"), "124 346 22 15 77 ");
    RunTest(SRC("loops/loops8.arco"), "124 346 22 15 77 ");
    RunTest(SRC("loops/loops9.arco"), "go!go!go!go!go!go!");
    RunTest(SRC("loops/loops10.arco"), "go!go!go!go!go!");
    RunTest(SRC("loops/loops11.arco"), "0 1 2 3 4 5 ");
    RunTest(SRC("loops/loops12.arco"), "0 1 2 3 4 ");
    RunTest(SRC("loops/loops13.arco"), "0 1 2 3 4 5 6 ");
    RunTest(SRC("loops/loops14.arco"), "0 1 2 3 4 5 ");
    RunTest(SRC("loops/loops15.arco"), "foo called finished");
    RunTest(SRC("loops/loops16.arco"), "10");
    RunTest(SRC("loops/loops17.arco"), "5");
    RunTest(SRC("loops/loops18.arco"), "124346221577");
    RunTest(SRC("loops/loops19.arco"), "10");
    RunTest(SRC("loops/loops20.arco"), "10");
    
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
    RunTest(SRC("arrays/arrays15.arco"), "235 2 63");
    RunTest(SRC("arrays/arrays16.arco"), "235 2 63");
    RunTest(SRC("arrays/arrays17.arco"), "235 2 63");
    RunTest(SRC("arrays/arrays18.arco"), "235 2 63");
    RunTest(SRC("arrays/decay_testing.arco"), "9 14 235 11 5 77 22 76 11 9");
    
    RunTest(SRC("structs/structs1.arco"), "55 44");
    RunTest(SRC("structs/structs2.arco"), "55 44");
    RunTest(SRC("structs/structs3.arco"), "55 44");
    RunTest(SRC("structs/structs4.arco"), "55 44");
    RunTest(SRC("structs/structs5.arco"), "124 66 25");
    RunTest(SRC("structs/structs6.arco"), "124 66 25");
    RunTest(SRC("structs/structs7.arco"), "55 99");
    RunTest(SRC("structs/structs8.arco"), "55 55 55");
    
    RunTest(SRC("returns/returns1.arco"), "55 55");
    RunTest(SRC("returns/returns2.arco"), "55 55");
    RunTest(SRC("returns/returns3.arco"), "55 55");
    RunTest(SRC("returns/returns4.arco"), "55 55");
    RunTest(SRC("returns/returns5.arco"), "55 55");
    RunTest(SRC("returns/returns6.arco"), "55 55");
    RunTest(SRC("returns/returns7.arco"), "44 55");
    
    RunTest(SRC("member_functions/member_functions1.arco"), "1234");
    RunTest(SRC("member_functions/member_functions2.arco"), "1234");
    RunTest(SRC("member_functions/member_functions3.arco"), "foo1foo2");
    RunTest(SRC("member_functions/member_functions4.arco"), "22");
    RunTest(SRC("member_functions/constructors1.arco"), "53");
    RunTest(SRC("member_functions/constructors2.arco"), "55");
    
    RunTest(SRC("globals/globals1.arco"), "44");
    RunTest(SRC("globals/globals2.arco"), "32 186");
    RunTest(SRC("globals/globals3.arco"), "34 66");
    RunTest(SRC("globals/globals4.arco"), "22 1111");
    RunTest(SRC("globals/globals5.arco"), "235 31 235 31 235 31 235 31 235 31 ");
    RunTest(SRC("globals/globals6.arco"), "4 6 3");
    RunTest(SRC("globals/globals7.arco"), "4 6 3");
    RunTest(SRC("globals/globals8.arco"), "4 6 3");
    RunTest(SRC("globals/globals9.arco"), "4 6 3");
    
    RunTest(SRC("destructors/destructors1.arco"), "Begin Destroyed!");
    RunTest(SRC("destructors/destructors2.arco"), "Begin Destroyed!");
    RunTest(SRC("destructors/destructors3.arco"), "Begin Destroyed!");
    RunTest(SRC("destructors/destructors4.arco"), "Begin Destroyed! End");
    RunTest(SRC("destructors/destructors5.arco"), "Begin Destroyed!");
    RunTest(SRC("destructors/destructors6.arco"), "Begin Destroyed! Destroyed! Destroyed! Destroyed! Destroyed! ");
    RunTest(SRC("destructors/destructors7.arco"), "Begin Destroyed!");
    RunTest(SRC("destructors/destructors8.arco"), "Destroyed!5Destroyed!8Destroyed!5");
    RunTest(SRC("destructors/destructors9.arco"), "Destroyed!3Destroyed!5");
    RunTest(SRC("destructors/destructors10.arco"), "Begin Destroyed!");
    RunTest(SRC("destructors/destructors11.arco"), "Destroyed!Destroyed!Destroyed!Destroyed!Destroyed!");
    RunTest(SRC("destructors/destructors12.arco"), "Destroyed!Destroyed!Destroyed!");
    RunTest(SRC("destructors/destructors13.arco"), "NoContinueDestroyed!NoContinueDestroyed!Destroyed!NoContinueDestroyed!NoContinueDestroyed!");
    RunTest(SRC("destructors/destructors14.arco"), "Begin Destroyed!Destroyed!");
    RunTest(SRC("destructors/destructors15.arco"), "10");
    
    RunTest(SRC("enums/enums1.arco"), "0 1 2 3 4 5 6");
    RunTest(SRC("enums/enums2.arco"), "0 1 2 3 4 5 6");
    RunTest(SRC("enums/enums3.arco"), "2 3 4 5 6 7 8");
    RunTest(SRC("enums/enums4.arco"), "6 9 6 1 7 2 1");
    RunTest(SRC("enums/enums5.arco"), "Programming is fun cool message");
    RunTest(SRC("enums/enums6.arco"), "Programming is fun cool message");
    
    RunTest(SRC("slices/slices1.arco"), "134 55 22 6");
    RunTest(SRC("slices/slices2.arco"), "134 55 22 6");
    RunTest(SRC("slices/slices3.arco"), "134 55 22 6");
    RunTest(SRC("slices/slices4.arco"), "134 55 22 6 42 66 ");
    RunTest(SRC("slices/slices5.arco"), "134 55 22 6");
    
    RunTest(SRC("varargs/varargs1.arco"), "0");
    RunTest(SRC("varargs/varargs2.arco"), "2 44 122");
    RunTest(SRC("varargs/varargs3.arco"), "23 62 11");
    RunTest(SRC("varargs/varargs4.arco"), "21 665 11");
    RunTest(SRC("varargs/varargs5.arco"), "31 3");
    
    RunTest(SRC("implicit_ptrs/implicit_ptrs1.arco"), "55");
    RunTest(SRC("implicit_ptrs/implicit_ptrs2.arco"), "65 72");
    RunTest(SRC("implicit_ptrs/implicit_ptrs3.arco"), "65 72");
    
    RunTest(SRC("named_args/named_args1.arco"), "314 14");
    RunTest(SRC("named_args/named_args2.arco"), "314 14");
    RunTest(SRC("named_args/named_args3.arco"), "314 14");
    RunTest(SRC("named_args/named_args4.arco"), "314 14");
    RunTest(SRC("named_args/named_args5.arco"), "314 14");
    RunTest(SRC("named_args/named_args6.arco"), "314 14");
    RunTest(SRC("named_args/named_args7.arco"), "42 11 83 61 342");
    
    RunTest(SRC("interfaces/interfaces1.arco"), "42");
    RunTest(SRC("interfaces/interfaces2.arco"), "42 341");
    RunTest(SRC("interfaces/interfaces3.arco"), "52");
    RunTest(SRC("interfaces/interfaces4.arco"), "42 32 56 88");
    
    RunTest(SRC("multiarray_to_voidptr_craziness.arco"), "hello hello hello hello hello hello hello hello hello ");
    
    RunTest(SRC("generics/generics1.arco"), "44 hello");
    RunTest(SRC("generics/generics2.arco"), "44 562");
    RunTest(SRC("generics/generics3.arco"), "44 hello");
    RunTest(SRC("generics/generics4.arco"), "44");
    RunTest(SRC("generics/generics5.arco"), "662 52 113 abc xyz ijq ");
    RunTest(SRC("generics/generics6.arco"), "52 some message");
    RunTest(SRC("generics/generics7.arco"), "6524");
    RunTest(SRC("generics/generics8.arco"), "Programming is fun cool message");

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