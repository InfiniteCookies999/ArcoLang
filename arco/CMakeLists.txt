cmake_minimum_required (VERSION ${PROJECT_CMAKE_VERSION})
set(CMAKE_CXX_STANDARD ${PROJECT_CXX_STANDARD})

# Some of these warnings on MSVC are rather annoying so ive disabled them
if (CMAKE_CXX_COMPILER_ID STREQUAL "MSVC")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /wd\"26812\"")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /wd\"26495\"")
endif()

#importing LLVM
find_package(LLVM REQUIRED CONFIG)

# Including LLVM source
include_directories(${LLVM_INCLUDE_DIRS})
separate_arguments(LLVM_DEFINITIONS_LIST NATIVE_COMMAND ${LLVM_DEFINITIONS})
add_definitions(${LLVM_DEFINITIONS})


# Add source to this project's executable.
add_library (arco   "Lexer.h" "Lexer.cpp" "Source.h" "Tokens.h" "Tokens.cpp" "Prelude.h" "Parser.h" "Parser.cpp" "AST.h" "Identifier.h" "Identifier.cpp" "Logger.h" "Logger.cpp" "TermColors.h" "TermColors.cpp" "Types.h" "Context.h" "Context.cpp" "IRGen.h" "IRGen.cpp" "SemAnalysis.h" "SemAnalysis.cpp" "Types.cpp" "CodeGen.h" "CodeGen.cpp" "Compiler.h" "Compiler.cpp" "FloatConversions.h" "FloatConversions.cpp" "AST.cpp" "Process.h" "Process.cpp"   "EmitDebugInfo.h" "EmitDebugInfo.cpp" "SpellChecking.h" "SpellChecking.cpp"   "Generics.h" "Generics.cpp" "Files.h" "Files.cpp" "ExpandedLoc.h" "ExpandedLoc.cpp")
target_include_directories (arco PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
target_include_directories (arco PUBLIC ${LLVM_INCLUDE_DIRS})


#LLVM Components provide access to the underlying libraries without having
# to specify the libraries being used.
llvm_map_components_to_libnames(LLVM_Libs
  Analysis
  Core
  ExecutionEngine
  InstCombine
  Object
  OrcJIT
  RuntimeDyld
  Support
  TransformUtils
  native
  )

#Linking the LLVM Libraries
target_link_libraries(arco ${LLVM_Libs})
