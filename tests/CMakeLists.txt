cmake_minimum_required (VERSION ${PROJECT_CMAKE_VERSION})
set(CMAKE_CXX_STANDARD ${PROJECT_CXX_STANDARD})

# Add source to this project's executable.
add_executable (tests "Tests.cpp"  )

target_compile_definitions(tests PUBLIC ARCO_TEST_SOURCE_DIR=\"${PROJECT_SOURCE_DIR}/tests/resources/\")

target_link_libraries(tests LINK_PUBLIC arco)