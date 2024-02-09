cmake_minimum_required (VERSION ${PROJECT_CMAKE_VERSION})
set(CMAKE_CXX_STANDARD ${PROJECT_CXX_STANDARD})

# Add source to this project's executable.
add_executable (workarea "WorkArea.cpp")

target_compile_definitions(workarea PUBLIC ARCO_TEST_SOURCE_DIR=\"${PROJECT_SOURCE_DIR}/tests/resources/\")

target_link_libraries(workarea LINK_PUBLIC arco)