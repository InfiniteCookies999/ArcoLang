cmake_minimum_required (VERSION ${PROJECT_CMAKE_VERSION})
set(CMAKE_CXX_STANDARD ${PROJECT_CXX_STANDARD})

# Add source to this project's executable.
add_executable (fuzzing "Fuzzing.cpp"   "FuzzUtils.h" "FuzzUtils.cpp")

target_link_libraries(fuzzing LINK_PUBLIC arco)