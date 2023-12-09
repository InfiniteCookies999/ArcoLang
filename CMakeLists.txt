set(PROJECT_CMAKE_VERSION 3.8)
set(PROJECT_CXX_STANDARD 23)

cmake_minimum_required (VERSION ${PROJECT_CMAKE_VERSION})


project ("arco")

# Include sub-projects.
add_subdirectory ("arco")
add_subdirectory ("tests")
