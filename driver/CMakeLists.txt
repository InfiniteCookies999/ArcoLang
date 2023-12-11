cmake_minimum_required (VERSION ${PROJECT_CMAKE_VERSION})
set(CMAKE_CXX_STANDARD ${PROJECT_CXX_STANDARD})

add_executable (driver "Driver.cpp")
set_target_properties (
	driver
	PROPERTIES
		OUTPUT_NAME "arco"
)

target_link_libraries(driver LINK_PUBLIC arco)

