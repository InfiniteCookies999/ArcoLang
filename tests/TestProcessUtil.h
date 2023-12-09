#ifndef ARCO_TEST_PROCESS_UTIL_H
#define ARCO_TEST_PROCESS_UTIL_H

#include <string>
#include <tuple>

/// Creates a new process in a hiden window. The resulting
/// standard output of the process is returned in the string.
/// 
/// The boolean indicates if there was a failure or not when
/// running the process.
/// 
std::tuple<std::string, bool> RunProcess(const char* Process);

#endif // ARCO_TEST_PROCESS_UTIL_H