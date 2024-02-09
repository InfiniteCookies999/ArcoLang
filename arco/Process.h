#ifndef ARCO_RUN_PROCESS_H
#define ARCO_RUN_PROCESS_H

#include <string>

namespace arco {

/// Creates a new process in a hidden window. The resulting
/// standard output of the process is returned in the string.
/// 
/// Either returns 1 if an error occured or the exit code.
/// 
int ExeHiddenProcess(const char* Process, std::string& Result);

int ExeProcess(const char* Process, bool SeperateWindow);

}

#endif // ARCO_RUN_PROCESS_H