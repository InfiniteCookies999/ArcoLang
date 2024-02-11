#ifndef ARCO_RUN_PROCESS_H
#define ARCO_RUN_PROCESS_H

#include <string>

namespace arco {

/// Creates a new process in a hidden window. The resulting
/// standard output of the process is returned in the string.
/// 
/// Either returns false if an error occured.
/// 
bool ExeHiddenProcess(const char* Process, std::string& Result);

int ExeProcess(const char* Process, const char* ProcessDir, bool SeperateWindow);

}

#endif // ARCO_RUN_PROCESS_H