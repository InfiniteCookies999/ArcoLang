#include "Process.h"

#ifdef _WIN32
#include <Windows.h>
#undef min
#undef max
#else
#include <unistd.h>
#include <stdlib.h>
#include <sys/wait.h>
#endif

#define MIN(a, b)  (((a) < (b)) ? (a) : (b))

#include <Logger.h>
#include <llvm/Support/raw_ostream.h>

bool arco::ExeHiddenProcess(const char* Process, std::string& Result) {
#ifdef _WIN32
    HANDLE WriteHandleIn, WriteHandle;

    SECURITY_ATTRIBUTES SecurityAttr;
    SecurityAttr.nLength = sizeof(SECURITY_ATTRIBUTES);
    SecurityAttr.bInheritHandle = TRUE;
    SecurityAttr.lpSecurityDescriptor = NULL;

    if (!CreatePipe(&WriteHandleIn, &WriteHandle, &SecurityAttr, 0)) {
        Logger::GlobalError(llvm::errs(), "Internal compiler error: Failed to create pipe for process");
        return false;
    }

    PROCESS_INFORMATION ProcessInfo;
    STARTUPINFOA StartupInfo = { 0 };
    StartupInfo.cb           = sizeof(STARTUPINFOA);
    StartupInfo.dwFlags      = STARTF_USESHOWWINDOW | STARTF_USESTDHANDLES;
    StartupInfo.hStdOutput   = WriteHandle;
    StartupInfo.hStdError    = WriteHandle;
    StartupInfo.wShowWindow  = SW_HIDE; // Don't show the newly created window.

    if (!CreateProcessA(NULL, (char*)Process, NULL, NULL, TRUE,
                        CREATE_NEW_CONSOLE,
                        NULL,
                        NULL,
                        &StartupInfo,
                        &ProcessInfo)) {
        CloseHandle(WriteHandle);
        CloseHandle(WriteHandleIn);
        return false;
    }

    bool RunningProcess = true;
    while (RunningProcess) {

        // Wait some time to not consume CPU
        RunningProcess = !(WaitForSingleObject(ProcessInfo.hProcess, 50) == WAIT_OBJECT_0);

        while (true) {
            char Buffer[1024];
            DWORD AmountRead = 0, dwAvail = 0;

            if (!PeekNamedPipe(WriteHandleIn, NULL, 0, NULL, &dwAvail, NULL)) {
                break;
            }

            if (!dwAvail) {
                break;
            }

            if (!ReadFile(WriteHandleIn, Buffer, MIN(sizeof(Buffer) - 1, dwAvail), &AmountRead, NULL)) {
                break;
            }

            Buffer[AmountRead] = 0;
            Result += Buffer;
        }
    }

    unsigned long ExitCode;
    GetExitCodeProcess(ProcessInfo.hProcess, &ExitCode);

    CloseHandle(WriteHandle);
    CloseHandle(WriteHandleIn);
    CloseHandle(ProcessInfo.hProcess);
    CloseHandle(ProcessInfo.hThread);
    return true;
#endif
}

int arco::ExeProcess(const char* Process, const char* ProcessDir, bool SeperateWindow) {
#ifdef _WIN32
    // No need to use inherited handles since this does not capture
    // the program's output.

    PROCESS_INFORMATION ProcessInfo;
    STARTUPINFOA StartupInfo = { 0 };
    StartupInfo.cb           = sizeof(STARTUPINFOA);
    if (!CreateProcessA(NULL, (char*)Process, NULL, NULL, FALSE,
                        SeperateWindow ? CREATE_NEW_CONSOLE : 0,
                        NULL,
                        ProcessDir,
                        &StartupInfo,
                        &ProcessInfo)) {
        Logger::GlobalError(llvm::errs(), "Internal compiler error: Failed to create process");
        return 1;
    }
    WaitForSingleObject(ProcessInfo.hProcess, INFINITE);

    unsigned long ExitCode;
    GetExitCodeProcess(ProcessInfo.hProcess, &ExitCode);

    CloseHandle(ProcessInfo.hProcess);
    CloseHandle(ProcessInfo.hThread);
    return ExitCode;
#else
    if(pid_t child = fork(); child == 0) {
        chdir(ProcessDir);
        exit(system(Process));
    } else {
        int exitcode = 0;
        wait(&exitcode);

        return exitcode;
    }
#endif
}
