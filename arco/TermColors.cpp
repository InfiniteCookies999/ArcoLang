#include "TermColors.h"

#ifdef _WIN32
#include <Windows.h>
#endif

// Satisfying linkage
bool arco::DisableTerminalColors = false;

void arco::SetTerminalColor(u32 ColorCode) {
    if (DisableTerminalColors) {
        return;
    }
#ifdef _WIN32
    SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), ColorCode);
#endif
}
