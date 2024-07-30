#include "TermColors.h"

#ifdef _WIN32
#include <Windows.h>
#else
#include <stdio.h>
#endif

// Satisfying linkage
bool arco::DisableTerminalColors = false;

void arco::SetTerminalColor(u32 ColorCode) {
    if (DisableTerminalColors) {
        return;
    }
#ifdef _WIN32
    SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), ColorCode);
#else
    char BrightChar = 3;
    char ColorChar = 0;
    switch(ColorCode) {
        case TerminalColorBlack:            ColorChar = 0; break;
        case TerminalColorDarkBlue:         ColorChar = 4; break;
        case TerminalColorDarkGreen:        ColorChar = 2; break;
        case TerminalColorBrightBlue:       ColorChar = 4; BrightChar = 9; break;
        case TerminalColorDarkRed:          ColorChar = 1; break;
        case TerminalColorFadedRed:         ColorChar = 1; break;
        case TerminalColorYellow:           ColorChar = 3; break;
        case TerminalColorDarkYellow:       ColorChar = 3; break;
        case TerminalColorBlue:             ColorChar = 4; break;
        case TerminalColorBrightGreen:      ColorChar = 2; BrightChar = 9; break;
        case TerminalColorCyan:             ColorChar = 6; break;
        case TerminalColorRed:              ColorChar = 1; break;
        case TerminalColorMagenta:          ColorChar = 5; break;
        case TerminalColorBrightYellow:     ColorChar = 3; BrightChar = 9; break;
        case TerminalColorWhite:            ColorChar = 7; break;

        case TerminalColorDefault:
            fprintf(stderr, "\x1b[0m");
            return;

        default:
            break;
    }

    fprintf(stderr, "\x1b[%c%cm", '0' + BrightChar, '0' + ColorChar);
#endif
}
