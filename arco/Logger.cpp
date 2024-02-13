#include "Logger.h"

#include "TermColors.h"
#include <sstream>

namespace arco {
    // Satisfying linkage
    bool FoundCompileError      = false;
    ulen TotalAccumulatedErrors = 0;
    ulen TOTAL_ALLOWED_ERRORS   = 20;
    bool ShortErrors            = false;
}

arco::Logger::Logger(const char* FilePath, SourceBuf Buffer)
    : FilePath(FilePath), Buffer(Buffer), OS(llvm::errs())
{
}

static std::string ReplaceTabsWithSpaces(const std::string& Tabs) {
    std::string NoTabs;
    for (char Ch : Tabs) {
        if (Ch != '\t') {
            NoTabs += Ch;
        } else {
            NoTabs += "    ";
        }
    }
    return NoTabs;
}

void arco::Logger::EndError() {

    if (TotalAccumulatedErrors == TOTAL_ALLOWED_ERRORS) {
        SetTerminalColor(TerminalColorBrightBlue);
        OS << ">>";
        SetTerminalColor(TerminalColorDefault);
        OS << " Exceeded the maximum allowed error messages. Exiting.\n";
        exit(1);
    }

    ++TotalAccumulatedErrors;

    if (ShortErrors) {
        OS << '\n';
        return;
    }

    std::string Between = ReplaceTabsWithSpaces(PrimaryErrLoc.Text.str());
    if (Between == "\n" || Between == "\r\n" || Between == "\r") {
        Between = "";
    }

    // Breaking the between location into seperate lines.
    std::string BetweenLine;
    std::vector<std::string> BetweenLines;
    const char* BPtr = Between.c_str();
    while (*BPtr) {
        if (*BPtr == '\n') {
            BetweenLines.push_back(BetweenLine);
            BetweenLine = "";
        } else if (*BPtr == '\r') {
            BetweenLines.push_back(BetweenLine);
            BetweenLine = "";
            if (*(BPtr + 1) == '\n') {
                ++BPtr;
            }
        } else {
            BetweenLine += *BPtr;
        }
        ++BPtr;
    }
    if (!BetweenLine.empty())
        BetweenLines.push_back(BetweenLine);

    LargestLineNum = PrimaryErrLoc.LineNumber + BetweenLines.size() - 1;
    LNPad = std::string(std::to_string(LargestLineNum).size(), ' ');

    if (ExtErrMsgAbovePrimaryLocAligned) {
        std::stringstream StrStream(ExtErrMsgAbovePrimaryLocAligned);
        std::string Line;
        bool First = true;
        while (std::getline(StrStream, Line, '\n')) {
            if (!First) {
                OS << "\n";
            }
            First = false;
            OS << LNPad << "  " << Line;
        }
        ExtErrMsgAbovePrimaryLocAligned = nullptr;	
    }
    DisplayErrorLoc(PrimaryErrLoc, BetweenLines);

    if (!NoteLines.empty()) {
        SetTerminalColor(TerminalColorYellow);
        OS << LNPad << "  Help: ";
        SetTerminalColor(TerminalColorDefault);
        NoteLines[0](OS);
        OS << '\n';
        for (ulen i = 1; i < NoteLines.size(); i++) {
            OS << LNPad << "       ";
            NoteLines[i](OS);
            OS << '\n';
        }

        NoteLines.clear();
    }

    OS << '\n';

}

ulen arco::Logger::CalcHeaderIndent(SourceLoc Loc) {
    ulen Total = strlen(FilePath);
    Total += 1 + std::to_string(Loc.LineNumber).length() + 1;
    Total += strlen(" error: ");
    ulen Column = 0;
    const char* MemPtr = Loc.Text.begin();
    while (MemPtr > Buffer.Memory && *MemPtr != '\n' && *MemPtr != '\r') {
        ++Column;
        --MemPtr;
    }
    Total += std::to_string(Column).size() + 1;
    return Total;
}

void arco::Logger::InternalErrorHeaderPrinting(SourceLoc Loc, const std::function<void()>& Printer, bool ShowPeriod) {
    
    SetTerminalColor(TerminalColorWhite);
    OS << FilePath;

    SetTerminalColor(TerminalColorWhite);
    OS << ":" << Loc.LineNumber << ":";
    ulen Column = 0;
    const char* MemPtr = Loc.Text.begin();
    while (MemPtr > Buffer.Memory && *MemPtr != '\n' && *MemPtr != '\r') {
        ++Column;
        --MemPtr;
    }
    OS << Column << ":";

    SetTerminalColor(TerminalColorRed);
    OS << " error: ";

    SetTerminalColor(TerminalColorWhite);
    Printer();
    if (ShowPeriod) {
        OS << ".";
    }
    SetTerminalColor(TerminalColorDefault);
    
    FoundCompileError = true;
}

void arco::Logger::DisplayErrorLoc(SourceLoc Loc, const std::vector<std::string>& Lines) {

    std::string Backwards = ReplaceTabsWithSpaces(RangeFromWindow(Loc.Text.begin(), -40));
    std::string Forwards  = ReplaceTabsWithSpaces(RangeFromWindow(Loc.Text.end() - 1, +40));

    assert(Backwards.find('\n', 0) == std::string::npos && "New Line in display!");
    assert(Forwards.find('\n', 0) == std::string::npos && "New Line in display!");

    OS << "\n";
    OS << LNPad << "  |\n";

    std::string Spaces = std::string(Backwards.size(), ' ');

    ulen LineNumber = Loc.LineNumber;
    for (ulen i = 0; i < Lines.size(); i++) {
        const std::string& Line = Lines[i];
        bool IsLast = i + 1 == Lines.size();

        // Display the line number and bar
        OS << ' ' << (LineNumber + i);
        OS << std::string(std::to_string(LargestLineNum).size() -
                          std::to_string(LineNumber + i).size(), ' ');
        OS << " | ";
        if (i == 0)
            OS << Backwards;

        OS << Line;
    
        if (IsLast) {
            OS << Forwards;
        }

        OS << '\n';

        // Displaying squiggly red underline 
        //      | ~~~
        auto FirstNonSpaceIt = Line.find_first_not_of(' ');
        std::string NonTrailingWhitespaceStr = Line.substr(FirstNonSpaceIt);

        OS << LNPad << "  | ";
        SetTerminalColor(TerminalColorRed);
        if (i == 0) {
            OS << Spaces;
        }
        OS << std::string(Line.size() - NonTrailingWhitespaceStr.size(), ' ')
           << std::string(NonTrailingWhitespaceStr.size(), '~');

        OS << '\n';
        SetTerminalColor(TerminalColorDefault);
    }
}

std::string arco::Logger::RangeFromWindow(const char* Loc, i64 Direction) {
    const char* MemPtr = Loc; // Pointering to character start.

    const char* EndOfBuffer = Buffer.Memory + Buffer.length - 1;
    assert(MemPtr >= Buffer.Memory && MemPtr <= EndOfBuffer && "Not in buffer!");
    
    i64 Moved = 0; // Absolute movement of the pointer.
    while (true) {
        if (*MemPtr == '\n') {
            // Pointer is at a new line.
            if (Direction < 0) ++MemPtr; // Moving in the negative direction so move forward one
            else               --MemPtr; // Moving in the forward direction so move backwards one
            break; // New line so end loop.
        } else if (*MemPtr == '\r' && Direction > 0) {
            
            // Direction > 0 because running into \r in while
            // moving backwards would mean it is a random \r
            // in the file not a new line.
            if (*(MemPtr + 1) == '\n') {
                --MemPtr;
                break;
            } // else \r in middle of memory for some reason.
        }

        ++Moved;

        if (MemPtr == Buffer.Memory || MemPtr == EndOfBuffer) {
            // Hit one end of the buffer so there is nothing more to di.
            break;
        }

        if (Moved == abs(Direction)) {
            // Moved enough.
            break;
        }

        if (Direction < 0) --MemPtr;
        else               ++MemPtr;
    }

    if (Moved == 0) return std::string("");
    if (Direction < 0) {
        //     abcd
        //     ^ <-- moved 3 but length is 4
        return llvm::StringRef(MemPtr, Moved-1).str();
    } else {
        return llvm::StringRef(Loc+1, Moved-1).str();
    }
}

void arco::Logger::GlobalError(llvm::raw_ostream& OS, const std::function<void()>& Printer) {
    
    SetTerminalColor(TerminalColorRed);
    OS << "error: ";
    SetTerminalColor(TerminalColorDefault);

    // Printing the message
    Printer();

    OS << ".\n";

    FoundCompileError = true;
}
