#include "Logger.h"

#include "TermColors.h"
#include "AST.h"
#include <sstream>

namespace arco {
    // Satisfying linkage
    bool FoundCompileError      = false;
    ulen TotalAccumulatedErrors = 0;
    ulen TOTAL_ALLOWED_ERRORS   = 20;
    bool ShortErrors            = false;
    bool FullPaths              = false;
}

arco::Logger::Logger(FileScope* FScope, SourceBuf Buffer)
    : FScope(FScope), Buffer(Buffer), OS(llvm::errs())
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

    if (PrimaryErrLoc.Text.data() == FScope->Buffer.Memory + FScope->Buffer.length) {
        PrimaryErrLoc.Text = llvm::StringRef(PrimaryErrLoc.Text.data() - 1, 1);
    }
    const char* LocData = PrimaryErrLoc.Text.data();
    if (*LocData == '\n' || *LocData == '\r') {
        ulen NewLineNumber = PrimaryErrLoc.LineNumber;
        while (*LocData == '\n' || *LocData == '\r') {
            // Yikes the location is a new line and the GenerateBetweenLines will just
            // discard the line information entirely because of this.
            --LocData;
            --NewLineNumber;
        }
        PrimaryErrLoc.Text = llvm::StringRef(LocData, 1);
        PrimaryErrLoc.LineNumber = NewLineNumber;
    }
    
    if (TotalAccumulatedErrors == TOTAL_ALLOWED_ERRORS) {
        SetTerminalColor(TerminalColorBrightBlue);
        OS << "\n>>";
        SetTerminalColor(TerminalColorDefault);
        OS << " Exceeded the maximum allowed error messages. Exiting.\n";
        exit(1);
    }

    ++TotalAccumulatedErrors;

    if (ShortErrors) {
        OS << '\n';
        return;
    }

    // Breaking the between location into seperate lines.
    std::string BetweenLine;
    std::vector<std::string> PrimaryBetweenLines = GenerateBetweenLines(PrimaryErrLoc.Text);
   
    for (auto& MarkMessage : MarkMessages) {
        MarkMessage.BetweenLines = GenerateBetweenLines(MarkMessage.Loc.Text);
    }

    LargestLineNum = PrimaryErrLoc.LineNumber + PrimaryBetweenLines.size() - 1;
    for (auto& MarkMessage : MarkMessages) {
        ulen LinesLength = MarkMessage.Loc.LineNumber + MarkMessage.BetweenLines.size() - 1;
        if (LinesLength > LargestLineNum) {
            LargestLineNum = LinesLength;
        }
    }

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
    DisplayErrorLoc(PrimaryErrLoc, PrimaryBetweenLines, TerminalColorDefault);

    for (auto MarkMessage : MarkMessages) {
        if (MarkMessage.Message != "") {
            SetTerminalColor(TerminalColorCyan);
            OS << "\n" << LNPad << "  >> " << MarkMessage.Message;
        }
        DisplayErrorLoc(MarkMessage.Loc, MarkMessage.BetweenLines, TerminalColorCyan);
    }
    MarkMessages.clear();
    OS << "\n";

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
    std::string UsedPath = FullPaths ? FScope->FullPath : FScope->Path;
    ulen Total = strlen(UsedPath.c_str());
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
    std::string UsedPath = FullPaths ? FScope->FullPath : FScope->Path;
    OS << UsedPath;

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

std::vector<std::string> arco::Logger::GenerateBetweenLines(llvm::StringRef Text) {
    std::string Between = ReplaceTabsWithSpaces(Text.str());
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

    return BetweenLines;
}

void arco::Logger::DisplayErrorLoc(SourceLoc Loc, const std::vector<std::string>& Lines, u32 ColorCode) {

    std::string Backwards = ReplaceTabsWithSpaces(RangeFromWindow(Loc.Text.begin(), -40));
    std::string Forwards  = ReplaceTabsWithSpaces(RangeFromWindow(Loc.Text.end() - 1, +40));

    assert(Backwards.find('\n', 0) == std::string::npos && "New Line in display!");
    assert(Forwards.find('\n', 0) == std::string::npos && "New Line in display!");

    SetTerminalColor(ColorCode);
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

        
        OS << LNPad << "  | ";
        SetTerminalColor(TerminalColorRed);
        if (i == 0) {
            OS << Spaces;
        }
        
        // Displaying squiggly red underline 
        //      | ~~~
        
        auto FirstNonSpaceIt = Line.find_first_not_of(' ');
        if (FirstNonSpaceIt != std::string::npos) {
            std::string NonTrailingWhitespaceStr = Line.substr(FirstNonSpaceIt);
            OS << std::string(Line.size() - NonTrailingWhitespaceStr.size(), ' ')
                << std::string(NonTrailingWhitespaceStr.size(), '~');
        } else {
            // Well all we have to work with must be a space.
            OS << std::string(Line.size() - 1, ' ') << "~";
        }
        
        if (i + 1 != Lines.size()) {
            OS << '\n';
        }
        SetTerminalColor(ColorCode);
    }

    SetTerminalColor(TerminalColorDefault);
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

    ++TotalAccumulatedErrors;
}
