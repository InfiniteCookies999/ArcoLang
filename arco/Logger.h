#ifndef ARCO_LOGGER_H
#define ARCO_LOGGER_H

#include <llvm/Support/raw_ostream.h>
#include <functional>

#include "Source.h"

namespace arco {

    // True if an error was found during compilation.
    // This includes reading files, parsing, semantic anlysis,
    // IR generation, and machine code generation.
    extern bool FoundCompileError;

    extern ulen TotalAccumulatedErrors;
    extern ulen TOTAL_ALLOWED_ERRORS;

    extern bool ShortErrors;

    class Logger {
    public:

        Logger(const char* FilePath, SourceBuf Buffer);

        void BeginError(SourceLoc Loc, const char* MsgHeader, bool ShowPeriod = true) {
            PrimaryErrLoc = Loc;
            InternalErrorHeaderPrinting(Loc, [this, MsgHeader]() { OS << MsgHeader; }, ShowPeriod);
        }

        ulen CalcHeaderIndent(SourceLoc Loc);

        void SetMsgToShowAbovePrimaryLocAligned(const char* Msg) {
            ExtErrMsgAbovePrimaryLocAligned = Msg;
        }

        template<typename... TArgs>
        void BeginError(SourceLoc Loc, const char* Fmt, TArgs&&... Args) {
            PrimaryErrLoc = Loc;
            InternalErrorHeaderPrinting(Loc, [&]() {
                ForwardFmt(OS, Fmt, std::forward<TArgs>(Args)...);
            }, true);
        }

        static void GlobalError(llvm::raw_ostream& OS, const char* Msg) {
            GlobalError(OS, [&]() { OS << Msg; });
        }

        template<typename... TArgs>
        static void GlobalError(llvm::raw_ostream& OS, const char* Fmt, TArgs&&... Args) {
            GlobalError(OS, [&]() {
                ForwardFmt(OS, Fmt, std::forward<TArgs>(Args)...);
                });
        }

        void AddNoteLine(const std::function<void(llvm::raw_ostream&)>& NoteLinePrinter) {
            NoteLines.push_back(NoteLinePrinter);
        }

        void EndError();

        const char* GetFilePath() const { return FilePath; }

    private:
        llvm::raw_ostream& OS;

        const char* FilePath;
        SourceBuf   Buffer;

        const char* ExtErrMsgAbovePrimaryLocAligned = nullptr;
        // Current information for printing the error.
        SourceLoc   PrimaryErrLoc;
        std::string LNPad; // New line pad for displaying error location.
        ulen LargestLineNum;

        llvm::SmallVector<std::function<void(llvm::raw_ostream&)>> NoteLines;

        void InternalErrorHeaderPrinting(SourceLoc Loc, const std::function<void()>& Printer, bool ShowPeriod);

        void DisplayErrorLoc(SourceLoc Loc, const std::vector<std::string>& Lines);

        std::string RangeFromWindow(const char* Loc, i64 Direction);

        static void GlobalError(llvm::raw_ostream& OS, const std::function<void()>& Printer);

        template<typename T>
        static void ForwardFmt(llvm::raw_ostream& OS, const char* Fmt, T&& Val) {
            for (; *Fmt != '\0'; Fmt++) {
                if (*Fmt == '%' && *(Fmt + 1) == 's') {
                    OS << std::forward<T>(Val);
                    ++Fmt;
                    continue;
                }
                OS << *Fmt;
            }
        }

        template<typename T, typename... TArgs>
        static void ForwardFmt(llvm::raw_ostream& OS, const char* Fmt, T&& Val, TArgs&&... Args) {
            for (; *Fmt != '\0'; Fmt++) {
                if (*Fmt == '%' && *(Fmt + 1) == 's') {
                    OS << std::forward<T>(Val);
                    ForwardFmt(OS, Fmt + 2, std::forward<TArgs>(Args)...);
                    return;
                }
                OS << *Fmt;
            }
        }

    };

}

#endif // ARCO_LOGGER_H