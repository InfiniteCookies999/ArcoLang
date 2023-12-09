#include "Logger.h"

#include "TermColors.h"

namespace arco {
	// Satisfying linkage
	bool FoundCompileError = false;
}

arco::Logger::Logger(const char* FilePath)
	: FilePath(FilePath), OS(llvm::errs())
{
}

void arco::Logger::EndError() {
	OS << "\n";
}

void arco::Logger::InternalErrorHeaderPrinting(SourceLoc Loc, const std::function<void()>& Printer) {
	
	SetTerminalColor(TerminalColorWhite);
	OS << FilePath;

	SetTerminalColor(TerminalColorWhite);
	OS << ":" << Loc.LineNumber << ":";
	SetTerminalColor(TerminalColorRed);
	OS << " Error: ";

	SetTerminalColor(TerminalColorWhite);
	Printer();
	OS << ".";
	SetTerminalColor(TerminalColorDefault);
	
	FoundCompileError = true;
}

void arco::Logger::GlobalError(llvm::raw_ostream& OS, const std::function<void()>& Printer) {
	
	SetTerminalColor(TerminalColorRed);
	OS << "Error: ";
	SetTerminalColor(TerminalColorDefault);

	// Printing the message
	Printer();

	OS << '\n';

	FoundCompileError = true;
}
