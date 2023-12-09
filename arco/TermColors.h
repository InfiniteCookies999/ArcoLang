#ifndef ARCO_TERM_COLORS_H
#define ARCO_TERM_COLORS_H

#include "Prelude.h"

namespace arco {

	constexpr u32 TerminalColorDefault      = 0x7;
	constexpr u32 TerminalColorBlack        = 0x0;
	constexpr u32 TerminalColorDarkBlue     = 0x1;
	constexpr u32 TerminalColorDarkGreen    = 0x2;
	constexpr u32 TerminalColorBrightBlue   = 0x3;
	constexpr u32 TerminalColorDarkRed      = 0x4;
	constexpr u32 TerminalColorFadedRed     = 0x5;
	constexpr u32 TerminalColorYellow       = 0x6;
	constexpr u32 TerminalColorDarkYellow   = 0x8;
	constexpr u32 TerminalColorBlue         = 0x9;
	constexpr u32 TerminalColorBrightGreen  = 0xA;
	constexpr u32 TerminalColorCyan         = 0xB;
	constexpr u32 TerminalColorRed          = 0xC;
	constexpr u32 TerminalColorMagenta      = 0xD;
	constexpr u32 TerminalColorBrightYellow = 0xE;
	constexpr u32 TerminalColorWhite        = 0xF;

	extern bool DisableTerminalColors;

	void SetTerminalColor(u32 ColorCode);

}

#endif // ARCO_TERM_COLORS_H