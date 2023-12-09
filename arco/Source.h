#ifndef ARCO_SOURCE_H
#define ARCO_SOURCE_H

#include "Prelude.h"

#include <llvm/ADT/StringRef.h>

namespace arco {

	/// Identifiers a location within the
	/// source buffer of a .arco file.
	/// 
	/// The use of an llvm::StringRef allows
	/// for pointing to a character location in the
	/// buffer along with the length of the text
	/// equivalent to that of a token's lexeme.
	///
	struct SourceLoc {
		llvm::StringRef Text;
		ulen            LineNumber;
	};

	/// Source buffer which points to the source
	/// code of a .arco file.
	/// 
	struct SourceBuf {
		char* memory;
		ulen  length;
	};

	inline bool IsDigit(char Char) {
		return Char >= 48 && Char <= 57;
	}

	inline bool IsAlpha(char Char) {
		return (Char >= 65 && Char <= 90) ||
			   (Char >= 97 && Char <= 122);
	}

	// TODO: LUT may be quicker
	inline bool IsHex(char Char) {
		return (Char >= 48 && Char <= 57) ||
			   (Char >= 65 && Char <= 90) ||
			   (Char >= 97 && Char <= 122);
	}

	constexpr char NUMBER_SEPERATOR = '_';
}

#endif // ARCO_SOURCE_H