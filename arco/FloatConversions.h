#ifndef ARCO_FLOAT_CONVERSIONS_H
#define ARCO_FLOAT_CONVERSIONS_H

#include <llvm/ADT/StringRef.h>

#include "Prelude.h"

namespace arco {
namespace FD {

	// This structure requires infinite precision since
	// radix 10 digits no matter how insignificant may end
	// up with a slightly different but closer approximation
	// in radix 2.
	struct BigIntFD {
	
		u32*  Blocks = nullptr;
		ulen Length;

		~BigIntFD();

		// copy
		BigIntFD(const BigIntFD& o);
		// move
		BigIntFD(BigIntFD&& o) noexcept;

		BigIntFD() : Length(0), Blocks(nullptr) { }

		BigIntFD(u32 B1, u32 B2, ulen Offset);

		BigIntFD(u32 B1, u32 B2, u32 B3, ulen Offset);

		BigIntFD& operator=(const BigIntFD& o);
		BigIntFD& operator=(BigIntFD&& o) noexcept;

		void MultiplyAndAdd(u32 Mult, u32 Add);

		BigIntFD Multiply(BigIntFD& o);
		BigIntFD Multiply(u32 Constant);
		BigIntFD Multiply(u32 Constant1, u32 Constant2);

		BigIntFD MultiplyPow5(i64 P5);

		BigIntFD LeftShift(i64 Shift);
			
		BigIntFD Subtract(BigIntFD& Sub);

		void TrimLeadingZeros();

		i64 Compare(BigIntFD& o);

	};

	enum class FloatParseError {
		OVERFLOWED,
		UNDERFLOWED,
		NONE
	};

	// !! This is required to be called before using
	// ToIEEEDouble and ToIEEESingle functions.
	void InitializeCache();

	// Requires the text to be properly formatted
	// before calling.
	double ToIEEEDouble(llvm::StringRef Text, FloatParseError& Error);

	// Requires the text to be properly formatted
	// before calling.
	float ToIEEESingle(llvm::StringRef Text, FloatParseError& Error);

}
}

#endif // ARCO_FLOAT_CONVERSIONS_H