#ifndef ARCO_IDENTIFIER_H
#define ARCO_IDENTIFIER_H

#include "Prelude.h"

#include <llvm/ADT/StringRef.h>
#include <llvm/ADT/DenseMapInfo.h>
#include <llvm/Support/raw_ostream.h>

namespace arco {
	struct Identifier {
		llvm::StringRef Text;
		ulen            ID;

		explicit Identifier(llvm::StringRef Text);

		Identifier() : ID(0) {}
		
		inline bool IsNull() const { return ID == 0; }

		bool operator==(const Identifier& RHS) const { return ID == RHS.ID; }
		bool operator!=(const Identifier& RHS) const { return ID != RHS.ID; }

	};

}


namespace llvm {

	raw_ostream& operator<<(raw_ostream& OS, const arco::Identifier& Ident);

	// Defining key information for the eris::Identifier so
	// it may be used as a key which relies on the unique
	// ID of the eris::Identifier.
	template<> struct DenseMapInfo<arco::Identifier> {
		static bool isEqual(const arco::Identifier& RHS, const arco::Identifier& LHS) {
			return LHS == RHS;
		}
		static arco::Identifier getTombstoneKey() {
			// Do not remove identifiers from maps.
			return arco::Identifier();
		}
		static arco::Identifier getEmptyKey() {
			return arco::Identifier();
		}
		static ulen getHashValue(const arco::Identifier& Val) {
			return Val.ID;
		}
	};
}

#endif // ARCO_IDENTIFIER_H