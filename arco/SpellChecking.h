#ifndef ARCO_SPELL_CHECKING_H
#define ARCO_SPELL_CHECKING_H

#include <llvm/ADT/SmallVector.h>

namespace arco {

    const char* FindClosestSpellingMatch(
        const llvm::SmallVector<std::string>& Comparisons,
		const std::string& FindFor
    );

}

#endif // ARCO_SPELL_CHECKING_H