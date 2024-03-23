#ifndef ARCO_SPELL_CHECKING_H
#define ARCO_SPELL_CHECKING_H

#include <llvm/ADT/SmallVector.h>
#include <llvm/ADT/DenseMap.h>
#include <llvm/ADT/StringMap.h>

#include "Identifier.h"

namespace arco {

    struct Module;
    class Logger;

    std::string FindClosestSpellingMatch(
        const llvm::SmallVector<std::string, 64>& Comparisons,
		const std::string& FindFor
    );

    // TODO: Should this limit the search count?
    class ErrorSpellChecker {
    public:

        template<typename T>
        void AddSearches(const llvm::DenseMap<Identifier, T>& SearchMap) {
            for (auto [Name, _] : SearchMap) {
                AllSearches.push_back(Name.Text.str());
            }
        }

        void AddSearches(const llvm::StringMap<Module*>& Modules);

        void AddSearches(llvm::SmallVector<std::string, 64> Searches);

        bool Search(Logger& Log, Identifier SearchIdent);

        bool SearchAndEndError(Logger& Log, Identifier SearchIdent);

    private:
        llvm::SmallVector<std::string, 64> AllSearches;
    };

}

#endif // ARCO_SPELL_CHECKING_H