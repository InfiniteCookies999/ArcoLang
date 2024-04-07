#include "SpellChecking.h"

#include "Prelude.h"
#include "Logger.h"
#include "TermColors.h"
#include "AST.h"

// Computes the levenstein distance.
ulen LevRow(const std::string& S1, const std::string& S2) {

    const ulen m = S1.size(), n = S2.size();

    if (m == 0) return n;
    if (n == 0) return m;

    std::vector<ulen> Costs;
    Costs.resize(S2.size() + 1);

    for (ulen k = 0; k <= n; ++k) {
        Costs[k] = k;
    }

    for (ulen i = 0; i < S1.size(); ++i) {
        const char c1 = S1[i];

        Costs[0] = i + 1;
        ulen Corner = i;

        for (ulen j = 0; j < S2.size(); ++j) {
            const char c2 = S2[j];

            ulen Upper = Costs[j + 1];
            if (c1 == c2) {
                Costs[j + 1] = Corner;
            } else {
                // min(Upper, Corner, Left) + 1
                ulen t = Upper < Corner ? Upper : Corner;
                Costs[j + 1] = (Costs[j] < t ? Costs[j] : t) + 1;
            }

            Corner = Upper;
        }
    }

    return Costs[n];
}

std::string arco::FindClosestSpellingMatch(
    const llvm::SmallVector<std::string, 64>& Comparisons,
    const std::string& FindFor) {
    
    if (Comparisons.empty()) return "";

    ulen SmallestCost = std::numeric_limits<ulen>::max();
    ulen SelectedIdx = 0, Idx = 0;

    for (const std::string& Comparison : Comparisons) {
        ulen Dist = LevRow(FindFor, Comparison);
        if (Dist < SmallestCost) {
            SmallestCost = Dist;
            SelectedIdx = Idx;
        }
        ++Idx;
    }

    if (SmallestCost > FindFor.size() / 2) {
        return "";
    }
    
    return Comparisons[SelectedIdx];
}

void arco::ErrorSpellChecker::AddSearches(const llvm::StringMap<Module*>& Modules) {
    auto Itr = Modules.begin();
    while (Itr != Modules.end()) {
        AllSearches.push_back(Itr->first().str());
        ++Itr;
    }
}

void arco::ErrorSpellChecker::AddSearches(llvm::SmallVector<std::string, 64> Searches) {
    AllSearches.insert(AllSearches.begin(), Searches.begin(), Searches.end());
}

namespace arco {

static std::string DidYouMeanStr = "Did you mean '";
static std::string InsteadOfStr = "' instead of '";


void AddNoteLineUnderscoring(llvm::raw_ostream& OS,
                             const std::string& Search,
                             const std::string& Found) {

    OS << std::string(DidYouMeanStr.size() + 1, ' ');
    
    llvm::SmallVector<llvm::SmallVector<ulen>> Table;
    for (ulen i = 0; i < Search.size() + 1; i++) {
        Table.push_back(llvm::SmallVector<ulen>(Found.size() + 1, 0));
    }

    for (ulen i = 0; i <= Search.size(); i++) {
        Table[i][Found.size()] = Search.size() - i;
    }
    for (ulen i = 0; i <= Found.size(); i++) {
        Table[Search.size()][i] = Found.size() - i;
    }

    for (long long i = Search.size() - 1; i >= 0; --i) {
        for (long long j = Found.size() - 1; j >= 0; --j) {
            ulen Right  = Table[i][j + 1];
            ulen Down   = Table[i + 1][j];
            ulen Corner = Table[i + 1][j + 1];
            bool Match = Search[i] == Found[j];
            ulen Value = Match ? 0 : 1;

            if (Right < Down && Right < Corner) {
                Table[i][j] = Value + Right;
            } else if (Down < Right && Down < Corner) {
                Table[i][j] = Value + Down;
            } else {
                Table[i][j] = Value + Corner;
            }
        }
    }

    std::string Pluses, Minuses;
    ulen i = 0, j = 0;
    while (i < Search.size() && j < Found.size()) {
        if (Search[i] == Found[j]) {
            // Nothing to do they match!
            Pluses  += " ";
            Minuses += " ";
            ++i;
            ++j;
        } else {
            ulen Right  = Table[i][j + 1];
            ulen Down   = Table[i + 1][j];
            ulen Corner = Table[i + 1][j + 1];
            if (Right < Down && Right < Corner) {
                // Add
                SetTerminalColor(TerminalColorBrightGreen);
                Pluses += "+";
                ++j;
            } else if (Down < Right && Down < Corner) {
                // Delete
                SetTerminalColor(TerminalColorRed);
                Minuses += "-";
                ++i;
            } else {
                // Replace
                SetTerminalColor(TerminalColorRed);
                Minuses += "-";
                Pluses  += "+";
                ++i;
                ++j;
            }
        }
    }
    ulen DelTrail = Search.size() - i;
    ulen AddTrail = Found.size() - j;

    Pluses  += std::string(AddTrail, '+');
    Minuses += std::string(DelTrail, '-');

    SetTerminalColor(TerminalColorBrightGreen);
    OS << Pluses;
    OS << std::string(InsteadOfStr.size(), ' ');
    SetTerminalColor(TerminalColorRed);
    OS << Minuses;
    SetTerminalColor(TerminalColorDefault);

}
}

bool arco::ErrorSpellChecker::Search(Logger& Log, Identifier SearchIdent) {
    std::string Search = SearchIdent.Text.str();
    std::string Found = FindClosestSpellingMatch(AllSearches, Search);
    if (!Found.empty()) {
        Log.AddNoteLine([=](auto& OS) {
            OS << DidYouMeanStr << Found << InsteadOfStr << SearchIdent << "'?";
            });
        Log.AddNoteLine([=](auto& OS) {
            AddNoteLineUnderscoring(OS, Search, Found);
        });
    }

    AllSearches.clear();
    return !Found.empty();
}

bool arco::ErrorSpellChecker::SearchAndEndError(Logger& Log, Identifier SearchIdent) {
    bool Found = Search(Log, SearchIdent);
    Log.EndError();
    return Found;
}
