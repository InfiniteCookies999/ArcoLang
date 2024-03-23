#include "SpellChecking.h"

// Computes the levenstein distance.
#include "Prelude.h"
#include "Logger.h"
#include "TermColors.h"

#include "AST.h"

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

bool arco::ErrorSpellChecker::Search(Logger& Log, Identifier SearchIdent) {
    std::string Found = FindClosestSpellingMatch(AllSearches, SearchIdent.Text.str());
    if (!Found.empty()) {
        std::string DidYouMeanStr = "Did you mean '";
        std::string InsteadOfStr = "' instead of '";
        Log.AddNoteLine([=](auto& OS) {
            OS << DidYouMeanStr << Found << InsteadOfStr << SearchIdent << "'?";
            });
        Log.AddNoteLine([=](auto& OS) {
            OS << std::string(DidYouMeanStr.size() + 1, ' ');
            SetTerminalColor(TerminalColorBrightGreen);

            auto GetResolveState = [](const std::string& S1,
                                      const std::string& S2, 
                                      ulen i1, ulen i2,
                                      char C1, char C2,
                                      ulen& MissFixGood, ulen& AddFixGood,
                                      ulen& i1Miss, ulen& i2Added
                ) {
                i1Miss = i1 + 1;
                MissFixGood = 0;
                while (i1Miss < S1.size()) {
                    const char NextC1 = S1[i1Miss];
                    if (NextC1 == C2) {
                        MissFixGood = 1;
                        ulen i22 = i2 + 1;
                        ulen i11 = i1Miss + 1;
                        while (i11 < S1.size() && i22 < S2.size()) {
                            if (S1[i11] != S2[i22]) {
                                break;
                            }
                            ++MissFixGood;
                            ++i11;
                            ++i22;
                        }
                        break;
                    }
                    ++i1Miss;
                }
                i2Added = i2 + 1;
                AddFixGood = 0;
                while (i2Added < S2.size()) {
                    const char NextC2 = S2[i2Added];
                    if (NextC2 == C1) {
                        AddFixGood = 1;
                        ulen i11 = i1 + 1;
                        ulen i22 = i2Added + 1;
                        while (i22 < S2.size() && i11 < S1.size()) {
                            if (S1[i11] != S2[i22]) {
                                break;
                            }
                            ++AddFixGood;
                            ++i22;
                            ++i11;
                        }
                        break;
                    }
                    ++i2Added;
                }
            };

            auto DiffStr = [GetResolveState]
                             (const std::string& S1,
                              const std::string& S2,
                              const char C) -> std::string {
                    std::string Result = "";
                    for (auto [i1, i2] = std::pair{ 0, 0 }; i1 < S1.size();) {
                        if (i2 >= S2.size()) {
                            Result += std::string(S1.size() - i2, C);
                            break;
                        }

                        const char C1 = S1[i1];
                        const char C2 = S2[i2];
                        if (C1 == C2) {
                            Result += ' ';
                        } else {
                            if (C == '+') {
                                // Try to get back to a state where things match.
                                ulen MissFixGood = 0, AddFixGood = 0;
                                ulen i1Miss      = 0, i2Added    = 0;
                                GetResolveState(S1, S2,
                                    i1, i2,
                                    C1, C2,
                                    MissFixGood, AddFixGood,
                                    i1Miss, i2Added
                                );
                                
                                if (AddFixGood || MissFixGood) {
                                    if (MissFixGood > AddFixGood) {
                                        ulen Count = i1Miss - i1;
                                        Result += std::string(Count, '+');
                                        i1 = i1Miss;
                                    } else {
                                        ulen Count = AddFixGood;
                                        Result += std::string(Count, ' ');
                                        i1 += Count; // Skip over the added amount.
                                    }
                                } else {
                                    Result += C;
                                }

                            } else {
                                // Try to get back to a state where things match.
                                ulen MissFixGood = 0, AddFixGood = 0;
                                ulen i1Miss      = 0, i2Added    = 0;
                                GetResolveState(S1, S2,
                                    i1, i2,
                                    C1, C2,
                                    MissFixGood, AddFixGood,
                                    i1Miss, i2Added
                                );

                                if (AddFixGood || MissFixGood) {
                                    if (MissFixGood > AddFixGood) {
                                        ulen Count = i1Miss - i1;
                                        Result += std::string(Count, '-');
                                        i1 = i1Miss;
                                    } else {
                                        ulen Count = AddFixGood;
                                        Result += std::string(Count, ' ');
                                        i1 += Count; // Skip over the added amount.
                                    }
                                } else {
                                    Result += C;
                                }
                            }

                        }
                        ++i1;
                        ++i2;
                    }
                    return Result;
            };

            std::string SearchStr = SearchIdent.Text.str();
            std::string Pluses  = DiffStr(Found, SearchStr, '+');
            std::string Minuses = DiffStr(SearchStr, Found, '-');
            
            OS << Pluses;
            SetTerminalColor(TerminalColorRed);
            OS << std::string(InsteadOfStr.size(), ' ');
            OS << Minuses;
            SetTerminalColor(TerminalColorDefault);
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
