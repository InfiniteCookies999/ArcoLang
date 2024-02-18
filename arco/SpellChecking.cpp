#include "SpellChecking.h"

// Computes the levenstein distance.
#include "Prelude.h"

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

const char* arco::FindClosestSpellingMatch(
    const llvm::SmallVector<std::string>& Comparisons,
    const std::string& FindFor) {
    
    if (Comparisons.empty()) return nullptr;

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
		return nullptr;
	}
	
    return Comparisons[SelectedIdx].c_str();
}
