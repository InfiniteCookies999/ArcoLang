#ifndef ARCO_OPTIONS_H
#define ARCO_OPTIONS_H

#include <llvm/ADT/StringRef.h>
#include <llvm/ADT/SmallVector.h>
#include <functional>
#include <vector>

class OptionManager {
public:

	using OptCB = std::function<void(int, llvm::StringRef)>;

	void AddOption(const char* OptName, bool* State);

	void AddOption(const char* OptName, const OptCB& Callback, bool OnlyStartsWith = true);

	bool ProcessOption(int ArgNum, llvm::StringRef Opt);

	llvm::SmallVector<std::string> GetAllOptionsAsStrings();

private:

	struct Option {
		const char* OptName;
		size_t      OptNameLength;
		bool*       State = nullptr;
		OptCB       Callback;
		bool        OnlyStartsWith = false;
	};
	
	std::vector<Option> Options;

	void RunOption(int ArgNum, const Option& Option, llvm::StringRef Opt);


};

#endif // ARCO_OPTIONS_H