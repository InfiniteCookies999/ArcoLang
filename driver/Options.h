#ifndef ARCO_OPTIONS_H
#define ARCO_OPTIONS_H

#include <llvm/ADT/StringRef.h>
#include <functional>
#include <vector>

class OptionManager {
public:

	using OptCB = std::function<void(llvm::StringRef)>;

	void AddOption(const char* OptName, bool* State);

	void AddOption(const char* OptName, const OptCB& Callback, bool OnlyStartsWith = true);

	bool ProcessOption(llvm::StringRef Opt);

private:

	struct Option {
		const char* OptName;
		bool*       State = nullptr;
		OptCB       Callback;
		bool        OnlyStartsWith = false;
	};
	
	std::vector<Option> Options;

	void RunOption(const Option& Option, llvm::StringRef Opt);


};

#endif // ARCO_OPTIONS_H