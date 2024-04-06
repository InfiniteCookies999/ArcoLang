#include "Options.h"
#include "Options.h"

void OptionManager::AddOption(const char* OptName, bool* State) {
    Option Opt;
    Opt.OptName       = OptName;
    Opt.OptNameLength = strlen(OptName);
    Opt.State         = State;
    Options.push_back(Opt);
}

void OptionManager::AddOption(const char* OptName, const OptCB& Callback, bool OnlyStartsWith) {
    Option Opt;
    Opt.OptName        = OptName;
    Opt.OptNameLength  = strlen(OptName);
    Opt.Callback       = Callback;
    Opt.OnlyStartsWith = OnlyStartsWith;
    Options.push_back(Opt);
}

bool OptionManager::ProcessOption(int ArgNum, llvm::StringRef Opt) {
    const Option* BestOptionMatch = nullptr;
    for (const Option& Option : Options) {
        if (Option.OnlyStartsWith) {
            if (Opt.startswith(Option.OptName)) {
                if (!BestOptionMatch) {
                    BestOptionMatch = &Option;
                } else if (BestOptionMatch->OptNameLength < Option.OptNameLength) {
                    BestOptionMatch = &Option;
                }
            }
        } else {
            if (Opt == Option.OptName) {
                BestOptionMatch = &Option;
                break;
            }
        }
    }
    if (!BestOptionMatch) {
        return false;
    }

    RunOption(ArgNum, *BestOptionMatch, Opt);
    return true;
}

llvm::SmallVector<std::string, 64> OptionManager::GetAllOptionsAsStrings() {
    llvm::SmallVector<std::string> AllOptStrings;
    AllOptStrings.reserve(Options.size());
    for (const Option& Option : Options) {
        AllOptStrings.push_back(Option.OptName);
    }
    return AllOptStrings;
}

void OptionManager::RunOption(int ArgNum, const Option& Option, llvm::StringRef Opt) {
    if (Option.State) {
        *Option.State = !(*Option.State);
    } else {
        Option.Callback(ArgNum, Opt.substr(strlen(Option.OptName)));
    }
}
