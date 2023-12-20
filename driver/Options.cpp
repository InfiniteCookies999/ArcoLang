#include "Options.h"
#include "Options.h"

void OptionManager::AddOption(const char* OptName, bool* State) {
    Option Opt;
    Opt.OptName = OptName;
    Opt.State   = State;
    Options.push_back(Opt);
}

void OptionManager::AddOption(const char* OptName, const OptCB& Callback, bool OnlyStartsWith) {
    Option Opt;
    Opt.OptName        = OptName;
    Opt.Callback       = Callback;
    Opt.OnlyStartsWith = OnlyStartsWith;
    Options.push_back(Opt);
}

bool OptionManager::ProcessOption(llvm::StringRef Opt) {
    for (const Option& Option : Options) {
        if (Option.OnlyStartsWith) {
            if (Opt.startswith(Option.OptName)) {
                RunOption(Option, Opt);
                return true;
            }
        } else {
            if (Opt == Option.OptName) {
                RunOption(Option, Opt);
                return true;
            }
        }
    }
    return false;
}

void OptionManager::RunOption(const Option& Option, llvm::StringRef Opt) {
    if (Option.State) {
        *Option.State = !(*Option.State);
    } else {
        Option.Callback(Opt.substr(strlen(Option.OptName)));
    }
}
