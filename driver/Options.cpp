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

bool OptionManager::ProcessOption(int ArgNum, llvm::StringRef Opt) {
    for (const Option& Option : Options) {
        if (Option.OnlyStartsWith) {
            if (Opt.startswith(Option.OptName)) {
                RunOption(ArgNum, Option, Opt);
                return true;
            }
        } else {
            if (Opt == Option.OptName) {
                RunOption(ArgNum, Option, Opt);
                return true;
            }
        }
    }
    return false;
}

void OptionManager::RunOption(int ArgNum, const Option& Option, llvm::StringRef Opt) {
    if (Option.State) {
        *Option.State = !(*Option.State);
    } else {
        Option.Callback(ArgNum, Opt.substr(strlen(Option.OptName)));
    }
}
