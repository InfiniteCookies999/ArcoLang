#ifndef ARCO_FUZZ_UTILS_H
#define ARCO_FUZZ_UTILS_H

#include <string>
#include <fstream>
#include <llvm/ADT/SmallVector.h>
#include <Context.h>

#include <Prelude.h>

std::string GenRandomIdentLiteral();

std::string GenRandomIntLiteral(int Base, int Limit = std::numeric_limits<int>::max());

std::string GenRandomCharLiteral();

std::string GenRandomStringLiteral();

std::string GenRandomFloatLiteral();

void WriteTokensToFile(const llvm::SmallVector<u16>& Tokens, std::ofstream& FileStream, arco::ArcoContext& Context);

#endif // ARCO_FUZZ_UTILS_H