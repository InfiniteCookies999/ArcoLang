#ifndef ARCO_FUZZ_UTILS_H
#define ARCO_FUZZ_UTILS_H

#include <string>

std::string GenRandomIdentLiteral();

std::string GenRandomIntLiteral(int Base, int Limit = std::numeric_limits<int>::max());

std::string GenRandomCharLiteral();

std::string GenRandomStringLiteral();

std::string GenRandomFloatLiteral();

#endif // ARCO_FUZZ_UTILS_H