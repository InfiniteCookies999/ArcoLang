#ifndef ARCO_FILES_H
#define ARCO_FILES_H

#include "Prelude.h"
#include <string>

namespace arco {

    bool CreateDirectories(const std::string& Dir, const char* Type);

    // TODO: This should probably be using ulen not u64.
    bool ReadFile(const std::string& Path, char*& Buffer, ulen& Size);

}

#endif // ARCO_FILES_H
