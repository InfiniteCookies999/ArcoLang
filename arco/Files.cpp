#include "Files.h"

#include "Logger.h"

#include <fstream>
#include <filesystem>

bool arco::CreateDirectories(const std::string& Dir, const char* Type) {
    std::error_code EC;
    if (!std::filesystem::exists(Dir, EC)) {
        if (!std::filesystem::create_directories(Dir, EC) || EC) {
            arco::Logger::GlobalError(llvm::errs(), "Failed to create the %s directory: '%s'",
                Dir);
            return false;
        }
    } else if (EC) {
        arco::Logger::GlobalError(llvm::errs(), "Failed to check if the %s directory exists");
        return false;
    }
    return true;
}

bool arco::ReadFile(const std::string& Path, char*& Buffer, ulen& Size) {
    std::ifstream Stream(Path, std::ios::binary | std::ios::in);
    if (!Stream.good()) {
        return false;
    }
    Stream.seekg(0, std::ios::end);
    Size = Stream.tellg();
    Buffer = new char[Size + 1];
    Stream.seekg(0, std::ios::beg);
    Stream.read(Buffer, Size);
    Buffer[Size] = 0; // Null termination
    return true;
}
