# Arco programming language

This repo contains the compiler and standard library source code for the arco language.

All source code is compiled into machine code using LLVM compiler framework.

```
import std;

fn main() {
    std.println("Hello from arco!");
}
```

## Installation

This project is currently only for windows machines.

For windows you must have CMake installed and LLVM installed on the machine for CMake to link to the libraries. See LLVM for further information: https://llvm.org

Must set ``ArcoStdLibPath`` environment variable to point to the standard library directory in order to use the standard library of the language.
