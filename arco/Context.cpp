#include "Context.h"

#include <llvm/IR/Module.h>
#include <llvm/IR/Intrinsics.h>

arco::ArcoContext::ArcoContext()
    :
    LLContext(*new llvm::LLVMContext),
    LLArcoModule(*new llvm::Module("Arco Module", LLContext)),

    MainIdentifier(Identifier("main")),
    LengthIdentifier(Identifier("length")),
    BufferIdentifier(Identifier("buffer")),
    StdcallIdentifier(Identifier("stdcall")),
    CdeclIdentifier(Identifier("cdecl")),
    FastcallIdentifier(Identifier("fastcall")),
    ForcesRaiseIdentifier(Identifier("FORCES_RAISE")),
    StringIdentifier(Identifier("String")),
    AnyIdentifier(Identifier("Any")),
    TypeIdentifier(Identifier("Type")),
    TypeIdIdentifier(Identifier("TypeId")),
    ArrayTypeIdentifier(Identifier("ArrayType")),
    StructTypeIdentifier(Identifier("Struct")),
    FieldTypeIdentifier(Identifier("Field")),
    EnumTypeIdentifier(Identifier("Enum")),
    ErrorInterfaceIdentifier(Identifier("Error")),

    CallConventions({
        { StdcallIdentifier,  llvm::CallingConv::X86_StdCall  },
        { CdeclIdentifier,	  llvm::CallingConv::C            },
        { FastcallIdentifier, llvm::CallingConv::X86_FastCall }
        }),

    IntType(new Type(TypeKind::Int, UniqueTypeIdCounter++)),
    PtrsizeType(new Type(TypeKind::Ptrsize, UniqueTypeIdCounter++)),
    Int8Type(new Type(TypeKind::Int8, UniqueTypeIdCounter++)),
    Int16Type(new Type(TypeKind::Int16, UniqueTypeIdCounter++)),
    Int32Type(new Type(TypeKind::Int32, UniqueTypeIdCounter++)),
    Int64Type(new Type(TypeKind::Int64, UniqueTypeIdCounter++)),
    UInt8Type(new Type(TypeKind::UInt8, UniqueTypeIdCounter++)),
    UInt16Type(new Type(TypeKind::UInt16, UniqueTypeIdCounter++)),
    UInt32Type(new Type(TypeKind::UInt32, UniqueTypeIdCounter++)),
    UInt64Type(new Type(TypeKind::UInt64, UniqueTypeIdCounter++)),
    Float32Type(new Type(TypeKind::Float32, UniqueTypeIdCounter++)),
    Float64Type(new Type(TypeKind::Float64, UniqueTypeIdCounter++)),
    CharType(new Type(TypeKind::Char, UniqueTypeIdCounter++)),
    VoidType(new Type(TypeKind::Void, UniqueTypeIdCounter++)),
    BoolType(new Type(TypeKind::Bool, UniqueTypeIdCounter++)),
    CStrType(new Type(TypeKind::CStr, UniqueTypeIdCounter++)),
    NullType(new Type(TypeKind::Null, UniqueTypeIdCounter++)),
    ErrorType(new Type(TypeKind::Error, UniqueTypeIdCounter++)),
    EmptyArrayElmType(new Type(TypeKind::EmptyArrayElm, UniqueTypeIdCounter++)),
    ImportType(new Type(TypeKind::Import, UniqueTypeIdCounter++)),
    FuncRefType(new Type(TypeKind::FuncRef, UniqueTypeIdCounter++)),
    StructRefType(new Type(TypeKind::StructRef, UniqueTypeIdCounter++)),
    EnumRefType(new Type(TypeKind::EnumRef, UniqueTypeIdCounter++)),
    InterfaceRefType(new Type(TypeKind::InterfaceRef, UniqueTypeIdCounter++)),

    LLVMIntrinsicsTable({
        { Identifier("memcpy"), llvm::Intrinsic::IndependentIntrinsics::memcpy },
        { Identifier("memset"), llvm::Intrinsic::IndependentIntrinsics::memset },
        { Identifier("floor") , llvm::Intrinsic::IndependentIntrinsics::floor  },
        { Identifier("ceil")  , llvm::Intrinsic::IndependentIntrinsics::ceil   },
        { Identifier("pow")   , llvm::Intrinsic::IndependentIntrinsics::pow    },
        { Identifier("log")   , llvm::Intrinsic::IndependentIntrinsics::log    },
        { Identifier("log10") , llvm::Intrinsic::IndependentIntrinsics::log10  },
        { Identifier("sqrt")  , llvm::Intrinsic::IndependentIntrinsics::sqrt   },
        { Identifier("sin")   , llvm::Intrinsic::IndependentIntrinsics::sin    },
        { Identifier("cos")   , llvm::Intrinsic::IndependentIntrinsics::cos    },
        }),

    BinaryOpsPrecedence({
        
        { '*', 9 },
        { '/', 9 },
        { '%', 9 },

        { '+', 8 },
        { '-', 8 },

        { TokenKind::LT_LT, 7 }, // <<
        { TokenKind::GT_GT, 7 }, // >>
        
        { '<'      , 6 },
        { '>'      , 6 },
        { TokenKind::LT_EQ, 6 }, // <=
        { TokenKind::GT_EQ, 6 }, // >=
        
        { TokenKind::EQ_EQ , 5 }, // ==
        { TokenKind::EXL_EQ, 5 }, // !=
        
        { '&', 4 },
        
        { '^', 3 },
        
        { '|', 2 },
        
        { TokenKind::AMP_AMP, 1 }, // &&
        { TokenKind::BAR_BAR, 1 }, // ||

        })
{
}

arco::ArcoContext::~ArcoContext() {
    delete &LLArcoModule;
    delete &LLContext;

    delete IntType;
    delete PtrsizeType;
    delete Int8Type;
    delete Int16Type;
    delete Int32Type;
    delete Int64Type;
    delete UInt8Type;
    delete UInt16Type;
    delete UInt32Type;
    delete UInt64Type;
    delete Float32Type;
    delete Float64Type;
    delete CharType;
    delete VoidType;
    delete BoolType;
    delete CStrType;
    delete NullType;
    delete ErrorType;
    delete EmptyArrayElmType;
    delete ImportType;
    delete FuncRefType;
    delete StructRefType;
    delete EnumRefType;
    delete InterfaceRefType;
    // TODO: Cleanup other types.

}

void arco::ArcoContext::Initialize() {
    
    TokenKeywordMap.insert({ "int"      , TokenKind::KW_INT       });
    TokenKeywordMap.insert({ "ptrsize"  , TokenKind::KW_PTRSIZE   });
    TokenKeywordMap.insert({ "int8"     , TokenKind::KW_INT8      });
    TokenKeywordMap.insert({ "int16"    , TokenKind::KW_INT16     });
    TokenKeywordMap.insert({ "int32"    , TokenKind::KW_INT32     });
    TokenKeywordMap.insert({ "int64"    , TokenKind::KW_INT64     });
    TokenKeywordMap.insert({ "uint8"    , TokenKind::KW_UINT8     });
    TokenKeywordMap.insert({ "uint16"   , TokenKind::KW_UINT16    });
    TokenKeywordMap.insert({ "uint32"   , TokenKind::KW_UINT32    });
    TokenKeywordMap.insert({ "uint64"   , TokenKind::KW_UINT64    });
    TokenKeywordMap.insert({ "float32"  , TokenKind::KW_FLOAT32   });
    TokenKeywordMap.insert({ "float64"  , TokenKind::KW_FLOAT64   });
    TokenKeywordMap.insert({ "char"     , TokenKind::KW_CHAR      });
    TokenKeywordMap.insert({ "void"     , TokenKind::KW_VOID      });
    TokenKeywordMap.insert({ "cstr"     , TokenKind::KW_CSTR      });
    TokenKeywordMap.insert({ "bool"     , TokenKind::KW_BOOL      });
    TokenKeywordMap.insert({ "true"     , TokenKind::KW_TRUE      });
    TokenKeywordMap.insert({ "false"    , TokenKind::KW_FALSE     });
    TokenKeywordMap.insert({ "new"      , TokenKind::KW_NEW       });
    TokenKeywordMap.insert({ "this"     , TokenKind::KW_THIS      });
    TokenKeywordMap.insert({ "import"   , TokenKind::KW_IMPORT    });
    TokenKeywordMap.insert({ "namespace", TokenKind::KW_NAMESPACE });
    TokenKeywordMap.insert({ "static"   , TokenKind::KW_STATIC    });
    TokenKeywordMap.insert({ "delete"   , TokenKind::KW_DELETE    });
    TokenKeywordMap.insert({ "sizeof"   , TokenKind::KW_SIZEOF    });
    TokenKeywordMap.insert({ "typeof"   , TokenKind::KW_TYPEOF    });
    TokenKeywordMap.insert({ "typeid"   , TokenKind::KW_TYPEID    });
    TokenKeywordMap.insert({ "copyobj"  , TokenKind::KW_COPYOBJ   });
    TokenKeywordMap.insert({ "moveobj"  , TokenKind::KW_MOVEOBJ   });
    TokenKeywordMap.insert({ "initobj"  , TokenKind::KW_INITOBJ   });
    TokenKeywordMap.insert({ "null"     , TokenKind::KW_NULL      });
    TokenKeywordMap.insert({ "try"      , TokenKind::KW_TRY       });
    TokenKeywordMap.insert({ "fn"       , TokenKind::KW_FN        });
    TokenKeywordMap.insert({ "struct"   , TokenKind::KW_STRUCT    });
    TokenKeywordMap.insert({ "enum"     , TokenKind::KW_ENUM      });
    TokenKeywordMap.insert({ "interface", TokenKind::KW_INTERFACE });
    TokenKeywordMap.insert({ "raise"    , TokenKind::KW_RAISE     });
    TokenKeywordMap.insert({ "raises"   , TokenKind::KW_RAISES    });
    TokenKeywordMap.insert({ "cast"     , TokenKind::KW_CAST      });
    TokenKeywordMap.insert({ "bitcast"  , TokenKind::KW_BITCAST   });
    TokenKeywordMap.insert({ "generics" , TokenKind::KW_GENERICS  });
    TokenKeywordMap.insert({ "loop"     , TokenKind::KW_LOOP      });
    TokenKeywordMap.insert({ "break"    , TokenKind::KW_BREAK     });
    TokenKeywordMap.insert({ "continue" , TokenKind::KW_CONTINUE  });
    TokenKeywordMap.insert({ "if"       , TokenKind::KW_IF        });
    TokenKeywordMap.insert({ "else"     , TokenKind::KW_ELSE      });
    TokenKeywordMap.insert({ "return"   , TokenKind::KW_RETURN    });
    TokenKeywordMap.insert({ "native"   , TokenKind::KW_NATIVE    });
    TokenKeywordMap.insert({ "const"    , TokenKind::KW_CONST     });
    TokenKeywordMap.insert({ "private"  , TokenKind::KW_PRIVATE   });
    TokenKeywordMap.insert({ "readonly" , TokenKind::KW_READONLY  });
    TokenKeywordMap.insert({ "writeonly", TokenKind::KW_WRITEONLY });
    TokenKeywordMap.insert({ "dllimport", TokenKind::KW_DLLIMPORT });
    TokenKeywordMap.insert({ "linkname" , TokenKind::KW_LINKNAME  });
    TokenKeywordMap.insert({ "catch"    , TokenKind::KW_CATCH     });


    for (const auto& [Text, Kind] : TokenKeywordMap) {
        TokenKeywordInvertedMap.insert({ static_cast<u32>(Kind), Text });
    }

    VoidPtrType    = PointerType::Create(VoidType, *this);
    CharPtrType    = PointerType::Create(CharType, *this);
    CharPtrPtrType = PointerType::Create(CharPtrType, *this);

    LLVMValidIntrinsicArgs = {
        { Identifier("memcpy"), { VoidPtrType, VoidPtrType, IntType }, VoidType },
        { Identifier("memset"), { VoidPtrType, Int8Type, IntType }, VoidPtrType },
        { Identifier("floor") ,  { Float64Type }, Float64Type },
        { Identifier("floor") ,  { Float32Type }, Float32Type },
        { Identifier("ceil")  ,  { Float64Type }, Float64Type },
        { Identifier("ceil")  ,  { Float32Type }, Float32Type },
        { Identifier("pow")   ,  { Float64Type, Float64Type }, Float64Type },
        { Identifier("pow")   ,  { Float32Type, Float32Type }, Float32Type },
        { Identifier("log")   ,  { Float64Type }, Float64Type },
        { Identifier("log")   ,  { Float32Type }, Float32Type },
        { Identifier("log10") ,  { Float64Type }, Float64Type },
        { Identifier("log10") ,  { Float32Type }, Float32Type },
        { Identifier("sqrt")  ,  { Float64Type }, Float64Type },
        { Identifier("sqrt")  ,  { Float32Type }, Float32Type },
        { Identifier("sin")   ,  { Float64Type }, Float64Type },
        { Identifier("sin")   ,  { Float32Type }, Float32Type },
        { Identifier("cos")   ,  { Float64Type }, Float64Type },
        { Identifier("cos")   ,  { Float32Type }, Float32Type },
    };
}

arco::TokenKind arco::ArcoContext::GetKeywordKind(llvm::StringRef Text) const {
    auto Itr = TokenKeywordMap.find(Text);
    if (Itr != TokenKeywordMap.end()) {
        return Itr->second;
    }
    return TokenKind::INVALID;
}

void arco::ArcoContext::RequestGen(Decl* D, GenericBinding* Binding) {
    if (D->GenRequestedAlready && !Binding) return;
    D->GenRequestedAlready = true;
    QueuedDeclsToGen.push({ Binding, D });
}

constexpr bool arco::OrderGlobalsComparitor::operator()(const VarDecl* Global1, const VarDecl* Global2) const {
    return Global1->Loc.LineNumber < Global2->Loc.LineNumber;
}
