#include "Context.h"

#include <llvm/IR/Module.h>
#include <llvm/IR/Intrinsics.h>

arco::ArcoContext::ArcoContext()
	:
	LLContext(*new llvm::LLVMContext),
	LLArcoModule(*new llvm::Module("Arco Module", LLContext)),

	MainIdentifier(Identifier("main")),
	LengthIdentifier(Identifier("length")),

	IntType(new Type(TypeKind::Int)),
	UIntType(new Type(TypeKind::UnsignedInt)),
	Int8Type(new Type(TypeKind::Int8)),
	Int16Type(new Type(TypeKind::Int16)),
	Int32Type(new Type(TypeKind::Int32)),
	Int64Type(new Type(TypeKind::Int64)),
	UInt8Type(new Type(TypeKind::UnsignedInt8)),
	UInt16Type(new Type(TypeKind::UnsignedInt16)),
	UInt32Type(new Type(TypeKind::UnsignedInt32)),
	UInt64Type(new Type(TypeKind::UnsignedInt64)),
	Float32Type(new Type(TypeKind::Float32)),
	Float64Type(new Type(TypeKind::Float64)),
	CharType(new Type(TypeKind::Char)),
	VoidType(new Type(TypeKind::Void)),
	BoolType(new Type(TypeKind::Bool)),
	CStrType(new Type(TypeKind::CStr)),
	NullType(new Type(TypeKind::Null)),
	ErrorType(new Type(TypeKind::Error)),
	EmptyArrayElmType(new Type(TypeKind::EmptyArrayElm)),
	ImportType(new Type(TypeKind::Import)),
	FuncRef(new Type(TypeKind::FuncRef)),

	LLVMIntrinsicsTable({
		{ Identifier("memcpy"), llvm::Intrinsic::IndependentIntrinsics::memcpy },
		{ Identifier("memset"), llvm::Intrinsic::IndependentIntrinsics::memset },
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
	delete UIntType;
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
	delete FuncRef;
	// TODO: Cleanup other types.

}

void arco::ArcoContext::Initialize() {
	
	TokenKeywordMap.insert({ "int"      , TokenKind::KW_INT       });
	TokenKeywordMap.insert({ "uint"     , TokenKind::KW_UINT      });
	TokenKeywordMap.insert({ "int8"     , TokenKind::KW_INT8      });
	TokenKeywordMap.insert({ "int16"    , TokenKind::KW_INT16     });
	TokenKeywordMap.insert({ "int32"    , TokenKind::KW_INT32     });
	TokenKeywordMap.insert({ "int64"    , TokenKind::KW_INT64     });
	TokenKeywordMap.insert({ "uint8"    , TokenKind::KW_UINT8     });
	TokenKeywordMap.insert({ "uint16"   , TokenKind::KW_UINT16    });
	TokenKeywordMap.insert({ "uint32"   , TokenKind::KW_UINT32    });
	TokenKeywordMap.insert({ "uint64"   , TokenKind::KW_UINT64    });
	TokenKeywordMap.insert({ "f32"      , TokenKind::KW_F32       });
	TokenKeywordMap.insert({ "f64"      , TokenKind::KW_F64       });
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
	TokenKeywordMap.insert({ "null"     , TokenKind::KW_NULL      });
	TokenKeywordMap.insert({ "fn"       , TokenKind::KW_FN        });
	TokenKeywordMap.insert({ "struct"   , TokenKind::KW_STRUCT    });
	TokenKeywordMap.insert({ "cast"     , TokenKind::KW_CAST      });
	TokenKeywordMap.insert({ "loop"     , TokenKind::KW_LOOP      });
	TokenKeywordMap.insert({ "break"    , TokenKind::KW_BREAK     });
	TokenKeywordMap.insert({ "continue" , TokenKind::KW_CONTINUE  });
	TokenKeywordMap.insert({ "if"       , TokenKind::KW_IF        });
	TokenKeywordMap.insert({ "else"     , TokenKind::KW_ELSE      });
	TokenKeywordMap.insert({ "return"   , TokenKind::KW_RETURN    });
	TokenKeywordMap.insert({ "native"   , TokenKind::KW_NATIVE    });
	TokenKeywordMap.insert({ "const"    , TokenKind::KW_CONST     });
	TokenKeywordMap.insert({ "private"  , TokenKind::KW_PRIVATE   });

	for (const auto& [Text, Kind] : TokenKeywordMap) {
		TokenKeywordInvertedMap.insert({ static_cast<u32>(Kind), Text });
	}

	VoidPtrType = PointerType::Create(VoidType, *this);
	CharPtrType = PointerType::Create(CharType, *this);

}

arco::TokenKind arco::ArcoContext::GetKeywordKind(llvm::StringRef Text) const {
	auto Itr = TokenKeywordMap.find(Text);
	if (Itr != TokenKeywordMap.end()) {
		return Itr->second;
	}
	return TokenKind::INVALID;
}

void arco::ArcoContext::RequestGen(Decl* D) {
	if (D->GenRequestedAlready) return;
	D->GenRequestedAlready = true;
	QueuedDeclsToGen.push(D);
}
