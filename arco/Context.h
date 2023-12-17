#ifndef ARCO_CONTEXT_H
#define ARCO_CONTEXT_H

#include "Tokens.h"
#include "AST.h"

#include <queue>
#include <llvm/ADT/StringMap.h>

namespace llvm {
	class LLVMContext;
	class Module;
}

namespace arco {

	class ArcoContext {
	public:

		ArcoContext();

		~ArcoContext();

		void Initialize();

		/// If the text matches one of the keywords then
		/// the token kind for the text is returned. Otherwise,
		/// a token kind of TokenKind::INVALID is returned.
		TokenKind GetKeywordKind(llvm::StringRef Text) const;

		/// Converts a keyword's token kind into its equivalent
		/// string.
		llvm::StringRef GetKeywordAsString(u16 Kind) const {
			return TokenKeywordInvertedMap.find(static_cast<u32>(Kind))->second;
		}

		void RequestGen(Decl* D);

		llvm::StringMap<Module*> ModNamesToMods;

		FuncDecl* MainEntryFunc = nullptr;

		// 'main' identifier (for identifying entry points)
		Identifier MainIdentifier;
		// 'length' identifier (for identifying array lengths)
		Identifier LengthIdentifier;

		Type* IntType;
		Type* UIntType;
		Type* VoidType;
		Type* CharType;
		Type* BoolType;
		Type* CStrType;
		Type* NullType;
		Type* Int8Type;
		Type* Int16Type;
		Type* Int32Type;
		Type* Int64Type;
		Type* UInt8Type;
		Type* UInt16Type;
		Type* UInt32Type;
		Type* UInt64Type;
		Type* ErrorType;
		Type* EmptyArrayElmType;
		Type* ImportType;
		Type* VoidPtrType;

		// Maps a binary operator to its precedence.
		llvm::DenseMap<u16, u32> BinaryOpsPrecedence;

		std::queue<Decl*> QueuedDeclsToGen;

		// ----- LLVM -----
		llvm::LLVMContext& LLContext;
		llvm::Module&      LLArcoModule;
		ulen               NumGeneratedGlobalVars = 0;
		llvm::DenseMap<Identifier, llvm::Intrinsic::ID> LLVMIntrinsicsTable;

		llvm::DenseMap<Type*, PointerType*>                PointerCache;
		llvm::DenseMap<std::pair<Type*, ulen>, ArrayType*> ArrayCache;
		std::queue<StructDecl*>                            DefaultConstrucorsNeedingCreated;

	private:
		// TODO: This can be replaced with perfect hashing!
		llvm::DenseMap<llvm::StringRef, TokenKind> TokenKeywordMap;

		// Keyword kind => Text
		//
		// The keyword kind is stored as a 32 bit integer
		// since DenseMaps know about how to store keys
		// of integers but not TokenKind.
		llvm::DenseMap<u32, llvm::StringRef> TokenKeywordInvertedMap;
	};
}

#endif // ARCO_CONTEXT_H