#ifndef ARCO_CONTEXT_H
#define ARCO_CONTEXT_H

#include "Tokens.h"
#include "AST.h"

#include <queue>
#include <unordered_set>
#include <llvm/ADT/StringMap.h>

namespace llvm {
	class LLVMContext;
	class Module;
	namespace CallingConv {
		using ID = unsigned;
	}
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
		// Identifiers for calling conventions
		Identifier StdcallIdentifier;
		Identifier CdeclIdentifier;
		Identifier FastcallIdentifier;
		llvm::DenseMap<Identifier, llvm::CallingConv::ID> CallConventions;

		Identifier StringIdentifier;
		StructDecl* StdStringStruct = nullptr;

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
		Type* Float32Type;
		Type* Float64Type;
		Type* ErrorType;
		Type* EmptyArrayElmType;
		Type* ImportType;
		Type* VoidPtrType;
		Type* CharPtrType;
		Type* FuncRef;

		// Maps a binary operator to its precedence.
		llvm::DenseMap<u16, u32> BinaryOpsPrecedence;

		std::queue<Decl*> QueuedDeclsToGen;

		// Even if a declaration is not generated it should
		// still be checked to make sure there is not errors
		// with the code.
		std::unordered_set<Decl*> UncheckedDecls;

		/// When the assignment of a global variable is not foldable
		/// it must be assigned at the start of the program. The variables
		/// that need assignment are stored here until their assignments
		/// are generated.
		///
		llvm::SmallVector<VarDecl*, 16> GlobalPostponedAssignments;

		// ----- LLVM -----
		llvm::LLVMContext& LLContext;
		llvm::Module&      LLArcoModule;
		ulen               NumGeneratedGlobalVars = 0;
		llvm::Function*    LLInitGlobalFunc;
		llvm::Function*    LLDestroyGlobalsFunc;
		llvm::DenseMap<Identifier, llvm::Intrinsic::ID> LLVMIntrinsicsTable;
		llvm::DenseMap<StructDecl*, llvm::Function*> CompilerGeneratedDestructors;
		llvm::SmallVector<VarDecl*> GlobalsNeedingDestruction;

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