#ifndef ARCO_CONTEXT_H
#define ARCO_CONTEXT_H

#include "Tokens.h"
#include "AST.h"

#include <queue>
#include <unordered_set>
#include <llvm/ADT/StringMap.h>
#include <llvm/ADT/PriorityQueue.h>

namespace llvm {
    class LLVMContext;
    class Module;
    class Type;
    class GlobalVariable;
    class StructType;
    class DIType;
    namespace CallingConv {
        using ID = unsigned;
    }
    
    // The function pointer cache needs vector of u32s to be hashed.
    template<> struct DenseMapInfo<llvm::SmallVector<u32>> {
        static bool isEqual(const llvm::SmallVector<u32>& LHS, const llvm::SmallVector<u32>& RHS) {
            return LHS == RHS;
        }
        static llvm::SmallVector<u32> getTombstoneKey() {
            // Do not remove function types from maps.
            return llvm::SmallVector<u32>{};
        }
        static llvm::SmallVector<u32> getEmptyKey() {
            return llvm::SmallVector<u32>{};
        }
        static unsigned getHashValue(const llvm::SmallVector<u32>& Val) {
            // We know index 0 exists because that is the return type.
            unsigned Hash = DenseMapInfo<u32>::getHashValue(Val[0]);
            for (ulen i = 0; i < Val.size(); i++) {
                Hash = detail::combineHashValue(DenseMapInfo<u32>::getHashValue(Val[i]), Hash);
            }
            return Hash;
        }
    };
}

namespace arco {

    struct OrderGlobalsComparitor {
        constexpr bool operator()(const VarDecl* Global1, const VarDecl* Global2) const;
    };

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

        void RequestGen(Decl* D, GenericBinding* Binding = nullptr);

        bool EmitDebugInfo;
        bool StandAlone;

        llvm::StringMap<Module*> ModNamesToMods;

        FuncDecl* MainEntryFunc = nullptr;

        bool CheckingUnhcecked = false;

        ulen BailCountForShowingOverloadedFuncs = 5;

        // 'main' identifier (for identifying entry points)
        Identifier MainIdentifier;
        // 'length' identifier (for identifying array lengths)
        Identifier LengthIdentifier;
        // 'buffer' identifier (for accessing slice buffers)
        Identifier BufferIdentifier;
        // Identifiers for calling conventions
        Identifier StdcallIdentifier;
        Identifier CdeclIdentifier;
        Identifier FastcallIdentifier;
        Identifier ForcesRaiseIdentifier;
        llvm::DenseMap<Identifier, llvm::CallingConv::ID> CallConventions;

        u32 UniqueTypeIdCounter = 1;

        Identifier StringIdentifier;
        Identifier AnyIdentifier;
        Identifier TypeIdentifier;
        Identifier TypeIdIdentifier;
        Identifier ArrayTypeIdentifier;
        Identifier StructTypeIdentifier;
        Identifier FieldTypeIdentifier;
        Identifier EnumTypeIdentifier;
        Identifier ErrorInterfaceIdentifier;
        StructDecl* StdStringStruct      = nullptr;
        StructDecl* StdAnyStruct         = nullptr;
        StructDecl* StdTypeStruct        = nullptr;
        StructDecl* StdArrayTypeStruct   = nullptr;
        StructDecl* StdStructTypeStruct  = nullptr;
        StructDecl* StdFieldTypeStruct   = nullptr;
        StructDecl* StdEnumTypeStruct    = nullptr;
        EnumDecl*   StdTypeIdEnum      = nullptr;
        InterfaceDecl* StdErrorInterface = nullptr;
        FuncDecl* StdErrorPanicFunc = nullptr;
        FuncDecl* InitializeErrorHandlingFunc = nullptr;
        StructType* StdStringStructType;
        StructType* StdAnyStructType;
        StructType* StdTypeStructType;
        StructType* StdArrayTypeStructType;
        StructType* StdStructTypeStructType;
        StructType* StdFieldTypeStructType;
        StructType* StdEnumTypeStructType;

        Type* IntType;
        Type* PtrsizeType;
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
        Type* CharPtrPtrType;
        Type* FuncRefType;
        Type* StructRefType;
        Type* EnumRefType;
        Type* InterfaceRefType;
        StructType* AnyType;
        PointerType* ErrorInterfacePtrType;

        // Maps a binary operator to its precedence.
        llvm::DenseMap<u16, u32> BinaryOpsPrecedence;

        struct QuduedGenDecl {
            GenericBinding* Binding;
            Decl*           D;
        };

        std::queue<QuduedGenDecl> QueuedDeclsToGen;

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
        struct LLIntrinsicDef {
            Identifier               Name;
            llvm::SmallVector<Type*> ParamTypes;
            Type*                    RetType;
        };
        // TODO: should this be a map instead?
        llvm::SmallVector<LLIntrinsicDef>          LLVMValidIntrinsicArgs;
        llvm::SmallVector<VarDecl*>                GlobalsNeedingDestruction;
        llvm::DenseMap<u32, llvm::GlobalVariable*> LLTypeInfoMap;
        llvm::DenseMap<u32, llvm::StructType*>     LLSliceTypes;
        llvm::DenseMap<u32, llvm::DIType*>         LLDITypeCache;
        llvm::SmallVector<llvm::Function*>         LLDiscardFuncs;

        llvm::DenseMap<u32, PointerType*>                     PointerTyCache;
        llvm::DenseMap<u32, ImplPointerType*>                 ImplPointerTyCache;
        llvm::DenseMap<u32, SliceType*>                       SliceTyCache;
        llvm::DenseMap<std::pair<u32, ulen>, ArrayType*>      ArrayTyCache;
        llvm::DenseMap<llvm::SmallVector<u32>, FunctionType*> FunctionTyCache;
        // TODO: llvm::DenseMap<u32, StructType*>                   StructCache;
        //std::queue<StructType*>                               ImplicitDefaultConstrucorsNeedingCreated;

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