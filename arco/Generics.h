#ifndef ARCO_GENERICS_H
#define ARCO_GENERICS_H

#include <llvm/ADT/SmallVector.h>
#include <stack>
#include "Source.h"

namespace llvm {
    class Function;
    class Value;
    class Constant;
    class StructType;
    class Instruction;
}


namespace arco {

    struct Decl;
    struct VarDecl;
    struct FileScope;
    class Type;
    class StructType;
    class GenericType;

    using InstructionRange = std::pair<llvm::Instruction*, llvm::Instruction*>;

    struct GenericStructInfo {
        u32                      UniqueTypeId;
        llvm::StructType*        LLStructType         = nullptr;
        llvm::Function*          LLDefaultConstructor = nullptr;
        llvm::Function*          LLDestructor         = nullptr;
        llvm::Function*          LLMoveConstructor    = nullptr;
        llvm::Function*          LLCopyConstructor    = nullptr;
        StructType*              QualStructTy;

        llvm::SmallVector<Type*> QualifiedFieldTypes;

        llvm::SmallVector<bool> FieldsConstAddressInfo;

        ulen            VirtualOffset = 0;
        llvm::Function* LLInitVTableFunc = nullptr;

        llvm::SmallVector<InstructionRange> ConstructorsSharedInstructionRanges;
        llvm::Function* LLSharedConstructorsFunc;

        // See StructDecl for explainations.
        bool FieldsHaveAssignment;
        bool NeedsDestruction;
        bool MustForceRaise;

    };

    struct GenericFuncInfo {
        llvm::Function* LLFunction;
        bool  UsesOptimizedIntRet = false;
        bool  UsesParamRetSlot    = false;
        Type* QualRetTy;
    };

    struct GenericBinding {
        union {
            GenericFuncInfo* FuncInfo;
            llvm::Constant*  LLValue;
        };

        Decl* Dec;

        // This is left outside of the union because it is shared
        // by both structs and generic functions belonging to generic
        // structs.
        GenericStructInfo* StructInfo;
        
        llvm::SmallVector<Type*> QualTypes;

        Type*                       QualifiedType; // Type deduced for variables after binding and checking.
        llvm::SmallVector<Type*, 8> BindableTypes;
        GenericBinding*             ParentBinding;
        FileScope*                  OriginalFile;
        SourceLoc                   OriginalLoc;
    };

    using GenericBindings = llvm::SmallVector<GenericBinding*>;

    struct GenericData {
        llvm::SmallVector<GenericType*>   GenTys;
        llvm::SmallVector<Type*>          TypesNeedingQualification;
        llvm::SmallVector<VarDecl*>       VarsNeedingConstQualification;
        GenericBindings                   Bindings;
        GenericBinding*                   CurBinding = nullptr;
        GenericData*                      ParentGenData;
        // Stack used to restore state of whatever the previous binding
        // was if the binding changed while it currently had a binding.
        std::stack<GenericBinding*>       BindingStack;
        ulen                              NumQualifications = 0;
    };

    void BindTypes(Decl* D, GenericBinding* Binding);

    void UnbindTypes(Decl* D);

    GenericBinding* GetExistingBinding(Decl* D, const llvm::SmallVector<Type*, 8>& BindableTypes, GenericBinding* ParentBinding);

    GenericBinding* CreateNewBinding(Decl* D, llvm::SmallVector<Type*, 8> BindableTypes, GenericBinding* ParentBinding);

}

#endif // ARCO_GENERICS_H