#ifndef ARCO_IRGEN_H
#define ARCO_IRGEN_H

#include <llvm/IR/IRBuilder.h>

#include "AST.h"

namespace arco {

    class ArcoContext;
    struct GenericStructInfo;
    using InstructionRange = std::pair<llvm::Instruction*, llvm::Instruction*>;
    
    llvm::Type* GenType(ArcoContext& Context, Type* Ty);
    llvm::StructType* GenForwardDeclStructType(ArcoContext& Context);
    void GenStructType(ArcoContext& Context, StructType* StructTy, llvm::StructType* LLStructTy);
    llvm::FunctionType* GenArcoConvFuncType(ArcoContext& Context, FuncDecl* Func);
    llvm::IntegerType* GetSystemIntType(llvm::LLVMContext& LLContext, llvm::Module& LLModule);
    bool FuncUsesParamRetSlot(llvm::Module& LLModule, StructType* StructTy, ulen SizeInBytes);
    bool FuncUsesParamRetSlot(ArcoContext& Context, StructType* StructTy);

    ulen SizeOfTypeInBytes(llvm::Module& LLModule, llvm::Type* LLType);

    llvm::Twine GenFuncLinkName(FuncDecl* Func, bool IsMainFunc);

    class IRGenerator {
    public:

        explicit IRGenerator(ArcoContext& Context);

        void GenFuncDecl(FuncDecl* Func);
        void GenFuncBody(FuncDecl* Func);

        void GenGlobalVar(VarDecl* Global);

        void GenImplicitDefaultConstructor(StructType* StructTy);
        void GenImplicitCopyConstructor(StructType* StructTy);
        void GenImplicitMoveConstructor(StructType* StructTy);
        void GenImplicitDestructor(StructType* StructTy);

        void GenGenericStructConstructorSharedInstructions(StructType* StructTy);

        void GenGlobalInitFuncBody();
        void GenGlobalDestroyFuncBody();

        llvm::Value* GenRValue(Expr* E);

    private:
        ArcoContext&       Context;
        llvm::LLVMContext& LLContext;
        llvm::Module&      LLModule;
        llvm::IRBuilder<>  Builder;
    
        // Current LLVM function being processed.
        FuncDecl*       CFunc  = nullptr;
        llvm::Function* LLFunc = nullptr;

        // The basic block jumped to at the end of the function.
        llvm::BasicBlock* LLFuncEndBB = nullptr;

        // The address that the return value gets placed into.
        llvm::Value* LLRetAddr = nullptr;

        // The 'this' pointer of the struct.
        llvm::Value* LLThis = nullptr;

        // The exit points of loops currently being processed
        llvm::SmallVector<llvm::BasicBlock*, 4> LoopBreakStack;

        // The restart points of the next loop iteration
        llvm::SmallVector<llvm::BasicBlock*, 4> LoopContinueStack;

        // When within a context which may raise an error this points
        // to the address of an error which captures a raised error.
        llvm::Value* LLCatchErrorAddr = nullptr;
        // Where to redirect to if there is an error!
        llvm::BasicBlock* LLErrorCatchBlock;

        GenericStructInfo* SharedConstructorGenInfo = nullptr;

        struct DestroyObject {
            Type*        Ty;
            llvm::Value* LLAddr;
            bool         PastErrorInit = true;
        };

        // Objects which have destructors and need to be destroyed
        // and are encountered before any returns or branching is
        // encountered.
        //
        // These objects can always be gaurenteed to be destroyed so
        // any object in this list has its destructor called no matter
        // where a return occured.
        //
        // The entire point to having this list is to reduce the number
        // of instructions needed to be generated since when there are
        // multiple returns the return statements branch to a common
        // function return.
        // This common return can then manage the cleanup of all of these
        // objects.
        llvm::SmallVector<DestroyObject> AlwaysInitializedDestroyedObjects;

        // Current raised errors within the context of a catch.
        llvm::SmallVector<StructDecl*> CurrentCaughtErrors;

        // If the function returns a locally defined variable it's set here once
        // it has been initialized.
        VarDecl* LocalReturnVar = nullptr;

        using ErrorAddrList = llvm::SmallVector<llvm::Value*, 4>;

        struct Scope {
            Scope* Parent = nullptr;

            bool IsLoopScope = false;

            // All objects currently initialized within the scope unless the scope is
            // the primary scope of a function and no returns have been encountered
            // in which case they are placed in the AlwaysInitializedDestroyedObjects list.
            //
            llvm::SmallVector<DestroyObject> ObjectsNeedingDestroyed;

        }* LocScope = nullptr;

        llvm::SmallVector<DestroyObject> CurrentErrorBoundNeedingDestroyed;

        bool EncounteredReturn = false;

        bool EmitDebugInfo;

        ulen FieldInitializingIdx = -1;

        llvm::Function* GenGlobalInitFuncDecl();
        llvm::Function* GenDestroyGlobalsFuncDecl();

        llvm::Value* GenNode(AstNode* Node);

        void GenGlobalVarDecl(VarDecl* Global);

        //===-------------------------------===//
        // Statements
        //===-------------------------------===//

        llvm::Value* GenVarDecl(VarDecl* Var);
        llvm::Value* GenReturn(ReturnStmt* Ret);
        llvm::Value* GenLoopControl(LoopControlStmt* LoopControl);
        llvm::Value* GenPredicateLoop(PredicateLoopStmt* Loop);
        llvm::Value* GenRangeExprLoop(Range* Rg, LexScope& LScope, VarDecl* CaptureVar);
        llvm::Value* GenRangeLoop(RangeLoopStmt* Loop);
        llvm::Value* GenIteratorLoop(IteratorLoopStmt* Loop);
        llvm::Value* LoadIteratorLoopValueIfNeeded(llvm::Value* LLValuePtr, Type* ToTy, Type* FromTy);
        llvm::Value* GenDelete(DeleteStmt* Delete);
        llvm::Value* GenIf(IfStmt* If);
        llvm::Value* GenNestedScope(NestedScopeStmt* NestedScope);
        llvm::Value* GenRaise(RaiseStmt* Raise);
        llvm::Value* GenInitObj(InitObjStmt* InitObj);
        void GenRaiseReturnZeroedValue();

        //===-------------------------------===//
        // Expressions
        //===-------------------------------===//

        llvm::Value* GenBinaryOp(BinaryOp* BinOp);
        llvm::Value* GenUnaryOp(UnaryOp* UniOp);
        llvm::Value* GenNumberLiteral(NumberLiteral* Number);
        llvm::Value* GenStringLiteral(StringLiteral* String);
        llvm::Value* GenStringLiteral(const char* String, ulen Length);
        llvm::Value* GenIdentRef(IdentRef* IRef);
        llvm::Value* GenFieldAccessor(FieldAccessor* FieldAcc);
        llvm::Value* GenFuncCall(FuncCall* Call,
                                 llvm::Value* LLAddr);
        llvm::Value* GenFuncCallGeneral(Expr* CallNode,
                                        FuncDecl* CalledFunc,
                                        llvm::SmallVector<Expr*>&      Args,
                                        llvm::SmallVector<NamedValue>& NamedArgs,
                                        llvm::Value* LLAddr,
                                        bool VarArgsPassAlong);
        llvm::Value* GenCallArg(Expr* Arg, bool ImplictPtr);
        llvm::Value* GenArray(Array* Arr, llvm::Value* LLAddr, bool IsConstDest);
        ArrayType* GetGenArrayDestType(Array* Arr);
        llvm::Constant* GenConstArray(Array* Arr, ArrayType* DestTy);
        void FillArrayViaGEP(Array* Arr, llvm::Value* LLAddr, ArrayType* DestTy);
        llvm::Value* GenArrayAccess(ArrayAccess* Access);
        llvm::Value* GenTypeCast(TypeCast* Cast);
        llvm::Value* GenTypeBitCast(TypeBitCast* Cast);
        llvm::Value* GenStructInitializer(StructInitializer* StructInit,
                                          llvm::Value* LLAddr);
        void GenStructInitArgs(llvm::Value* LLAddr,
                               StructDecl* Struct,
                               StructType* StructTy,
                               llvm::SmallVector<Expr*>&      Args,
                               llvm::SmallVector<NamedValue>& NamedArgs);
        llvm::Value* GenHeapAlloc(HeapAlloc* Alloc, llvm::Value* LLAddr);
        llvm::Value* GenTypeOf(TypeOf* TOf);
        llvm::GlobalVariable* GenTypeOfGlobal(Type* GetTy);
        llvm::Constant* GenTypeOfType(Type* GetTy);
        llvm::GlobalVariable* GenTypeOfArrayTypeGlobal(ArrayType* ArrayTy);
        llvm::GlobalVariable* GenTypeOfStructTypeGlobal(StructType* StructTy);
        llvm::GlobalVariable* GenTypeOfEnumTypeGlobal(EnumDecl* Enum);
        llvm::Constant* GenTypeId(TypeId* TId);
        llvm::Value* GenTernary(Ternary* Tern, llvm::Value* LLAddr, bool DestroyIfNeeded);
        llvm::Value* GenVarDeclList(VarDeclList* List);
        llvm::Value* GenTryError(TryError* Try, llvm::Value* LLAddr);
        llvm::Value* GenCatchError(CatchError* Catch);
        llvm::BasicBlock* GenCatchErrorPrelude(CatchError* Catch);
        llvm::BasicBlock* GenTryErrorPrelude(TryError* Try);
        void GenCatchErrorBody(CatchError* Catch, llvm::BasicBlock* LLCatchEndBlock);
        void GenTryErrorBody(TryError* Try, llvm::BasicBlock* LLCatchEndBlock);

        llvm::Value* GenAdd(llvm::Value* LLLHS, llvm::Value* LLRHS, Type* Ty);
        llvm::Value* GenSub(llvm::Value* LLLHS, llvm::Value* LLRHS, Type* Ty);
        llvm::Value* GenMul(llvm::Value* LLLHS, llvm::Value* LLRHS, Type* Ty);
        llvm::Value* GenDiv(llvm::Value* LLLHS, llvm::Value* LLRHS, Type* Ty);

        void GenLoopCondJump(llvm::BasicBlock* LLCondBB,
                             llvm::BasicBlock* LLBodyBB,
                             llvm::BasicBlock* LLEndBB,
                             Expr* Cond);
        llvm::Value* GenCond(Expr* Cond);
        void GenBlock(llvm::BasicBlock* LLBB, ScopeStmts& Stmts);

        llvm::Value* GenCast(Type* ToType, Type* FromType, llvm::Value* LLValue);
        llvm::Value* GenCastToInterface(PointerType* InterfacePtrTy, StructDecl* Struct, llvm::Value* LLValue, llvm::Type* LLCastType);
        llvm::Value* CreateLoad(llvm::Value* LLAddr);

        void GenArrayToSlice(llvm::Value* LLSlice, llvm::Value* LLArray, Type* SliceTy, Type* ArrayTy);
        void GenToAny(llvm::Value* LLAny, llvm::Value* LLValue, Type* ValueTy);

        llvm::Constant* GenConstValue(Type* Ty);
        llvm::Constant* GenZeroedValue(Type* Ty);

        llvm::Value* GenMalloc(llvm::Type* LLType, llvm::Value* LLArrayLength);

        void StructArrayCallDefaultConstructors(Type* BaseTy,
                                                llvm::Value* LLArrStartPtr,
                                                llvm::Value* LLTotalLinearLength);
        void GenInternalArrayLoop(Type* BaseTy,
                                  llvm::Value* LLArrStartPtr,
                                  llvm::Value* LLTotalLinearLength,
                                  const std::function<void(llvm::PHINode*, Type*)>& CodeGenCallback);

        llvm::Value* GenGlobalEnumArray(EnumDecl* Enum);

        llvm::GlobalVariable* GenVTable(StructDecl* Struct);

        /// Converts the llvm array type to its eqv. pointer type.
        ///
        llvm::Value* DecayArray(llvm::Value* LLArray);
        llvm::Value* ArrayToPointer(llvm::Value* LLArray);
        llvm::Value* MultiDimensionalArrayToPointerOnly(llvm::Value* LLArray, ArrayType* ArrTy);

        inline llvm::Value* CreateInBoundsGEP(llvm::Value* LLAddr, llvm::ArrayRef<llvm::Value*> IdxList);
        inline llvm::Value* GetArrayIndexAddress(llvm::Value* LLArray, llvm::Value* LLIndex);
        inline llvm::Value* CreateStructGEP(llvm::Value* LLAddr, ulen Idx);

        llvm::GlobalVariable* GenLLVMGlobalVariable(llvm::StringRef Name, llvm::Type* LLType);
        llvm::GlobalVariable* GenConstGlobalArray(llvm::Constant* LLArray, bool DSOLocal = true);

        inline ulen SizeOfTypeInBytes(llvm::Type* LLType) {
            return arco::SizeOfTypeInBytes(LLModule, LLType);
        }
        ulen SizeOfTypeInBytesNonVirtualInclusive(Type* Ty);
        inline llvm::Align GetAlignment(llvm::Type* LLType);

        llvm::Value* GenReturnValueForOptimizedStructAsInt(llvm::Value* LLRetVal);

        void GenReturnByStoreToElisionRetSlot(Expr* Value, llvm::Value* LLSlot);

        void CopyOrMoveStructObject(llvm::Value* LLToAddr,
                                    llvm::Value* LLFromAddr,
                                    StructType*  StructTy);
        void CopyStructObject(llvm::Value* LLToAddr, llvm::Value* LLFromAddr, StructType* StructTy);
        void MoveStructObject(llvm::Value* LLToAddr, llvm::Value* LLFromAddr, StructType* StructTy);

        void GenConstructorBodyFieldAssignments(FuncDecl* Func, StructDecl* Struct);
        void GenCallToInitVTableFunc(llvm::Value* LLAddr,
                                     StructDecl*  Struct,
                                     StructType*  StructTy);
        llvm::Function* GenInitVTableFunc(StructDecl* Struct, StructType* StructTy);

        std::tuple<bool, llvm::Constant*> GenGlobalVarInitializeValue(VarDecl* Global);

        void AddObjectToDestroyOpt(Type* Ty, llvm::Value* LLAddr);
        void AddObjectToDestroy(Type* Ty, llvm::Value* LLAddr);

        void CallDestructors(llvm::SmallVector<DestroyObject>& Objects);
        void CallDestructors(Type* Ty, llvm::Value* LLAddr);

        void DestroyLocScopeInitializedObjects();
        void DestroyCurrentlyInitializedObjects();
        
        llvm::Value* GetOneValue(Type* Ty);

        /// This will only unconditionally branch to the given
        /// block as long as the current block does not already
        /// end in a branch (terminal).
        /// 
        /// This can happen for example in the following code
        /// if a < 5 {
        ///     return;
        /// }
        ///
        /// There is no reason for the if statement to branch to the
        /// end of its if statement 'if.end' if there is a return which
        /// causes it to branch to the end of the function already.
        ///
        /// Another code example:
        ///
        /// loop i int = 0; i < 55; i++ {
        /// }
        /// 
        /// The conditional block branches to the body of the loop if
        /// the statement is true, so when emitting the body of the loop
        /// it is essential to not branch unconditionally since a conditional
        /// branch was placed right beforehand.
        ///
        void GenBranchIfNotTerm(llvm::BasicBlock* LLBB);

        void GenBranchOnCond(Expr* Cond, llvm::BasicBlock* LLTrueBB, llvm::BasicBlock* LLFalseBB);

        void GenAssignment(llvm::Value* LLAddress,
                           Type* AddrTy,
                           Expr* Value,
                           bool IsConstAddress,
                           bool DestroyIfNeeded = false);
        void GenDefaultValue(Type* Ty, llvm::Value* LLAddr);

        void GenSliceToSlice(llvm::Value* LLToAddr, Expr* Assignment);

        void CallDefaultConstructor(llvm::Value* LLAddr, StructType* StructTy);

        llvm::Value* CreateUnseenAlloca(llvm::Type* LLTy, const char* Name);

        llvm::Value* GetElisionRetSlotAddr(FuncDecl* Func);
        llvm::Value* GetErrorRetAddr(FuncDecl* Func);

        void GenStoreStructRetFromCall(FuncCall* Call, llvm::Value* LLAddr);

        llvm::Value* GenLLVMIntrinsicCall(SourceLoc CallLoc,
                                          FuncDecl* CalledFunc,
                                          const llvm::SmallVector<Expr*>&      Args,
                                          const llvm::SmallVector<NamedValue>& NamedArgs);

        using ReplaceInstList = llvm::SmallVector<std::pair<llvm::Instruction*, llvm::Instruction*>, 64>;
        void GenCommonSharedFieldUnseenAllocs(
            llvm::Function* LLSharedFunc,
            ReplaceInstList& LLReplaceUnseenInstructions
            );
        void GenCommonSharedFieldInstructions(
            llvm::Value* LLThis,
            llvm::Function* LLSharedFunc,
            const InstructionRange& Range,
            ReplaceInstList& LLReplaceInstructions,
            ReplaceInstList& LLReplaceUnseenInstructions,
            llvm::DenseMap<llvm::BasicBlock*, llvm::BasicBlock*>& LLVisitedBlocks,
            bool IsRoot = true);
        llvm::BasicBlock* GenCommonSharedFieldInstructionsBlock(
            llvm::Value* LLThis,
            llvm::Function* LLSharedFunc,
            ReplaceInstList& LLReplaceInstructions,
            ReplaceInstList& LLReplaceUnseenInstructions,
            llvm::DenseMap<llvm::BasicBlock*, llvm::BasicBlock*>& LLVisitedBlocks,
            llvm::BasicBlock* LLBranchBB,
            llvm::Instruction* LLEndInst
        );

        inline DebugInfoEmitter* GetDIEmitter(Decl* D) {
            return D->FScope->DIEmitter;
        }
        inline DebugInfoEmitter* GetDIEmitter() {
            assert(CFunc && "Cannot get emitter because not within function scope");
            return GetDIEmitter(CFunc);
        }
        void EmitDebugLocation(SourceLoc Loc);
        inline void EmitDebugLocation(AstNode* Node) {
            EmitDebugLocation(Node->Loc);
        }

        inline llvm::Type* GenType(Type* Ty) {
            return arco::GenType(Context, Ty);
        }

        inline llvm::Type* GenType(Type* Ty, Decl* CDecl) {
            return arco::GenType(Context, Ty);
        }

        inline llvm::Constant* GetLLInt8(i32 Value) {
            return llvm::ConstantInt::get(
                llvm::IntegerType::getInt8Ty(LLContext), Value, true);
        }
        inline llvm::Constant* GetLLUInt8(u32 Value) {
            return llvm::ConstantInt::get(
                llvm::IntegerType::getInt8Ty(LLContext), Value, false);
        }
        inline llvm::Constant* GetLLInt16(i32 Value) {
            return llvm::ConstantInt::get(
                llvm::IntegerType::getInt16Ty(LLContext), Value, true);
        }
        inline llvm::Constant* GetLLUInt16(u32 Value) {
            return llvm::ConstantInt::get(
                llvm::IntegerType::getInt16Ty(LLContext), Value, false);
        }
        inline llvm::Constant* GetLLInt32(i32 Value) {
            return llvm::ConstantInt::get(
                llvm::IntegerType::getInt32Ty(LLContext), Value, true);
        }
        inline llvm::Constant* GetLLUInt32(u32 Value) {
            return llvm::ConstantInt::get(
                llvm::IntegerType::getInt32Ty(LLContext), Value, false);
        }
        inline llvm::Constant* GetLLInt64(i64 Value) {
            return llvm::ConstantInt::get(
                llvm::IntegerType::getInt64Ty(LLContext), Value, true);
        }
        inline llvm::Constant* GetLLUInt64(u64 Value) {
            return llvm::ConstantInt::get(
                llvm::IntegerType::getInt64Ty(LLContext), Value, false);
        }
        inline llvm::Constant* GetSystemUInt(u64 Value) {
            return llvm::ConstantInt::get(GetSystemIntType(LLContext, LLModule), Value, false);
        }

        inline llvm::Constant* GetSystemInt(i64 Value) {
            return llvm::ConstantInt::get(GetSystemIntType(LLContext, LLModule), Value, true);
        }

    };

}

#endif // ARCO_IRGEN_H