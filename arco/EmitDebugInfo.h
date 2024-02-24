#ifndef ARCO_EMIT_DEBUG_INFO_H
#define ARCO_EMIT_DEBUG_INFO_H

#include "AST.h"

#include <llvm/IR/DIBuilder.h>
#include <llvm/IR/IRBuilder.h>

namespace arco {

    class ArcoContext;

    class DebugInfoEmitter {
    public:

        ~DebugInfoEmitter();

        explicit DebugInfoEmitter(ArcoContext& Context);

        void EmitFile(FileScope* FScope);

        llvm::DISubprogram* EmitFunc(FuncDecl* Func, bool ForwardDecl = false);
        void EmitParam(FuncDecl* Func, VarDecl* Param, llvm::IRBuilder<>& IRBuilder);
        void EmitFuncEnd(FuncDecl* Func);
        void EmitLocalVar(VarDecl* Var, llvm::IRBuilder<>& IRBuilder);
        void EmitThisVar(llvm::Value* LLThisAddr, FuncDecl* Func, llvm::IRBuilder<>& IRBuilder);

        void EmitGlobalVar(VarDecl* Global, llvm::IRBuilder<>& IRBuilder);

        void EmitScopeStart(SourceLoc Loc);
        void EmitScopeEnd();

        void EmitDebugLocation(SourceLoc Loc, llvm::IRBuilder<>& IRBuilder);
        void EmitDebugLocation(llvm::Instruction* LLInst, SourceLoc Loc);

        void Finalize();

    private:
        ArcoContext& Context;

        llvm::DIBuilder*     DBuilder;
        llvm::DICompileUnit* DebugUnit = nullptr;

        llvm::SmallVector<llvm::DIScope*> DILexicalScopes;

        llvm::DIType* EmitType(Type* Ty, llvm::DINode::DIFlags DIFlags = llvm::DINode::FlagZero);
        llvm::DIType* EmitFirstSeenType(Type* Ty, llvm::DINode::DIFlags DIFlags);
        llvm::DIType* EmitMemberFieldType(llvm::DIType* DIScope, VarDecl* Field, u64 BitsOffset);

    };
}

#endif // ARCO_EMIT_DEBUG_INFO_H