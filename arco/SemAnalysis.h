#ifndef ARCO_SEM_ANALYSIS_H
#define ARCO_SEM_ANALYSIS_H

#include "AST.h"
#include "Logger.h"

namespace arco {

    class ArcoContext;

    class SemAnalyzer {
        using BindableList = llvm::SmallVector<Type*, 8>;
    
    public:
        
        explicit SemAnalyzer(ArcoContext& Context, Decl* D);

        static void ReportStatementsInInvalidContext(FileScope* FScope);

        static void ResolveImports(FileScope* FScope, ArcoContext& Context);

        static void CheckForDuplicateFuncDeclarations(Module* Mod);
        static void CheckForDuplicateFuncDeclarations(Namespace* NSpace);

        void CheckFuncDecl(FuncDecl* Func);
        
        void CheckStructDecl(StructDecl* Struct);
        void CheckEnumDecl(EnumDecl* Enum);
        void CheckInterfaceDecl(InterfaceDecl* Interface);

        void CheckVarDecl(VarDecl* Var, bool PartialGenFixup = false, bool ForceCheck = false);

        static void RequestGenNonGenericFunc(ArcoContext& Context, FuncDecl* Func);

    private:
        ArcoContext& Context;
        Module*      Mod;
        Logger       Log;
        
        FileScope*  FScope;
        FuncDecl*   CFunc   = nullptr;
        StructDecl* CStruct = nullptr;
        VarDecl*    CVar    = nullptr;
        VarDecl*    CGlobal = nullptr;
        VarDecl*    CField  = nullptr;

        bool NotFullyQualifiedVarState = false;

        // Every time a loop is entered this is incremented,
        // and decremented when existed
        ulen LoopDepth = 0;

        bool CatchingErrors = false;

        struct Scope {
            Scope* Parent = nullptr;
            // This refers to anything that
            // prevents the usual flow of
            // execution such as 'break',
            // 'continue', 'return'
            bool FoundTerminal = false;
            // Keeping track of returns
            // as a way of find out
            // if a function definitatively
            // returns.
            bool AllPathsReturn = false;
            bool AllPathsBranch = false;
        }  * LocScope = nullptr;

        struct ArgMismatchData {
            ulen         ArgCount; // -1 if named argument.
            Identifier   Name;     // Set if a named argument.
            std::string& MismatchInfo;
            bool         FailBesidesBind = false;
        };

        void CheckFuncParams(FuncDecl* Func, bool NotFullyQualified = false);
        //void CheckStructFields(StructDecl* Struct, bool NotFullyQualified = false);

        void CheckNode(AstNode* Node);

        void FixupInterfaces(StructDecl* Struct);
        void FixupInterface(StructDecl* Struct, const StructDecl::InterfaceHook& InterfaceHook);
        void ReportAboutNoOverloadedInterfaceFunc(SourceLoc ErrorLoc,
                                                  InterfaceDecl* Interface,
                                                  FuncDecl* InterfaceFunc,
                                                  FuncsList* Canidates);
        std::string GetMismatchInfoForInterfaceFunc(FuncDecl* InterfaceFunc, FuncDecl* Canidate);

        //===-------------------------------===//
        // Statements
        //===-------------------------------===//

        void CheckScopeStmts(LexScope& LScope, Scope& NewScope);
        void CheckReturn(ReturnStmt* Return);
        void CheckLoopControl(LoopControlStmt* LoopControl);
        void CheckPredicateLoop(PredicateLoopStmt* Loop);
        void CheckRangeLoop(RangeLoopStmt* Loop);
        void CheckIteratorLoop(IteratorLoopStmt* Loop);
        void CheckDeleteStmt(DeleteStmt* Delete);
        void CheckIf(IfStmt* If, bool& AllPathsReturn, bool& AllPathsBranch);
        void CheckNestedScope(NestedScopeStmt* NestedScope, bool& AllPathsReturn, bool& AllPathsBranch);
        void CheckRaiseStmt(RaiseStmt* Raise);

        //===-------------------------------===//
        // Expressions
        //===-------------------------------===//

        void CheckBinaryOp(BinaryOp* BinOp);
        void CheckUnaryOp(UnaryOp* UniOp);
        template<bool SpellChecking>
        void CheckIdentRef(IdentRef* IRef,
                           bool ExpectsFuncCall,
                           Namespace* NamespaceToLookup,
                           StructType* StructToLookupTy = nullptr,
                           bool ExpectsGenericConstraint = false);
        void CheckFieldAccessor(FieldAccessor* FieldAcc, bool ExpectsFuncCall);
        void CheckThisRef(ThisRef* This);
        void CheckFuncCall(FuncCall* Call);
        FuncDecl* CheckCallToCanidates(Identifier FuncName,
                                       SourceLoc ErrorLoc,
                                       FuncsList* Canidates,
                                       llvm::SmallVector<Expr*>&      Args,
                                       llvm::SmallVector<NamedValue>& NamedArgs,
                                       bool& VarArgsPassAlong,
                                       GenericBinding*& Binding,
                                       Type*& RetTy,
                                       GenericBinding* StructGenericBinding);
        void CreateCallCasts(FuncDecl* Selected,
                             llvm::SmallVector<Expr*>&      Args,
                             llvm::SmallVector<NamedValue>& NamedArgs,
                             bool VarArgsPassAlong,
                             const BindableList* QualifiedTypes);
        FuncDecl* FindBestFuncCallCanidate(Identifier FuncName,
                                           FuncsList* Canidates,
                                           llvm::SmallVector<Expr*>&      Args,
                                           llvm::SmallVector<NamedValue>& NamedArgs,
                                           bool& SelectedVarArgsPassAlong,
                                           llvm::SmallVector<BindableList, 8>& AllBindableTypes,
                                           llvm::SmallVector<BindableList, 8>& AllQualTypes,
                                           BindableList* StructBindableTypes);
        bool CompareAsCanidate(FuncDecl* Canidate,
                               llvm::SmallVector<Expr*>&      Args,
                               llvm::SmallVector<NamedValue>& NamedArgs,
                               ulen& NumConflicts,
                               ulen& EnumImplicitConflicts,
                               ulen& NumSignConflicts,
                               bool& CanidateVarArgPassAlong,
                               bool& HasAny,
                               BindableList* BindableTypes,
                               BindableList* QualTypes,
                               BindableList* StructBindableTypes);
        bool CheckCallArg(Expr* Arg, VarDecl* Param, Type* ParamType,
                          ulen& NumConflicts,
                          ulen& EnumImplicitConflicts,
                          ulen& NumSignConflicts,
                          BindableList* BindableTypes,
                          BindableList* QualTypes,
                          BindableList* StructBindableTypes);
        bool CheckCallArgGeneric(Type* ArgTy,
                                 bool AllowImplicitPointer,
                                 Type* ParamType,
                                 BindableList& BindableTypes,
                                 BindableList& QualTypes,
                                 BindableList& StructBindableTypes,
                                 Type*& QualType,
                                 ulen QualIdx,
                                 bool IsRoot,
                                 bool FromPtr = false,
                                 ArgMismatchData* MismatchData = nullptr);
        bool CheckGenericConstraint(FuncDecl* Canidate,
                                    GenericType* GenTy,
                                    BindableList* BindableTypes,
                                    std::string* MismatchInfo = nullptr);
        void DisplayErrorForNoMatchingFuncCall(SourceLoc ErrorLoc,
                                               FuncsList* Canidates,
                                               const llvm::SmallVector<Expr*>&      Args,
                                               const llvm::SmallVector<NamedValue>& NamedArgs,
                                               BindableList* StructBindableTypes);
        void DisplayErrorForSingleFuncForFuncCall(
            const char* CallType,
            SourceLoc Loc,
            llvm::SmallVector<TypeInfo>& ParamTypes,
            const llvm::SmallVector<Expr*>&      Args,
            const llvm::SmallVector<NamedValue>& NamedArgs,
            ulen NumDefaultArgs = 0,
            FuncDecl* CalledFunc = nullptr,
            BindableList* StructBindableTypes = nullptr);
        bool DidGenericConstraintsHaveErrors(FuncDecl* Canidate,
                                             const BindableList& BindableTypes);
        std::string GetFuncDefForError(const llvm::SmallVector<TypeInfo>& ParamTypes, FuncDecl* CalledFunc);
        std::string GetCallMismatchInfo(const char* CallType,
                                        llvm::SmallVector<TypeInfo>& ParamTypes,
                                        const llvm::SmallVector<Expr*>&      Args,
                                        const llvm::SmallVector<NamedValue>& NamedArgs,
                                        ulen NumDefaultArgs,
                                        bool IsVariadic,
                                        ulen NumGenerics,
                                        ulen NumQualifications,
                                        BindableList& BindableTypes,
                                        BindableList& QualTypes,
                                        BindableList* StructBindableTypes,
                                        FuncDecl* Canidate,
                                        bool& PreliminaryError);
        void GetCallMismatchInfoForArg(Type* ParamTy,
                                       Expr* Arg,
                                       bool AllowImplicitPtr,
                                       bool ParamConstMemory,
                                       BindableList& BindableTypes,
                                       BindableList& QualTypes,
                                       BindableList* StructBindableTypes,
                                       ulen& QualIdx,
                                       ArgMismatchData& MismatchData);
        void ShowMismatchInfoForBindFail(bool IsRoot,
                                         Type* ArgTy,
                                         Type* ParamType,
                                         ArgMismatchData* MismatchData);
        std::string GetGenBindCallArgHeaderInfo(ArgMismatchData& MismatchData, Type* ArgTy);
        void DisplayErrorForBadCallSiteType(FuncCall* Call);

        void CheckIfErrorsAreCaptured(SourceLoc ErrorLoc, FuncDecl* CalledFunc);
        void CheckTryError(TryError* Try);
        bool CheckStdPanicFuncExists();

        void CheckArray(Array* Arr);
        void CheckArrayAccess(ArrayAccess* Access);
        void CheckTypeCast(TypeCast* Cast);
        void CheckTypeBitCast(TypeBitCast* Cast);
        void CheckStructInitializer(StructInitializer* StructInit);
        FuncDecl* CheckStructInitArgs(StructType* StructTy,
                                      SourceLoc ErrorLoc,
                                      llvm::SmallVector<Expr*>&      Args,
                                      llvm::SmallVector<NamedValue>& NamedArgs,
                                      bool& VarArgPassAlon);
        void CheckHeapAlloc(HeapAlloc* Alloc, Type* AssignToType);
        void CheckSizeOf(SizeOf* SOf);
        void CheckTypeOf(TypeOf* TOf);
        void CheckTypeId(TypeId* TId);
        void CheckRange(Range* Rg);
        void CheckMoveObj(MoveObj* Move);
        void CheckInitObj(InitObjStmt* Init);
        void CheckTernary(Ternary* Tern);
        void CheckVarDeclList(VarDeclList* List);
        void CheckCatchError(CatchError* Catch, VarDecl* CaptureVar);
        void CheckTypeOrExpr(TypeOrExpr* TOrE);

        void CheckCondition(Expr* Cond, const char* PreErrorText);

        void CreateCast(Expr* E, Type* ToType);

        bool IsAssignableTo(Type* ToTy, Expr* FromExpr);
        bool IsAssignableTo(Type* ToTy, Type* FromTy, Expr* FromExpr);
        bool IsCastableTo(Type* ToTy, Type* FromTy);
        bool ViolatesConstAssignment(VarDecl* DestVar, Expr* Assignment);
        bool ViolatesConstAssignment(Type* DestTy, bool DestConstAddress, Expr* Assignment);

        bool FixupType(Type* Ty, bool AllowDynamicArrays = false, bool PartialGenFixup = false);
        bool FixupArrayType(ArrayType* ArrayTy, bool PartialGenFixup, bool FixupElmTy = true);
        bool FixupStructType(StructType* StructTy, bool PartialGenFixup);
    public:
        static void FinishNonGenericStructType(ArcoContext& Context, StructType* StructTy);
    private:
        void FinishGenericStructType(StructType* StructTy);
        

        Decl* FindStructLikeTypeByName(Identifier Name);

        Type* QualifyType(Type* Ty, const GenericBinding* Binding);

        void CheckModifibility(Expr* LValue);
        bool IsLValue(Expr* E);

        void EnsureChecked(SourceLoc ErrLoc, VarDecl* Var);

        void DisplayCircularDepError(SourceLoc ErrLoc, VarDecl* StartDep, const char* ErrHeader);

        static void CheckForDuplicateFuncs(const FuncsList& FuncList);

        bool IsComparable(Type* Ty);

        void DisplayNoteInfoForTypeMismatch(Expr* FromExpr, Type* ToTy);
        void DisplayErrorForTypeMismatch(const char* ErrMsg, SourceLoc ErrorLoc,
                                         Expr* FromExpr, Type* ToTy);

        static llvm::SmallVector<TypeInfo> ParamsToTypeInfo(FuncDecl* Func);

        /// Returns nullptr if an error occured in folding the value such
        /// as signed overflow (unhelpfully called "poision" value by LLVM).
        /// 
        llvm::Value* GenFoldable(SourceLoc ErrorLoc, Expr* E);

        void AddGenericErrorInfo();

        void CreateQualifications(Decl* D);

        void Error(SourceLoc Loc, const char* Msg) {
            Log.BeginError(Loc, Msg);
            AddGenericErrorInfo();
            Log.EndError();
        }

        template<typename... TArgs>
        void Error(SourceLoc Loc, const char* Fmt, TArgs&&... Args) {
            Log.BeginError(Loc, Fmt, std::forward<TArgs>(Args)...);
            AddGenericErrorInfo();
            Log.EndError();
        }

        void Error(AstNode* Node, const char* Msg) {
            Error(Node->Loc, Msg);
        }

        template<typename... TArgs>
        void Error(AstNode* Node, const char* Fmt, TArgs&&... Args) {
            Error(Node->Loc, Fmt, std::forward<TArgs>(Args)...);
        }
    };

}

#endif // ARCO_SEM_ANALYSIS_H