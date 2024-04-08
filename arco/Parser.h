#ifndef ARCO_PARSER_H
#define ARCO_PARSER_H

#include "Lexer.h"
#include "AST.h"
#include "Logger.h"
#include <llvm/ADT/DenseSet.h>

namespace arco {

    class ArcoContext;

    // This is the maximum depth that arrays can be declared at.
    // For example, the following array has a depth of 2:
    //
    // [ [ 124, 12 ], [ 44 ] ]
    constexpr ulen MAX_ARRAY_DEPTH = 8;

    class Parser {
    public:

        explicit Parser(ArcoContext& Context, FileScope* FScope, Module* Mod, const SourceBuf FileBuffer);

        void Parse();

        ulen GetLinesParsed() const { return Lex.GetLinesLexed(); }

    private:
        static const ulen MAX_SAVED_TOKENS = 8;

        ArcoContext& Context;
        Module*      Mod;
        Namespace*   NSpace;
        Lexer        Lex;
        Logger       Log;

        FileScope* FScope;

        Token PrevToken;
        Token CTok;
        ulen  SavedTokensCount = 0;
        Token SavedTokens[MAX_SAVED_TOKENS];

        // The current structure being parsed.
        StructDecl* CStruct = nullptr;
        // Current function being parsed.
        FuncDecl*   CFunc   = nullptr;
        // The current variable being parsed.
        VarDecl*    CVar    = nullptr;

        // Temporary storgage in case the native modifier
        // has an explicit name.
        llvm::StringRef NativeModifierName;

        bool AllowStructInitializer = true;

        /*
         * Used to keep track of variables
         * within the scope of functions.
         */
        struct Scope {
            Scope* Parent = nullptr;
            
            // TODO: Due to memory constraints and the fact that
            // in general functions have a rather small number of
            // variables it might be more efficient to use a list.
            llvm::DenseMap<Identifier, VarDecl*> VarDecls;

            // Recursively searches the scope stack for the variable
            // declaration given the name.
            VarDecl* FindVariable(Identifier Name);

        } * LocScope = nullptr;;

        // This tells us at what depth an array
        // is being parsed.
        ulen ArrayDepthCount = 0;

        // This tells us what the largest length at
        // a given depth is while parsing.
        ulen LargestArrayLengthAtDepth[MAX_ARRAY_DEPTH] = { 0 };

        enum class RecoveryStrat {
            Normal,
            Params,
            CallArgs,
            StructInitArgs,
            Array
        } RecStrat = RecoveryStrat::Normal;

        void ParseImport();

        //===-------------------------------===//
        // Statements
        //===-------------------------------===//

        void ParseOptStmt(AstNode*& Stmt, u16 TokenEndDelim);
        AstNode* ParseStmt();
        FuncDecl* ParseFuncDecl(Modifiers Mods, llvm::SmallVector<GenericType*> GenTys);
        void ParseFuncSignature(FuncDecl* Func);
        VarDecl* ParseVarDecl(Modifiers Mods, llvm::SmallVector<GenericType*> GenTys = {});
        VarDeclList* ParseVarDeclList(Modifiers Mods);
        VarDecl* CreateVarDecl(Token Tok, Identifier Name, Modifiers Mods);
        void FinishVarDecl(VarDecl* Var);
        StructDecl* ParseStructDecl(Modifiers Mods, llvm::SmallVector<GenericType*> GenTys = {});
        EnumDecl* ParseEnumDecl(Modifiers Mods);
        InterfaceDecl* ParseInterfaceDecl(Modifiers Mods);

        void ParseScopeStmtOrStmts(LexScope& Scope);
        void ParseScopeStmts(LexScope& Scope);

        ReturnStmt* ParseReturn();
        IfStmt* ParseIf();
        AstNode* ParseLoop();
        LoopControlStmt* ParseLoopControl();
        PredicateLoopStmt* ParsePredicateLoop(Token LoopTok);
        RangeLoopStmt* ParseRangeLoop(Token LoopTok, VarDeclList* List);
        IteratorLoopStmt* ParseIteratorLoop(Token LoopTok, VarDeclList* List);
        NestedScopeStmt* ParseNestedScope();
        DeleteStmt* ParseDelete();
        RaiseStmt* ParseRaise();
        InitObjStmt* ParseInitObj();

        Modifiers ParseModifiers();
        Type* ParseType(bool IsRoot,
                        bool AllowImplicitArrayType,
                        bool AllowDynamicArray,
                        bool ForRetTy = false);
        void ProcessGenericContainingType(Type* Ty);
        Type* ParseFunctionType();
        Type* ParseBasicType(bool ForRetTy = false);
        llvm::SmallVector<GenericType*> ParseGenerics();

        //===-------------------------------===//
        // Expressions
        //===-------------------------------===//

        Expr* ParseAssignmentAndExprs();
        Expr* ParseAssignmentAndExprs(Expr* LHS);
        Expr* ParseExprAndCatch();
        Expr* ParseExprAndCatch(Expr* E);
        Expr* ParseExpr();
        Expr* ParseBinaryExpr(Expr* LHS);
        Expr* ParsePrimaryAndPostfixUnaryExpr();
        Expr* ParsePrimaryAndPostfixUnaryExpr(Expr* LHS);
        Expr* ParsePrimaryExpr();
        Expr* ParseIdentPostfix(Expr* Site);
        NumberLiteral* ParseIntLiteral(NumberLiteral* Number);
        NumberLiteral* ParseHexLiteral(NumberLiteral* Number);
        NumberLiteral* ParseBinLiteral(NumberLiteral* Number);
        NumberLiteral* ParseCharLiteral();
        NumberLiteral* FinalizeIntLiteral(ulen Idx, u64 IntValue, NumberLiteral* Number);
        NumberLiteral* ParseFloatLiteral(NumberLiteral* Number);
        StringLiteral* ParseStringLiteral();
        FuncCall* ParseFuncCall(Expr* Site);
        StructInitializer* ParseStructInitializer(Token IdentTok, llvm::SmallVector<Type*, 8> BindTypes);
        Array* ParseArray();
        // Updates the largest array length at the depth
        // of the current array.
        void CalcLargestArrayLengthAtDepth(Array* Arr);
        // Recursively goes through the array and it's child arrays
        // and sets the required array lengths based on the largest
        // array lengths at the given depths.
        void SetRequiredArrayLengthForArray(Array* Arr, ulen CArrayDepth = 0);
        
        void ParseAggregatedValues(llvm::SmallVector<NonNamedValue>& Values,
                                   llvm::SmallVector<NamedValue>& NamedValues,
                                   RecoveryStrat Strat);

        void ParseTypeBindings(bool IsRoot, llvm::SmallVector<Type*, 8>& BindTypes);

        TypeOrExpr* ParseTypeOrExpr(u16 StopTok);

        template<typename T>
        T FoldInt(Token OpTok, T LHSVal, T RHSVal, bool& OpApplies);
        template<typename T>
        T FoldFloat(Token OpTok, T LHSVal, T RHSVal, bool& OpApplies);
        Expr* FoldNumbers(Token OpTok, NumberLiteral* LHS, NumberLiteral* RHS, NumberLiteral* Result);
        Expr* NewBinaryOp(Token OpTok, Expr* LHS, Expr* RHS);

        //===-------------------------------===//
        // Utilities
        //===-------------------------------===//

        /// Retreives the next Token from either
        /// the Lexer or the SavedTokens and
        /// stores it in CTok.
        ///
        void NextToken();

        /// Looks ahead n tokens and saves the tokens 
        /// skipped into SavedTokens.
        Token PeekToken(ulen n);

        /// if the current token matches the TokenKind
        /// then it is consumed, otherwise an error is
        /// generated.
        void Match(u16 Kind, const char* Purpose = nullptr);

        /// Skips tokens until it can find a valid place to
        /// start parsing again.
        void SkipRecovery(llvm::DenseSet<u16> IncludeSet = {});

        Identifier ParseIdentifier(const char* ErrorMessage);

        IdentRef* CreateIdentRef(Token IdentTok);

        void Error(Token Tok, const char* Msg) {
            Log.BeginError(Tok.Loc, Msg);
            Log.EndError();
        }

        void Error(SourceLoc Loc, const char* Msg) {
            Log.BeginError(Loc, Msg);
            Log.EndError();
        }

        template<typename... TArgs>
        void Error(Token Tok, const char* Fmt, TArgs&&... Args) {
            Log.BeginError(Tok.Loc, Fmt, std::forward<TArgs>(Args)...);
            Log.EndError();
        }

        template<typename... TArgs>
        void Error(SourceLoc Loc, const char* Fmt, TArgs&&... Args) {
            Log.BeginError(Loc, Fmt, std::forward<TArgs>(Args)...);
            Log.EndError();
        }

    };
}

#endif // ARCO_PARSER_H