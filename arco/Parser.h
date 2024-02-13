#ifndef ARCO_PARSER_H
#define ARCO_PARSER_H

#include "Lexer.h"
#include "AST.h"
#include "Logger.h"

namespace arco {

    class ArcoContext;

    // This is the maximum depth that arrays can be declared at.
    // For example, the following array has a depth of 2:
    //
    // [ [ 124, 12 ], [ 44 ] ]
    constexpr ulen MAX_ARRAY_DEPTH = 8;

    // Only need a single one since the declarations get extracted.
    // TODO: If this is ever multithreaded then there will be a need
    // for one of these per thread.
    extern VarDeclList* SingleVarDeclList;

    class Parser {
    public:

        explicit Parser(ArcoContext& Context, Module* Mod, const char* FilePath, const SourceBuf FileBuffer);

        FileScope* Parse();

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

        // Current declaration being parsed.
        Decl*       CDecl   = nullptr;
        // Current function being parsed.
        FuncDecl*   CFunc   = nullptr;

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

        void ParseImport();

        //===-------------------------------===//
        // Statements
        //===-------------------------------===//

        void ParseOptStmt(AstNode*& Stmt, u16 TokenEndDelim);
        AstNode* ParseStmt();
        FuncDecl* ParseFuncDecl(Modifiers Mods);
        VarDecl* ParseVarDecl(Modifiers Mods);
        VarDeclList* ParseVarDeclList(Modifiers Mods);
        VarDecl* CreateVarDecl(Token Tok, Identifier Name, Modifiers Mods);
        StructDecl* ParseStructDecl(Modifiers Mods);
        EnumDecl* ParseEnumDecl(Modifiers Mods);

        void ParseScopeStmts(LexScope& Scope);

        ReturnStmt* ParseReturn();
        IfStmt* ParseIf();
        AstNode* ParseLoop();
        LoopControlStmt* ParseLoopControl();
        PredicateLoopStmt* ParsePredicateLoop(Token LoopTok);
        RangeLoopStmt* ParseRangeLoop(Token LoopTok);
        IteratorLoopStmt* ParseIteratorLoop(Token LoopTok);
        NestedScopeStmt* ParseNestedScope();
        DeleteStmt* ParseDelete();

        Modifiers ParseModifiers();
        Type* ParseType(bool AllowImplicitArrayType);
        Type* ParseFunctionType();
        Type* ParseBasicType();

        //===-------------------------------===//
        // Expressions
        //===-------------------------------===//

        Expr* ParseAssignmentAndExprs();
        Expr* ParseExpr();
        Expr* ParseBinaryExpr(Expr* LHS);
        Expr* ParsePrimaryAndPostfixUnaryExpr();
        Expr* ParsePrimaryAndPostfixUnaryExpr(Expr* LHS);
        Expr* ParsePrimaryExpr();
        Expr* ParseIdentPostfix(Expr* Site);
        NumberLiteral* ParseIntLiteral();
        NumberLiteral* ParseHexLiteral();
        NumberLiteral* ParseBinLiteral();
        NumberLiteral* ParseCharLiteral();
        NumberLiteral* FinalizeIntLiteral(ulen Idx, u64 IntValue);
        NumberLiteral* ParseFloatLiteral();
        StringLiteral* ParseStringLiteral();
        FuncCall* ParseFuncCall(Expr* Site);
        Array* ParseArray();
        // Updates the largest array length at the depth
        // of the current array.
        void CalcLargestArrayLengthAtDepth(Array* Arr);
        // Recursively goes through the array and it's child arrays
        // and sets the required array lengths based on the largest
        // array lengths at the given depths.
        void SetRequiredArrayLengthForArray(Array* Arr, ulen CArrayDepth = 0);


        void ParseAggregatedValues(llvm::SmallVector<NonNamedValue, 2>& Values,
                                   u16 EndDelimTok,
                                   bool AllowTrailingComma);

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
        void SkipRecovery();

        Identifier ParseIdentifier(const char* ErrorMessage);

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