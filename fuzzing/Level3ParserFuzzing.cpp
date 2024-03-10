#include "Level3ParserFuzzing.h"

#include <functional>
#include <fstream>

#include "FuzzUtils.h"

using TokList = llvm::SmallVector<u16>&;
using arco::TokenKind;

void BuildScopeStmts(TokList Tokens, int NumNodes);
void BuildStmt(TokList Tokens);
void BuildModifiers(TokList Tokens);
void BuildExpr(TokList Tokens);

int ScopeDepth = 0;
int ExprDepth  = 0;

void BuildType(TokList Tokens, bool AllowImplicitArrayType) {
    
    using arco::TypeKind;
    static llvm::SmallVector<TypeKind> TypeKinds = {
        TypeKind::Ptrsize,
        TypeKind::UInt8,
        TypeKind::UInt16,
        TypeKind::UInt32,
        TypeKind::UInt64,
        TypeKind::Int,
        TypeKind::Int8,
        TypeKind::Int16,
        TypeKind::Int32,
        TypeKind::Int64,
        TypeKind::Char,
        TypeKind::Float32,
        TypeKind::Float64,
        TypeKind::Void,
        TypeKind::Bool,
        TypeKind::CStr,
        TypeKind::Struct,
    };

    bool IsPtr   = rand() % 8 == 0;
    bool IsSlice = false;
    if (!IsPtr) {
        IsSlice = rand() % 8 == 0;
    }
    bool IsArray = rand() % 8 == 0;

    TypeKind Kind = TypeKinds[rand() % TypeKinds.size()];
    switch (Kind) {
    case TypeKind::Ptrsize: Tokens.push_back(TokenKind::KW_PTRSIZE); break;
    case TypeKind::UInt8:   Tokens.push_back(TokenKind::KW_UINT8);   break;
    case TypeKind::UInt16:  Tokens.push_back(TokenKind::KW_UINT16);  break;
    case TypeKind::UInt32:  Tokens.push_back(TokenKind::KW_UINT32);  break;
    case TypeKind::UInt64:  Tokens.push_back(TokenKind::KW_UINT64);  break;
    case TypeKind::Int:     Tokens.push_back(TokenKind::KW_INT);     break;
    case TypeKind::Int8:    Tokens.push_back(TokenKind::KW_INT8);    break;
    case TypeKind::Int16:   Tokens.push_back(TokenKind::KW_INT16);   break;
    case TypeKind::Int32:   Tokens.push_back(TokenKind::KW_INT32);   break;
    case TypeKind::Int64:   Tokens.push_back(TokenKind::KW_INT64);   break;
    case TypeKind::Char:    Tokens.push_back(TokenKind::KW_CHAR);    break;
    case TypeKind::Float32: Tokens.push_back(TokenKind::KW_FLOAT32); break;
    case TypeKind::Float64: Tokens.push_back(TokenKind::KW_FLOAT64); break;
    case TypeKind::Void:    Tokens.push_back(TokenKind::KW_VOID);    break;
    case TypeKind::Bool:    Tokens.push_back(TokenKind::KW_BOOL);    break;
    case TypeKind::CStr:    Tokens.push_back(TokenKind::KW_CSTR);    break;
    case TypeKind::Struct:  Tokens.push_back(TokenKind::IDENT);      break;
    }

    if (IsPtr) {
        int PtrCount = rand() % 8;
        for (int i = 0; i < PtrCount; i++) {
            Tokens.push_back('*');
        }
    }
    if (IsSlice) {
        Tokens.push_back('[');
        Tokens.push_back('*');
        Tokens.push_back(']');
    }

    if (IsArray) {
        int ArrayDepth = (rand() % 3) + 1;
        for (int i = 0; i < ArrayDepth; i++) {
            Tokens.push_back('[');
            BuildExpr(Tokens);
            Tokens.push_back(']');
        }
    }
}

void BuildVarDecl(TokList Tokens, bool AllowModifiers) {
    if (AllowModifiers) {
        BuildModifiers(Tokens);
    }
    Tokens.push_back(TokenKind::IDENT);
    bool InferType = rand() % 2 == 0;
    if (InferType) {
        Tokens.push_back(TokenKind::COL_EQ);
        BuildExpr(Tokens);
    } else {
        BuildType(Tokens, true);
    }
}

void BuildFunc(TokList Tokens) {
    
    BuildModifiers(Tokens);

    Tokens.push_back(TokenKind::KW_FN);
    Tokens.push_back(TokenKind::IDENT);
    Tokens.push_back('(');
    int ParamsCount = rand() % 6;
    for (int i = 0; i < ParamsCount; i++) {
        BuildVarDecl(Tokens, false);
        if (i + 1 != ParamsCount) {
            Tokens.push_back(',');
        }
    }
    Tokens.push_back(')');
    if (rand() % 2 == 0) {
        BuildType(Tokens, false);
    }
    Tokens.push_back('{');
    if (ScopeDepth < 3) { // Limit so we do not just endlessly create too many tokens.
        BuildScopeStmts(Tokens, rand() % 5);
    }
    Tokens.push_back('}');
}

void BuildImport(TokList Tokens) {
    Tokens.push_back(TokenKind::KW_IMPORT);
    Tokens.push_back(TokenKind::IDENT);
    bool NspaceOrStruct  = rand() % 2 == 0;
    bool NspaceOrStruct2 = rand() % 2 == 0;
    if (NspaceOrStruct) {
        Tokens.push_back('.');
        Tokens.push_back(TokenKind::IDENT);
    }
    if (NspaceOrStruct2) {
        Tokens.push_back('.');
        Tokens.push_back(TokenKind::IDENT);
    }

    Tokens.push_back(';');
}

void BuildPrimaryExpr(TokList Tokens) {
    ++ExprDepth;

    using arco::AstKind;
    static llvm::SmallVector<AstKind> StartNodes = {
        AstKind::NUMBER_LITERAL,
        AstKind::STRING_LITERAL,
        AstKind::BOOL_LITERAL,
        AstKind::NULLPTR,
        AstKind::IDENT_REF,
        AstKind::UNARY_OP,
        AstKind::TYPE_CAST,
        AstKind::TYPE_BITCAST,
        AstKind::SIZEOF,
        AstKind::THIS_REF,

    };
    static llvm::SmallVector<u16> NumberKinds = {
        TokenKind::INT_LITERAL,
        TokenKind::HEX_LITERAL,
        TokenKind::BIN_LITERAL,
        TokenKind::FLOAT32_LITERAL,
        TokenKind::FLOAT64_LITERAL,
        TokenKind::CHAR_LITERAL,
    };
    static llvm::SmallVector<u16> PreUnaryOps = {
        '+', '-', '&', '*', '!', '~',
        TokenKind::PLUS_PLUS, TokenKind::MINUS_MINUS
    };

    if (ExprDepth > 30) {
        // The depth is too big let's just return a numberic value
        Tokens.push_back(NumberKinds[rand() % NumberKinds.size()]);
        return;
    }


    AstKind StartNode = StartNodes[rand() % StartNodes.size()];
    switch (StartNode) {
    case AstKind::NUMBER_LITERAL:
        Tokens.push_back(NumberKinds[rand() % NumberKinds.size()]);
        break;
    case AstKind::STRING_LITERAL:
        Tokens.push_back(TokenKind::STRING_LITERAL);
        break;
    case AstKind::BOOL_LITERAL:
        Tokens.push_back(rand() % 2 == 0 ? TokenKind::KW_TRUE : TokenKind::KW_FALSE);
        break;
    case AstKind::NULLPTR:
        Tokens.push_back(TokenKind::KW_NULL);
        break;
    case AstKind::IDENT_REF:
        Tokens.push_back(TokenKind::IDENT);
        break;
    case AstKind::UNARY_OP: {
        Tokens.push_back(PreUnaryOps[rand() % PreUnaryOps.size()]);
        BuildPrimaryExpr(Tokens);
        break;
    }
    case AstKind::TYPE_CAST: {
        Tokens.push_back(TokenKind::KW_CAST);
        Tokens.push_back('(');
        BuildType(Tokens, false);
        Tokens.push_back(')');
        // TODO: ParsePrimaryAndPostfixUnaryExpr
        BuildPrimaryExpr(Tokens);
        break;
    }
    case AstKind::TYPE_BITCAST: {
        Tokens.push_back(TokenKind::KW_BITCAST);
        Tokens.push_back('(');
        BuildType(Tokens, false);
        Tokens.push_back(')');
        // TODO: ParsePrimaryAndPostfixUnaryExpr
        BuildPrimaryExpr(Tokens);
        break;
    }
    case AstKind::SIZEOF: {
        Tokens.push_back(TokenKind::KW_SIZEOF);
        Tokens.push_back('(');
        BuildType(Tokens, false);
        Tokens.push_back(')');
        break;
    }
    case AstKind::TYPEOF: {
        Tokens.push_back(TokenKind::KW_TYPEOF);
        Tokens.push_back('(');
        BuildType(Tokens, false);
        Tokens.push_back(')');
        break;
    }
    // TODO: (expr)
    case AstKind::THIS_REF:
        Tokens.push_back(TokenKind::KW_THIS);
        break;
    }

    --ExprDepth;
}

void BuildBinaryExpr(TokList Tokens) {
    ++ExprDepth;
    bool HasBinary = rand() % 2 == 0 && ExprDepth < 30;
    static llvm::SmallVector<u16> BinaryOps = {
        '*', '/', '%', '+', '-', TokenKind::LT_LT, TokenKind::GT_GT,
        '<', '>', TokenKind::LT_EQ, TokenKind::GT_EQ, TokenKind::EQ_EQ,
        TokenKind::EXL_EQ, '&', '^', '|', TokenKind::AMP_AMP, TokenKind::BAR_BAR
    };

    if (HasBinary) {
        BuildBinaryExpr(Tokens);
        Tokens.push_back(BinaryOps[rand() % BinaryOps.size()]);
        BuildBinaryExpr(Tokens);
    } else {
        BuildPrimaryExpr(Tokens);
    }
    --ExprDepth;
}

void BuildExpr(TokList Tokens) {
    ++ExprDepth;
    BuildBinaryExpr(Tokens);
    --ExprDepth;
}

void BuildReturn(TokList Tokens) {
    Tokens.push_back(TokenKind::KW_RETURN);
    if (rand() % 2 == 0) {
        BuildExpr(Tokens);
    }
    Tokens.push_back(';');
}

void BuildScopeStmtOrStmts(TokList Tokens) {
    bool HasBrackets = rand() % 2 == 0;
    if (HasBrackets) {
        Tokens.push_back('{');
        if (ScopeDepth < 3) { // Limit so we do not just endlessly create too many tokens.
            BuildScopeStmts(Tokens, rand() % 2);
        }
        Tokens.push_back('}');
    } else {
        BuildStmt(Tokens);
    }
}

void BuildIf(TokList Tokens) {
    Tokens.push_back(TokenKind::KW_IF);
    BuildExpr(Tokens);
    BuildScopeStmtOrStmts(Tokens);
    bool HasElseIf = rand() % 4 == 0;
    bool HasElse = !HasElseIf && rand() % 4 == 0;
    if (HasElseIf) {
        Tokens.push_back(TokenKind::KW_ELSE);
        BuildIf(Tokens);
    } else if (HasElse) {
        Tokens.push_back(TokenKind::KW_ELSE);
        BuildScopeStmtOrStmts(Tokens);
    }
}

void BuildModifiers(TokList Tokens) {
    llvm::SmallVector<u16> ModifierTokens = {
        TokenKind::KW_NATIVE, TokenKind::KW_DLLIMPORT,
        TokenKind::KW_PRIVATE, TokenKind::KW_READONLY,
        TokenKind::KW_WRITEONLY
    };
    while (!ModifierTokens.empty()) {
        bool MoreOperators = rand() % 3 == 0;
        if (!MoreOperators) {
            break;
        }
        int SelectedIndex = rand() % ModifierTokens.size();
        Tokens.push_back(ModifierTokens[SelectedIndex]);
        ModifierTokens.erase(ModifierTokens.begin() + SelectedIndex);
    }
}

void BuildStructDecl(TokList Tokens) {

    BuildModifiers(Tokens);

    Tokens.push_back(TokenKind::IDENT);
    Tokens.push_back(TokenKind::KW_STRUCT);
    Tokens.push_back('{');
    BuildScopeStmts(Tokens, rand() % 4);
    Tokens.push_back('}');
}

void BuildPredicateLoop(TokList Tokens) {
    Tokens.push_back(TokenKind::KW_LOOP);
    BuildExpr(Tokens);
    BuildScopeStmtOrStmts(Tokens);
}

void BuildStmt(TokList Tokens) {
    using arco::AstKind;
    static llvm::SmallVector<AstKind> StartNodes = {
        AstKind::FUNC_DECL,
        AstKind::CONTINUE,
        AstKind::BREAK,
        AstKind::RETURN,
        AstKind::IF,
        AstKind::STRUCT_DECL,
        AstKind::VAR_DECL
    };

    AstKind StartNode = StartNodes[rand() % StartNodes.size()];
    switch (StartNode) {
    case AstKind::FUNC_DECL:
        BuildFunc(Tokens);
        break;
    case AstKind::CONTINUE:
        Tokens.push_back(TokenKind::KW_CONTINUE);
        Tokens.push_back(';');
        break;
    case AstKind::BREAK:
        Tokens.push_back(TokenKind::KW_BREAK);
        Tokens.push_back(';');
        break;
    case AstKind::RETURN:
        BuildReturn(Tokens);
        break;
    case AstKind::IF:
        BuildIf(Tokens);
        break;
    case AstKind::STRUCT_DECL:
        BuildStructDecl(Tokens);
        break;
    case AstKind::VAR_DECL:
        BuildVarDecl(Tokens, true);
        Tokens.push_back(';');
        break;
    case AstKind::PREDICATE_LOOP:
        BuildPredicateLoop(Tokens);
        break;
    }
}

void BuildScopeStmts(TokList Tokens, int NumNodes) {
    ++ScopeDepth;
    for (int i = 0; i < NumNodes; i++) {
        BuildStmt(Tokens);
    }
    --ScopeDepth;
}

void BuildRootNodes(TokList Tokens) {
    int NumRootNodes = 100;
    int NumImports = 10;

    for (int i = 0; i < NumImports; i++) {
        BuildImport(Tokens);
    }

    BuildScopeStmts(Tokens, NumRootNodes);

}


void GenLevel3ParseFuzz(arco::ArcoContext& Context) {
    
    // Building all the tokens!
    llvm::SmallVector<u16> AllTokens;
    BuildRootNodes(AllTokens);
    
    std::ofstream FileStream("fuzz_gen.arco", std::ios::out | std::ios::trunc);
    WriteTokensToFile(AllTokens, FileStream, Context);

}
