#include "AdvancedFuzzing.h"

#include <functional>
#include <fstream>

#include "FuzzUtils.h"

using TokList = llvm::SmallVector<FuzzToken>;
using arco::TokenKind;

void BuildScopeStmts(int NumNodes);
arco::AstKind BuildStmt();
bool BuildModifiers();
void BuildExpr();

bool BuildingArrayType = false;
bool AllowStructInitializer = true;

bool RootScope = false;
int ScopeDepth = 0;
int ExprDepth  = 0;

llvm::DenseSet<u32> SemaConsiders;

TokList Tokens;
arco::ArcoContext* Context;

void PushToken(u16 Kind) {
    Tokens.push_back(FuzzToken::FromKind(Kind));
}

void PushToken(std::string Lexeme) {
    FuzzToken Tok;
    Tok.Lexeme = Lexeme;
    Tok.Kind = FUZZ_BUILT_TOKEN_KIND;
    Tokens.push_back(Tok);
}

void BuildType(bool AllowImplicitArrayType) {
    
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
        IsSlice = rand() % 14 == 0;
    }
    bool IsArray = rand() % 14 == 0;

    TypeKind Kind = TypeKinds[rand() % TypeKinds.size()];
    switch (Kind) {
    case TypeKind::Ptrsize: PushToken(TokenKind::KW_PTRSIZE); break;
    case TypeKind::UInt8:   PushToken(TokenKind::KW_UINT8);   break;
    case TypeKind::UInt16:  PushToken(TokenKind::KW_UINT16);  break;
    case TypeKind::UInt32:  PushToken(TokenKind::KW_UINT32);  break;
    case TypeKind::UInt64:  PushToken(TokenKind::KW_UINT64);  break;
    case TypeKind::Int:     PushToken(TokenKind::KW_INT);     break;
    case TypeKind::Int8:    PushToken(TokenKind::KW_INT8);    break;
    case TypeKind::Int16:   PushToken(TokenKind::KW_INT16);   break;
    case TypeKind::Int32:   PushToken(TokenKind::KW_INT32);   break;
    case TypeKind::Int64:   PushToken(TokenKind::KW_INT64);   break;
    case TypeKind::Char:    PushToken(TokenKind::KW_CHAR);    break;
    case TypeKind::Float32: PushToken(TokenKind::KW_FLOAT32); break;
    case TypeKind::Float64: PushToken(TokenKind::KW_FLOAT64); break;
    case TypeKind::Void:    PushToken(TokenKind::KW_VOID);    break;
    case TypeKind::Bool:    PushToken(TokenKind::KW_BOOL);    break;
    case TypeKind::CStr:    PushToken(TokenKind::KW_CSTR);    break;
    case TypeKind::Struct:  PushToken(TokenKind::IDENT);      break;
    }

    if (IsPtr) {
        int PtrCount = rand() % 3;
        for (int i = 0; i < PtrCount; i++) {
            PushToken('*');
        }
    }
    if (IsSlice) {
        PushToken('[');
        PushToken('*');
        PushToken(']');
    }

    // TODO: Should there be arrays of slices? The parser doesn't support this at the moment.
    if (IsArray && !IsSlice) {
        int ArrayDepth = (rand() % 3) + 1;
        bool WasbuildingArrayType = BuildingArrayType;
        BuildingArrayType = true;
        for (int i = 0; i < ArrayDepth; i++) {
            PushToken('[');
            BuildExpr();
            PushToken(']');
        }
        BuildingArrayType = WasbuildingArrayType;
    }
}

void BuildVarDecl(bool AllowModifiers, bool AllowAssignment = false) {
    if (AllowModifiers) {
        BuildModifiers();
    }
    PushToken(TokenKind::IDENT);
    bool InferType = rand() % 2 == 0 && AllowAssignment;
    if (InferType) {
        PushToken(TokenKind::COL_EQ);
        BuildExpr();
    } else {
        BuildType(true);
        if (AllowAssignment) {
            bool HasAssignment = rand() % 2;
            if (HasAssignment) {
                PushToken('=');
                BuildExpr();
            }
        }
    }
}

void BuildFunc() {
    
    bool IsNative = BuildModifiers();

    PushToken(TokenKind::KW_FN);
    PushToken(TokenKind::IDENT);
    PushToken('(');
    int ParamsCount = rand() % 6;
    for (int i = 0; i < ParamsCount; i++) {
        BuildVarDecl(false);
        if (i + 1 != ParamsCount) {
            PushToken(',');
        }
    }
    PushToken(')');
    if (rand() % 2 == 0) {
        BuildType(false);
    }
    if (!IsNative) {
        PushToken('{');
        if (ScopeDepth < 3) { // Limit so we do not just endlessly create too many tokens.
            BuildScopeStmts(rand() % 5);
        }
        PushToken('}');
    } else {
        PushToken(';');
    }
}

void BuildImport() {
    PushToken(TokenKind::KW_IMPORT);
    PushToken(TokenKind::IDENT);
    bool NspaceOrStruct  = rand() % 2 == 0;
    bool NspaceOrStruct2 = rand() % 2 == 0;
    if (NspaceOrStruct) {
        PushToken('.');
        PushToken(TokenKind::IDENT);
    }
    if (NspaceOrStruct2) {
        PushToken('.');
        PushToken(TokenKind::IDENT);
    }

    PushToken(';');
}

void BuildArray() {
    PushToken('[');
    int NumValues = rand() % 4;
    for (int i = 0; i < NumValues; i++) {
        BuildExpr();
        if (i + 1 != NumValues) {
            PushToken(',');
        } else {
            bool TrailingComma = rand() % 2 == 0;
            if (TrailingComma) {
                PushToken(',');
            }
        }
    }
    PushToken(']');
}

void BuildStructInitializer() {
    PushToken(TokenKind::IDENT);
    PushToken('{');
    int NumValues = rand() % 4;
    for (int i = 0; i < NumValues; i++) {
        BuildExpr();
        if (i + 1 != NumValues) {
            PushToken(',');
        }
    }
    PushToken('}');
}

void BuildPrimaryExpr() {
    ++ExprDepth;

    using arco::AstKind;
    llvm::SmallVector<AstKind> StartNodes = {
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
        AstKind::ARRAY,
        AstKind::MOVEOBJ
    };
    if (AllowStructInitializer) {
        StartNodes.push_back(AstKind::STRUCT_INITIALIZER);
    }

    static llvm::SmallVector<u16> NumberKinds = {
        TokenKind::INT_LITERAL,
        TokenKind::HEX_LITERAL,
        TokenKind::BIN_LITERAL,
        TokenKind::FLOAT32_LITERAL,
        TokenKind::FLOAT64_LITERAL,
        TokenKind::CHAR_LITERAL,
    };
    llvm::SmallVector<u16> PreUnaryOps = {
        '+', '-', '&', '*', '!', '~',
        TokenKind::PLUS_PLUS, TokenKind::MINUS_MINUS
    };

    if (ExprDepth > 10) {
        // The depth is too big let's just return a numberic value
        PushToken(NumberKinds[rand() % NumberKinds.size()]);
        --ExprDepth;
        return;
    }

    AstKind StartNode = StartNodes[rand() % StartNodes.size()];
    switch (StartNode) {
    case AstKind::NUMBER_LITERAL: {
        PushToken(NumberKinds[rand() % NumberKinds.size()]);
        break;
    }
    case AstKind::STRING_LITERAL:
        PushToken(TokenKind::STRING_LITERAL);
        break;
    case AstKind::BOOL_LITERAL:
        PushToken(rand() % 2 == 0 ? TokenKind::KW_TRUE : TokenKind::KW_FALSE);
        break;
    case AstKind::NULLPTR:
        PushToken(TokenKind::KW_NULL);
        break;
    case AstKind::IDENT_REF:
        PushToken(TokenKind::IDENT);
        break;
    case AstKind::STRUCT_INITIALIZER:
        BuildStructInitializer();
        break;
    case AstKind::UNARY_OP: {
        if (BuildingArrayType) {
            // don't include * if building an array type becaue it will think
            // it is a slice type. Doesn't make sense to dereference when making
            // array indexes anyway since that cannot be done at compile time.
            auto Itr = std::find(PreUnaryOps.begin(), PreUnaryOps.end(), '*');
            PreUnaryOps.erase(Itr);
        }
        PushToken(PreUnaryOps[rand() % PreUnaryOps.size()]);
        BuildPrimaryExpr();
        break;
    }
    case AstKind::TYPE_CAST: {
        PushToken(TokenKind::KW_CAST);
        PushToken('(');
        BuildType(false);
        PushToken(')');
        // TODO: ParsePrimaryAndPostfixUnaryExpr
        BuildPrimaryExpr();
        break;
    }
    case AstKind::TYPE_BITCAST: {
        PushToken(TokenKind::KW_BITCAST);
        PushToken('(');
        BuildType(false);
        PushToken(')');
        // TODO: ParsePrimaryAndPostfixUnaryExpr
        BuildPrimaryExpr();
        break;
    }
    case AstKind::SIZEOF: {
        PushToken(TokenKind::KW_SIZEOF);
        PushToken('(');
        BuildType(false);
        PushToken(')');
        break;
    }
    case AstKind::TYPEOF: {
        PushToken(TokenKind::KW_TYPEOF);
        PushToken('(');
        BuildType(false);
        PushToken(')');
        break;
    }
    // TODO: (expr)
    case AstKind::THIS_REF:
        PushToken(TokenKind::KW_THIS);
        break;
    case AstKind::ARRAY:
        BuildArray();
        break;
    case AstKind::MOVEOBJ: {
        PushToken(TokenKind::KW_MOVEOBJ);
        PushToken('(');
        BuildExpr();
        PushToken(')');
        break;
    default:
        assert(!"unreachable");
        break;
    }
    }

    --ExprDepth;
}

void BuildBinaryExpr() {
    ++ExprDepth;
    bool HasBinary = rand() % 2 == 0 && ExprDepth < 30;
    static llvm::SmallVector<u16> BinaryOps = {
        '*', '/', '%', '+', '-', TokenKind::LT_LT, TokenKind::GT_GT,
        '<', '>', TokenKind::LT_EQ, TokenKind::GT_EQ, TokenKind::EQ_EQ,
        TokenKind::EXL_EQ, '&', '^', '|', TokenKind::AMP_AMP, TokenKind::BAR_BAR
    };

    if (HasBinary) {
        u16 K = BinaryOps[rand() % BinaryOps.size()];
        bool IsShift = K == TokenKind::LT_LT || K == TokenKind::GT_GT;
        if (IsShift && SemaConsiders.contains(AdvFuzzSemConsiders::RESTRICT_SHIFT_SIZE)) {
            PushToken('(');
        }

        BuildBinaryExpr();
        PushToken(K);
        if (SemaConsiders.contains(AdvFuzzSemConsiders::RESTRICT_SHIFT_SIZE)) {
            if (IsShift) {
                // Do not actually know the size of the LHS side so just going
                // to go for the smallest size of 8 bits.
                PushToken(GenRandomIntLiteral(10, 7));
                PushToken(')');
            } else {
                BuildBinaryExpr();
            }
        } else {
            BuildBinaryExpr();
        }
    } else {
        BuildPrimaryExpr();
    }
    --ExprDepth;
}

void BuildExpr() {
    ++ExprDepth;
    BuildBinaryExpr();
    --ExprDepth;
}

void BuildReturn() {
    PushToken(TokenKind::KW_RETURN);
    if (rand() % 2 == 0) {
        BuildExpr();
    }
    PushToken(';');
}

void BuildScopeStmtOrStmts() {
    bool HasBrackets = rand() % 2 == 0;
    if (HasBrackets) {
        PushToken('{');
        if (ScopeDepth < 3) { // Limit so we do not just endlessly create too many tokens.
            BuildScopeStmts(rand() % 2);
        }
        PushToken('}');
    } else {
        BuildStmt();
    }
}

void BuildIf() {
    PushToken(TokenKind::KW_IF);
    bool PrevAllowStructInitializer = AllowStructInitializer;
    AllowStructInitializer = false;
    BuildExpr();
    AllowStructInitializer = PrevAllowStructInitializer;
    BuildScopeStmtOrStmts();
    bool HasElseIf = rand() % 4 == 0;
    bool HasElse = !HasElseIf && rand() % 4 == 0;
    if (HasElseIf) {
        PushToken(TokenKind::KW_ELSE);
        BuildIf();
    } else if (HasElse) {
        PushToken(TokenKind::KW_ELSE);
        BuildScopeStmtOrStmts();
    }
}

bool BuildModifiers() {
    bool IsNative = false;
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
        u16 K = ModifierTokens[SelectedIndex];
        if (K == TokenKind::KW_NATIVE) {
            IsNative = true;
        }
        PushToken(K);
        ModifierTokens.erase(ModifierTokens.begin() + SelectedIndex);
    }
    return IsNative;
}

void BuildStructDecl() {

    BuildModifiers();

    PushToken(TokenKind::IDENT);
    PushToken(TokenKind::KW_STRUCT);
    PushToken('{');
    BuildScopeStmts(rand() % 4);
    PushToken('}');
}

void BuildPredicateLoop() {
    PushToken(TokenKind::KW_LOOP);
    bool PrevAllowStructInitializer = AllowStructInitializer;
    AllowStructInitializer = false;
    BuildExpr();
    AllowStructInitializer = PrevAllowStructInitializer;
    BuildScopeStmtOrStmts();
}

void BuildRangeLoop() {
    PushToken(TokenKind::KW_LOOP);
    bool DeclExpr = rand() % 2 == 0;
    if (DeclExpr) {
        bool IsAssignment = rand() % 2 == 0;
        if (IsAssignment) {
            PushToken(TokenKind::IDENT);
            PushToken('=');
            BuildExpr();
        } else {
            BuildVarDecl(false);
        }
    }
    PushToken(';');
    bool PrevAllowStructInitializer = AllowStructInitializer;
    AllowStructInitializer = false;
    bool NoCond = rand() % 4 == 0;
    if (!NoCond) {
        //BuildExpr();
    }
    PushToken(';');
    // TODO: At the moment it expects only expressions
    // if the iterator is left off. This is not really
    // the intended functionality so is subject to change.
    BuildExpr();
    AllowStructInitializer = PrevAllowStructInitializer;
    BuildScopeStmtOrStmts();

    //bool NoItrExpr = rand() % 4 == 0;
    //if (!NoItrExpr) {
    //    BuildExpr(Tokens);   
    //}
    //BuildScopeStmtOrStmts(Tokens);
}

void BuildIteratorLoop() {
    PushToken(TokenKind::KW_LOOP);
    bool TypeDeclared = rand() % 2 == 0;
    if (TypeDeclared) {
        BuildVarDecl(false, false);
    } else {
        PushToken(TokenKind::IDENT);
    }
    PushToken(':');
    bool PrevAllowStructInitializer = AllowStructInitializer;
    AllowStructInitializer = false;
    BuildExpr();
    AllowStructInitializer = PrevAllowStructInitializer;
    BuildScopeStmtOrStmts();
}

void BuildDelete() {
    PushToken(TokenKind::KW_DELETE);
    BuildExpr();
    PushToken(';');
}

void BuildRaise() {
    PushToken(TokenKind::KW_RAISE);
    BuildStructInitializer();
    PushToken(';');
}

arco::AstKind BuildStmt() {
    using arco::AstKind;
    static llvm::SmallVector<AstKind> StartNodes = {
        AstKind::FUNC_DECL,
        AstKind::CONTINUE,
        AstKind::BREAK,
        AstKind::RETURN,
        AstKind::IF,
        AstKind::STRUCT_DECL,
        AstKind::VAR_DECL,
        AstKind::PREDICATE_LOOP,
        AstKind::RANGE_LOOP,
        AstKind::ITERATOR_LOOP,
        AstKind::NESTED_SCOPE,
        AstKind::DELETE,
        AstKind::RAISE
    };

    AstKind StartNode = StartNodes[rand() % StartNodes.size()];
    switch (StartNode) {
    case AstKind::FUNC_DECL:
        BuildFunc();
        break;
    case AstKind::CONTINUE:
        PushToken(TokenKind::KW_CONTINUE);
        PushToken(';');
        break;
    case AstKind::BREAK:
        PushToken(TokenKind::KW_BREAK);
        PushToken(';');
        break;
    case AstKind::RETURN:
        BuildReturn();
        break;
    case AstKind::IF:
        BuildIf();
        break;
    case AstKind::STRUCT_DECL:
        BuildStructDecl();
        break;
    case AstKind::VAR_DECL:
        BuildVarDecl(true, true);
        PushToken(';');
        break;
    case AstKind::PREDICATE_LOOP:
        BuildPredicateLoop();
        break;
    case AstKind::RANGE_LOOP:
        BuildRangeLoop();
        break;
    case AstKind::ITERATOR_LOOP:
        BuildIteratorLoop();
        break;
    case AstKind::NESTED_SCOPE:
        PushToken('{');
        if (ScopeDepth < 3) {
            BuildScopeStmts(rand() % 5);
        }
        PushToken('}');
        break;
    case AstKind::DELETE:
        BuildDelete();
        break;
    case AstKind::RAISE:
        BuildRaise();
        break;
    }
    return StartNode;
}

void BuildScopeStmts(int NumNodes) {
    ++ScopeDepth;
    for (int i = 0; i < NumNodes; i++) {
        bool WasRootScope = RootScope;
        RootScope = false;
        arco::AstKind Kind = BuildStmt();
        RootScope = WasRootScope;
        if (SemaConsiders.contains(AdvFuzzSemConsiders::NO_UNREACHABLE)) {
            if (!RootScope) {
                switch (Kind) {
                case arco::AstKind::RETURN:
                case arco::AstKind::BREAK:
                case arco::AstKind::CONTINUE:
                case arco::AstKind::RAISE:
                    return;
                }
            }
        }
    }
    --ScopeDepth;
}

void BuildRootNodes() {
    int NumRootNodes = 100;
    int NumImports = 10;

    for (int i = 0; i < NumImports; i++) {
        BuildImport();
    }

    RootScope = true;
    BuildScopeStmts(NumRootNodes);

}


void GenAdvancedFuzz(arco::ArcoContext& Context, llvm::DenseSet<u32> Considers) {
    
    SemaConsiders = std::move(Considers);

    // Building all the tokens!
    BuildRootNodes();
    
    std::ofstream FileStream("fuzz_gen.arco", std::ios::out | std::ios::trunc);
    WriteTokensToFile(Tokens, FileStream, Context);

}
