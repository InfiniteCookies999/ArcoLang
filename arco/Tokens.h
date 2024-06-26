#ifndef ARCO_TOKENS_H
#define ARCO_TOKENS_H

#include "Source.h"
#include <string>

namespace arco {

    class ArcoContext;

    enum TokenKind : u16 {
        INVALID = 0,

        // Index starts at 256 to reserve
        // space for UTF-8 characters.
        //
        __UNIQUE_TOKEN_KIND_START__ = 256,

        // === Keyword ===

        __KW_START__,
        
        KW_INT = __KW_START__,
        KW_PTRSIZE,
        KW_INT8,
        KW_INT16,
        KW_INT32,
        KW_INT64,
        KW_UINT8,
        KW_UINT16,
        KW_UINT32,
        KW_UINT64,
        KW_FLOAT32,
        KW_FLOAT64,
        KW_CHAR,
        KW_VOID,
        KW_CSTR,
        KW_BOOL,

        KW_NATIVE,
        KW_CONST,
        KW_PRIVATE,
        KW_READONLY,
        KW_WRITEONLY,
        KW_DLLIMPORT,
        KW_LINKNAME,
        
        KW_TRUE,
        KW_FALSE,
        KW_NEW,
        KW_THIS,
        KW_IMPORT,
        KW_NAMESPACE,
        KW_STATIC,
        KW_DELETE,
        KW_SIZEOF,
        KW_TYPEOF,
        KW_TYPEID,
        KW_COPYOBJ,
        KW_MOVEOBJ,
        KW_INITOBJ,
        KW_NULL,
        KW_TRY,
        KW_FN,
        KW_STRUCT,
        KW_ENUM,
        KW_INTERFACE,
        KW_RAISE,
        KW_RAISES,
        KW_CATCH,
        KW_CAST,
        KW_BITCAST,
        KW_GENERICS,
        KW_LOOP,
        KW_BREAK,
        KW_CONTINUE,
        KW_IF,
        KW_ELSE,
        KW_RETURN,

        __KW_END__ = KW_RETURN,

        // === Extra ===
        
        IDENT,
        INT_LITERAL,
        HEX_LITERAL,
        BIN_LITERAL,
        CHAR_LITERAL,
        FLOAT32_LITERAL,
        FLOAT64_LITERAL,
        ERROR_FLOAT_LITERAL,
        STRING_LITERAL,

        // === Symbols ===

        PLUS_PLUS,   // ++
        MINUS_MINUS, // --
        
        // Not really a token but used for
        // distingishing between post/pre
        // ++, --
        POST_PLUS_PLUS,
        POST_MINUS_MINUS,

        PLUS_EQ,           // +=
        MINUS_EQ,          // -=
        SLASH_EQ,          // /=
        STAR_EQ,           // *=
        MOD_EQ,            // %=
        AMP_EQ,            // &=
        BAR_EQ,            // |=
        CRT_EQ,            // ^=
        LT_LT,             // <<
        GT_GT,             // >>
        LT_LT_EQ,          // <<=
        GT_GT_EQ,          // >>=
        LT_EQ,             // <=
        GT_EQ,             // >=
        AMP_AMP,           // &&
        BAR_BAR,           // ||
        EQ_EQ,             // ==
        EXL_EQ,            // !=
        COL_EQ,            // :=
        COL_COL,           // ::
        DOT_DOT,           // ..
        DOT_DOT_LT,        // ..<
        DOT_DOT_EQ,        // ..=
        TLD_DOT,           // ~.
        DOT_DOT_DOT,       // ...
        MINUS_MINUS_MINUS, // ---

        // End of file
        TK_EOF
    };

    struct Token {
        u16       Kind;
        SourceLoc Loc;

        Token() : Kind(0) {}

        Token(u16 Kind, SourceLoc Loc)
            : Kind(Kind), Loc(Loc) {}

        llvm::StringRef GetText() const { return Loc.Text; }

        // Checks if the token is of the given kind.
        inline bool Is(u16 Kind) const { return this->Kind == Kind; }

        // Checks if the token is not of the given kind.
        inline bool IsNot(u16 Kind) const { return this->Kind != Kind; }

        inline bool IsKeyword() const {
            return Kind >= TokenKind::__KW_START__ && Kind <= TokenKind::__KW_END__;
        }

        static std::string TokenKindToString(u16 Kind, ArcoContext& Context);

        // Retrieves a presentation string in the
        // form (kind, 'text').
        std::string GetPresentationString(ArcoContext& Context) const;

    };

}

#endif // ARCO_TOKENS_H