#include "Tokens.h"

#include "Context.h"

std::string arco::Token::TokenKindToString(u16 Kind, ArcoContext& Context) {
    switch (Kind) {
    case TokenKind::IDENT:          return "ident";
    case TokenKind::INT_LITERAL:    return "int-literal";
    case TokenKind::HEX_LITERAL:    return "hex-literal";
    case TokenKind::BIN_LITERAL:    return "bin-literal";
    case TokenKind::CHAR_LITERAL:   return "char-literal";
    case TokenKind::FLOAT32_LITERAL:  return "float32-literal";
    case TokenKind::FLOAT64_LITERAL:  return "float64-literal";
    case TokenKind::ERROR_FLOAT_LITERAL: return "error";
    case TokenKind::STRING_LITERAL: return "string-literal";
    case TokenKind::TK_EOF:         return "eof";
    case TokenKind::PLUS_PLUS:      return "++";
    case TokenKind::MINUS_MINUS:    return "--";
    case TokenKind::PLUS_EQ:        return "+=";
    case TokenKind::MINUS_EQ:       return "-=";
    case TokenKind::SLASH_EQ:       return "/=";
    case TokenKind::STAR_EQ:        return "*=";
    case TokenKind::MOD_EQ:	        return "%=";
    case TokenKind::AMP_EQ:	        return "&=";
    case TokenKind::BAR_EQ:	        return "|=";
    case TokenKind::CRT_EQ:	        return "^=";
    case TokenKind::LT_LT:	        return "<<";
    case TokenKind::GT_GT:	        return ">>";
    case TokenKind::LT_LT_EQ:       return "<<=";
    case TokenKind::GT_GT_EQ:       return ">>=";
    case TokenKind::LT_EQ:          return "<=";
    case TokenKind::GT_EQ:          return ">=";
    case TokenKind::AMP_AMP:        return "&&";
    case TokenKind::BAR_BAR:        return "||";
    case TokenKind::EQ_EQ:          return "==";
    case TokenKind::EXL_EQ:         return "!=";
    case TokenKind::COL_EQ:         return ":=";
    case TokenKind::COL_COL:        return "::";
    case TokenKind::DOT_DOT:        return "..";
    case TokenKind::DOT_DOT_DOT:    return "...";
    case TokenKind::DOT_DOT_EQ:     return "..=";
    case TokenKind::DOT_DOT_LT:     return "..<";
    case TokenKind::MINUS_MINUS_MINUS: return "---";
    default:
        if (static_cast<u32>(Kind) < 256) {
            u32 UTF8Kind = static_cast<u32>(Kind);
            if (UTF8Kind >= 33 && UTF8Kind <= 126) // Easily displayable ASCII character.
                return std::string(1, static_cast<char>(Kind));
            else
                return std::to_string(UTF8Kind);
        }
        if (Kind >= TokenKind::__KW_START__ && Kind <= TokenKind::__KW_END__) {
            return Context.GetKeywordAsString(Kind).str();
        }
        return "";
    }
}

std::string arco::Token::GetPresentationString(ArcoContext& Context) const {
    return std::string("(") +  Token::TokenKindToString(Kind, Context) +
        ", '" + GetText().str() + "')";
}
