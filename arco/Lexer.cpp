#include "Lexer.h"

#include "Context.h"

arco::Lexer::Lexer(ArcoContext& Context, Logger& Log ,const SourceBuf Buffer)
    : Context(Context), Log(Log), CurPtr(Buffer.Memory)
{}

arco::Token arco::Lexer::NextToken() {

    // When skipping tokens this label is used
    // to jumps back and start over.
restartLex:

    const char* TokStart = CurPtr;

    switch (*CurPtr++) {
        // Whitespace
    case ' ':
    case '\t':
    case '\v':
    case '\f':
        goto restartLex;
    case '\n':
        ++LineNumber;
        goto restartLex;
    case '\r':
        ++LineNumber;
        if (*CurPtr == '\n') {
            ++CurPtr;
        }
        goto restartLex;
    case 'a': case 'b': case 'c': case 'd': case 'e':
    case 'f': case 'g': case 'h': case 'i': case 'j':
    case 'k': case 'l': case 'm': case 'n': case 'o':
    case 'p': case 'q': case 'r': case 's': case 't':
    case 'u': case 'v': case 'w': case 'x': case 'y':
    case 'z':
    case 'A': case 'B': case 'C': case 'D': case 'E':
    case 'F': case 'G': case 'H': case 'I': case 'J':
    case 'K': case 'L': case 'M': case 'N': case 'O':
    case 'P': case 'Q': case 'R': case 'S': case 'T':
    case 'U': case 'V': case 'W': case 'X': case 'Y':
    case 'Z':
    case '_':
        return NextWord();
    case '0': case '1': case '2': case '3': case '4':
    case '5': case '6': case '7': case '8': case '9':
        return NextNumber();
    case '"':
        return NextString();
    case '\'':
        return NextChar();
    case '(': return CreateToken('(', TokStart);
    case ')': return CreateToken(')', TokStart);
    case '{': return CreateToken('{', TokStart);
    case '}': return CreateToken('}', TokStart);
    case '[': return CreateToken('[', TokStart);
    case ']': return CreateToken(']', TokStart);
    case ';': return CreateToken(';', TokStart);
    case ',': return CreateToken(',', TokStart);
    case '?': return CreateToken('?', TokStart);
    case '.':
        if (*CurPtr == '.') {
            ++CurPtr;
            if (*CurPtr == '.')      return CreateTokenAndEat(TokenKind::DOT_DOT_DOT, TokStart);
            else if (*CurPtr == '<') return CreateTokenAndEat(TokenKind::DOT_DOT_LT, TokStart);
            else if (*CurPtr == '=') return CreateTokenAndEat(TokenKind::DOT_DOT_EQ, TokStart);
            else                     return CreateToken(TokenKind::DOT_DOT, TokStart);
        }
        else                    return CreateToken('.', TokStart);
    case ':':
        if (*CurPtr == '=')      return CreateTokenAndEat(TokenKind::COL_EQ, TokStart);
        else if (*CurPtr == ':') return CreateTokenAndEat(TokenKind::COL_COL, TokStart);
        else                     return CreateToken(':', TokStart);
    case '+':
        if (*CurPtr == '+')      return CreateTokenAndEat(TokenKind::PLUS_PLUS, TokStart);
        else if (*CurPtr == '=') return CreateTokenAndEat(TokenKind::PLUS_EQ, TokStart);
        else                     return CreateToken('+', TokStart);
    case '-':
        if (*CurPtr == '-') {
            ++CurPtr;
            if (*CurPtr == '-')  return CreateTokenAndEat(TokenKind::MINUS_MINUS_MINUS, TokStart);
            else                 return CreateToken(TokenKind::MINUS_MINUS, TokStart);
        }
        else if (*CurPtr == '=') return CreateTokenAndEat(TokenKind::MINUS_EQ, TokStart);
        else                     return CreateToken('-', TokStart);
    case '*':
        if (*CurPtr == '=') return CreateTokenAndEat(TokenKind::STAR_EQ, TokStart);
        else                return CreateToken('*', TokStart);
    case '/':
        if (*CurPtr == '/') {
            EatTillEndOfLine();
            goto restartLex;
        } else if (*CurPtr == '*') {
            EatMultilineComment();
            goto restartLex;
        } else if (*CurPtr == '=') return CreateTokenAndEat(TokenKind::SLASH_EQ, TokStart);
        else return CreateToken('/', TokStart);
    case '%':
        if (*CurPtr == '=') return CreateTokenAndEat(TokenKind::MOD_EQ, TokStart);
        else                return CreateToken('%', TokStart);
    case '=':
        if (*CurPtr == '=') return CreateTokenAndEat(TokenKind::EQ_EQ, TokStart);
        else                return CreateToken('=', TokStart);
    case '!':
        if (*CurPtr == '=') return CreateTokenAndEat(TokenKind::EXL_EQ, TokStart);
        else                return CreateToken('!', TokStart);
    case '&':
        if (*CurPtr == '=')      return CreateTokenAndEat(TokenKind::AMP_EQ, TokStart);
        else if (*CurPtr == '&') return CreateTokenAndEat(TokenKind::AMP_AMP, TokStart);
        else                     return CreateToken('&', TokStart);
    case '|':
        if (*CurPtr == '=')      return CreateTokenAndEat(TokenKind::BAR_EQ, TokStart);
        else if (*CurPtr == '|') return CreateTokenAndEat(TokenKind::BAR_BAR, TokStart);
        else                     return CreateToken('|', TokStart);
    case '^':
        if (*CurPtr == '=') return CreateTokenAndEat(TokenKind::CRT_EQ, TokStart);
        else                return CreateToken('^', TokStart);
    case '~': return CreateToken('~', TokStart);
    case '<':
        if (*CurPtr == '<') {
            ++CurPtr;
            if (*CurPtr == '=') return CreateTokenAndEat(TokenKind::LT_LT_EQ, TokStart);
            else                return CreateToken(TokenKind::LT_LT, TokStart);
        } else if (*CurPtr == '=') return CreateTokenAndEat(TokenKind::LT_EQ, TokStart);
        else                       return CreateToken('<', TokStart);
    case '>':
        if (*CurPtr == '>') {
            ++CurPtr;
            if (*CurPtr == '=') return CreateTokenAndEat(TokenKind::GT_GT_EQ, TokStart);
            else                return CreateToken(TokenKind::GT_GT, TokStart);
        } else if (*CurPtr == '=') return CreateTokenAndEat(TokenKind::GT_EQ, TokStart);
        else return CreateToken('>', TokStart);

    case '\0': {
        // Move the cursor back one in case NextToken() is called again so that
        // the lexer can continue to return EOF as to the parser.
        --CurPtr;
        return CreateToken(TokenKind::TK_EOF, TokStart);
    }
    default: {
        unsigned char UnknownChar = *TokStart;
        if (UnknownChar >= 33 && UnknownChar <= 126) // Easily displayable ASCII character.
            Error(TokStart, "Invalid character: '%s'", *TokStart);
        else
            Error(TokStart, "Invalid character (as decimal): '%s'", static_cast<u32>(UnknownChar));
        goto restartLex;
    }
    }

    return Token();
}

arco::Token arco::Lexer::NextWord() {
    const char* TokStart = CurPtr - 1; // -1 since initial switch consumes one character.

    while (IsAlpha(*CurPtr) || IsDigit(*CurPtr) || *CurPtr == '_') {
        ++CurPtr;
    }

    llvm::StringRef Text = CreateText(TokStart);
    // Checking if it is a keyword.
    TokenKind KeywordKind = Context.GetKeywordKind(Text);
    if (KeywordKind != TokenKind::INVALID) {
        return CreateToken(KeywordKind, Text);
    }

    // Not a keyword, so it is an identifier.
    return CreateToken(TokenKind::IDENT, Text);
}

arco::Token arco::Lexer::NextNumber() {
    
    const char* TokStart = CurPtr - 1;  // -1 since initial switch table consumes one character.
    
    if (*TokStart == '0') {
        if (*CurPtr == 'x') {
            ++CurPtr; // Eating 'x'.
            while (IsHex(*CurPtr) || *CurPtr == NUMBER_SEPERATOR) {
                ++CurPtr;
            }
            return FinishNumber(TokStart, TokenKind::HEX_LITERAL);
        } else if (*CurPtr == 'b') {
            ++CurPtr; // Eating 'b'.
            while (*CurPtr == '0' || *CurPtr == '1' || *CurPtr == NUMBER_SEPERATOR) {
                ++CurPtr;
            }
            return FinishNumber(TokStart, TokenKind::BIN_LITERAL);
        }
    }

    bool EncounteredError = false;

    // Leading whole digits [0-9']+
    while (IsDigit(*CurPtr) || *CurPtr == NUMBER_SEPERATOR) {
        ++CurPtr;
    }

    bool IsFloating = false;
    if ((*CurPtr == '.' && !(*(CurPtr +1) == '.')) ||
        *CurPtr == 'E' || *CurPtr == 'e') {
        IsFloating = true;
        if (*CurPtr == '.') {
            ++CurPtr; // Eating '.'.
            // Fractional digits [0-9]+
            while (IsDigit(*CurPtr) || *CurPtr == NUMBER_SEPERATOR) {
                ++CurPtr;
            }
        }

        if (*CurPtr == 'E' || *CurPtr == 'e') {
            ++CurPtr; // Eating 'E' or 'e'.

            // Possible exponent sign
            if (*CurPtr == '+' || *CurPtr == '-') {
                ++CurPtr;
            }

            bool EncounteredExpDigits = false;
            // Eating exponent digits
            while (IsDigit(*CurPtr) || *CurPtr == NUMBER_SEPERATOR) {
                if (*CurPtr == NUMBER_SEPERATOR) {
                    EncounteredError = true;
                    Error(CurPtr, "Numeric seperators do not belong in the exponent");
                } else {
                    EncounteredExpDigits = true;
                }
                ++CurPtr;
            }
            if (!EncounteredExpDigits) {
                Error(CurPtr, "Expected digits for exponent");
                EncounteredError = true;
            }
        }
    }

    if (IsFloating) {
        bool Is64Bit = true;
        if (*CurPtr == 'f' || *CurPtr == 'F') {
            // The expected type is a 32 bit floating number.
            ++CurPtr;
            Is64Bit = false;
        } else if (*CurPtr == 'd' || *CurPtr == 'D') {
            // The expected type is a 64 bit floating number.
            ++CurPtr;
        }

        if (EncounteredError) {
            return CreateToken(TokenKind::ERROR_FLOAT_LITERAL, CreateText(TokStart));
        } else if (Is64Bit) {
            return CreateToken(TokenKind::FLOAT64_LITERAL, CreateText(TokStart));
        } else {
            return CreateToken(TokenKind::FLOAT32_LITERAL, CreateText(TokStart));
        }
    } else {
        return FinishNumber(TokStart, TokenKind::INT_LITERAL);
    }
}

arco::Token arco::Lexer::FinishNumber(const char* TokStart, TokenKind Kind) {
    if (*CurPtr == '\'') {
        ++CurPtr;

        switch (*CurPtr) {
        case 'i':
        case 'u':
            ++CurPtr;
            if (*CurPtr == '8') {
                CurPtr += 1;
            } else if (*CurPtr == '1' && *(CurPtr + 1) == '6') {
                CurPtr += 2;
            } else if (*CurPtr == '3' && *(CurPtr + 1) == '2') {
                CurPtr += 2;
            } else if (*CurPtr == '6' && *(CurPtr + 1) == '4') {
                CurPtr += 2;
            } else {
                Error(CurPtr-1, "Expected to find valid type information");
            }
            break;
        default:
            Error(CurPtr-1, "Expected to find valid type information");
            break;
        }
    }
    
    return CreateToken(Kind, CreateText(TokStart));
}

arco::Token arco::Lexer::NextString() {
    const char* TokStart = CurPtr - 1;
    while (true) {
        switch (*CurPtr) {
        case '"':
        case 0:    // EOF.
        case '\n':
        case '\r':
        case '\v':
        case '\f':
            goto finishedParsingStringLab;
        case '\\':
            // TODO:
            // want to check for unicode so the parser can
            // preallocate the correct type of string and then
            // simply parse the characters of the given unicode.
            if (*(++CurPtr) == '"')
                ++CurPtr;
            break;
        default:
            ++CurPtr;
            break;
        }
    }
finishedParsingStringLab:

    if (*CurPtr != '"') {
        Error(CurPtr-1, "Expected closing quotation for string literal");
    } else {
        ++CurPtr;
    }

    return CreateToken(TokenKind::STRING_LITERAL, TokStart);
}

arco::Token arco::Lexer::NextChar() {
    const char* TokStart = CurPtr - 1;

    if (*CurPtr == '\\') {
        // TODO: Add support for unicode characters
        ++CurPtr;
        switch (*CurPtr) {
        case '\t': case '\r':
        case '\n': case '\v':
        case '\f': case '\0':
            break;
        default:
            ++CurPtr; // Eating the character after the backslash
            break;
        }
    } else {
        switch (*CurPtr) {
        case '\t': case '\r':
        case '\n': case '\v':
        case '\f': case '\0':
            break;
        default:
            ++CurPtr; // Eating the character inside ''
            break;
        }
    }

    if (*CurPtr == '\'') {
        ++CurPtr; // Eating closing '
    } else {
        Error(CurPtr-1, "Expected closing quote for character");
    }

    return CreateToken(TokenKind::CHAR_LITERAL, TokStart);
}

void arco::Lexer::EatTillEndOfLine() {
    // Keeps eating characters until the new line
    while (true) {
        switch (*CurPtr) {
        case '\n':
        case '\r':
        case '\0': // EOF
            return;
        default:
            // Just advance
            ++CurPtr;
            break;
        }
    }
}

void arco::Lexer::EatMultilineComment() {
    ++CurPtr; // Consuming '*' character
    while (true) {
        switch (*CurPtr) {
        case '\0':
            Error(CurPtr-1, "Unexpected end of file. Expected comment to be closed with */");
            return;
        case '\n':
            ++LineNumber;
            ++CurPtr;
            break;
        case '\r':
            ++LineNumber;
            if (*(++CurPtr) == '\n') {
                ++CurPtr;
            }
            break;
        case '*':
            if (*(++CurPtr) == '/') {
                ++CurPtr; // Eating '/' character
                // End of multiline comment
                return;
            }
            break;
        default:
            // Just advance
            ++CurPtr;
            break;
        }
    }
}
