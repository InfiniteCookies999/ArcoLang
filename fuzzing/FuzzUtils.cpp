#include "FuzzUtils.h"

#include <unordered_map>
#include <assert.h>
#include <llvm/Support/raw_ostream.h>

#include <Tokens.h>

std::string GenRandomIdentLiteral() {
    int Length = (rand() % 30) + 1;
    std::string Ident;
    for (int i = 0; i < Length; i++) {
        int CharIdx = rand() % 27;
        if (CharIdx == 0) {
            Ident += '_';
        } else {
            bool IsCap = rand() % 2 == 0;
            if (IsCap) {
                Ident += (char)((CharIdx - 1) + 'A');
            } else {
                Ident += (char)((CharIdx - 1) + 'a');
            }
        }
    }

    return Ident;
}

std::string GenRandomIntLiteral(int Base, int Limit) {
    int Number = rand() % Limit;
    std::string Result;
    static std::unordered_map<int, char> DecToHex = {
        { 0, '0' }, { 1, '1' }, { 2, '2' }, { 3, '3' }, { 4, '4' },
        { 5, '5' }, { 6, '6' }, { 7, '7' }, { 8, '8' }, { 9, '9' },
        { 10, 'a' }, { 11, 'b' }, { 12, 'c' }, { 13, 'd' }, { 14, 'e' },
        { 15, 'f' }
    };
    if (Number == 0) {
        Result = "0";
    }
    while (Number) {
        int Rem = Number % Base;
        char Digit = 0;
        if (Base == 10) {
            Digit = Rem + '0';
        } else if (Base == 16) {
            Digit += DecToHex[Rem];
        } else if (Base == 2) {
            Digit = Rem == 0 ? '0' : '1';
        } else {
            assert(!"unreachable");
        }

        Result += Digit;
        Number /= Base;
    }
    std::reverse(Result.begin(), Result.end());
    if (Base == 16) {
        Result = "0x" + Result;
    } else if (Base == 2) {
        Result = "0b" + Result;
    }

    return Result;
}

std::string GenRandomCharChar() {
    int Index = 32 + (rand() % (94 + 12));
    if (Index - 32 <= 11) {
        switch (Index - 32) {
        case 0:  return "\\\\";
        case 1:  return "\\n"; 
        case 2:  return "\\t"; 
        case 3:  return "\\0"; 
        case 4:  return "\\\"";
        case 5:  return "\\a"; 
        case 6:  return "\\r"; 
        case 7:  return "\\v"; 
        case 8:  return "\\b"; 
        case 9:  return "\\f"; 
        case 10: return "\\?"; 
        case 11: return "\\'"; 
        default:
            assert(!"unreachable");
        }
    } else {
        char c = (char)(Index - 12);
        if (c == '\'') {
            return "\\'";
        } else if (c == '\\') {
            return "\\\\";
        } else if (c == '"') {
            return "\\\"";
        }
        return std::string(1, c);
    }
}

std::string GenRandomCharLiteral() {
    std::string Result = "'";
    Result += GenRandomCharChar();
    Result += "'";
    return Result;
}

std::string GenRandomStringLiteral() {
    std::string Result = "\"";
    int Length = rand() % 10;
    for (int i = 0; i < Length; i++) {
        Result += GenRandomCharChar();
    }
    Result += "\"";
    return Result;
}

std::string GenRandomFloatLiteral() {
    std::string Result;
    std::string WholeDigits = GenRandomIntLiteral(10);
    Result += WholeDigits;
    bool HasDot = rand() % 2 == 0;
    if (HasDot) {
        Result += ".";
        Result += GenRandomIntLiteral(10);
    }
    bool HasExponent = (rand() % 2 == 0) || !HasDot;
    if (HasExponent) {
        Result += "e";
        bool PosExp = rand() % 2 == 0;
        if (!PosExp) {
            Result += "-";
        }
        std::string ExpDigits = GenRandomIntLiteral(10, 200);
        Result += ExpDigits;
    }

    return Result;
}

std::string BuildTokenString(arco::ArcoContext& Context, const FuzzToken& Tok) {
    u16 Kind = Tok.Kind;

    std::string AsString;
    if (Kind == FUZZ_BUILT_TOKEN_KIND) {
        AsString = Tok.Lexeme;
    } else if (Kind == arco::TokenKind::IDENT) {
        AsString = GenRandomIdentLiteral();
    } else if (Kind == arco::TokenKind::INT_LITERAL) {
        AsString = GenRandomIntLiteral(10);
    } else if (Kind == arco::TokenKind::HEX_LITERAL) {
        AsString = GenRandomIntLiteral(16);
    } else if (Kind == arco::TokenKind::BIN_LITERAL) {
        AsString = GenRandomIntLiteral(2);
    } else if (Kind == arco::TokenKind::CHAR_LITERAL) {
        AsString = GenRandomCharLiteral();
    } else if (Kind == arco::TokenKind::STRING_LITERAL) {
        AsString = GenRandomStringLiteral();
    } else if (Kind == arco::TokenKind::FLOAT32_LITERAL ||
               Kind == arco::TokenKind::FLOAT64_LITERAL ||
               Kind == arco::TokenKind::ERROR_FLOAT_LITERAL
        ) {
        AsString = GenRandomFloatLiteral();
    } else {
        AsString = arco::Token::TokenKindToString(Kind, Context);
    }
    assert(!AsString.empty());
    
    return AsString;
}

void WriteTokensToFile(const llvm::SmallVector<FuzzToken>& Tokens,
                       std::ofstream& FileStream,
                       arco::ArcoContext& Context) {
    int Count = 0;
    for (const FuzzToken& Tok : Tokens) {
        std::string AsString = BuildTokenString(Context, Tok);
        
        for (char c : AsString) {
            FileStream << c;
        }

        ++Count;
        if (Count % 10 == 0) {
            FileStream << "\n";
        } else {
            FileStream << " ";
        }
    }
}
