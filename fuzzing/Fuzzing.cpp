#include <Compiler.h>
#include <Logger.h>
#include <Tokens.h>
#include <Context.h>

#include <fstream>

#include "FuzzUtils.h"
#include "Level3ParserFuzzing.h"

void GenLevel0LexFuzz() {

    int NumberOfChars = 10000;
    std::ofstream FileStream("fuzz_gen.arco");

    for (int i = 0; i < NumberOfChars; i++) {
        char RandomChar = (char)(rand() % 256);
        FileStream << RandomChar;
    }
}

void GenLevel1LexFuzz() {

    int NumberOfChars = 10000;
    std::ofstream FileStream("fuzz_gen.arco");

    llvm::SmallVector<char> ValidCharacters;
    for (char c = '0'; c <= '9'; c++) {
        ValidCharacters.push_back(c);
    }
    for (char c = 'A'; c <= 'Z'; c++) {
        ValidCharacters.push_back(c);
    }
    for (char c = 'a'; c <= 'z'; c++) {
        ValidCharacters.push_back(c);
    }
    ValidCharacters.push_back(' ');
    ValidCharacters.push_back('!');
    ValidCharacters.push_back('"');
    ValidCharacters.push_back('%');
    ValidCharacters.push_back('&');
    ValidCharacters.push_back('\'');
    ValidCharacters.push_back('(');
    ValidCharacters.push_back(')');
    ValidCharacters.push_back('*');
    ValidCharacters.push_back('+');
    ValidCharacters.push_back(',');
    ValidCharacters.push_back('-');
    ValidCharacters.push_back('.');
    ValidCharacters.push_back('/');
    ValidCharacters.push_back(':');
    ValidCharacters.push_back(';');
    ValidCharacters.push_back('<');
    ValidCharacters.push_back('=');
    ValidCharacters.push_back('>');
    ValidCharacters.push_back('?');
    ValidCharacters.push_back('[');
    ValidCharacters.push_back(']');
    ValidCharacters.push_back('|');
    ValidCharacters.push_back('{');
    ValidCharacters.push_back('}');
    ValidCharacters.push_back('~');
    ValidCharacters.push_back('\n');
    ValidCharacters.push_back('\t');

    for (int i = 0; i < NumberOfChars; i++) {
        char c = ValidCharacters[rand() % ValidCharacters.size()];
        FileStream << c;
    }
}

void GenLevel2ParseFuzz(arco::ArcoContext& Context) {

    llvm::SmallVector<u16> ValidTokenKinds;
    ValidTokenKinds.push_back('!');
    ValidTokenKinds.push_back('%');
    ValidTokenKinds.push_back('&');
    ValidTokenKinds.push_back('(');
    ValidTokenKinds.push_back(')');
    ValidTokenKinds.push_back('*');
    ValidTokenKinds.push_back('+');
    ValidTokenKinds.push_back(',');
    ValidTokenKinds.push_back('-');
    ValidTokenKinds.push_back('.');
    ValidTokenKinds.push_back('/');
    ValidTokenKinds.push_back(':');
    ValidTokenKinds.push_back(';');
    ValidTokenKinds.push_back('<');
    ValidTokenKinds.push_back('=');
    ValidTokenKinds.push_back('>');
    ValidTokenKinds.push_back('?');
    ValidTokenKinds.push_back('[');
    ValidTokenKinds.push_back(']');
    ValidTokenKinds.push_back('|');
    ValidTokenKinds.push_back('{');
    ValidTokenKinds.push_back('}');
    ValidTokenKinds.push_back('~');

    for (u16 KeyW = arco::TokenKind::__KW_START__; KeyW <= arco::TokenKind::__KW_END__; KeyW++) {
        ValidTokenKinds.push_back(KeyW);
    }

#define ADD_TOKEN(K) \
    ValidTokenKinds.push_back(arco::TokenKind::K);

    ADD_TOKEN(IDENT);
    ADD_TOKEN(INT_LITERAL);
    ADD_TOKEN(HEX_LITERAL);
    ADD_TOKEN(BIN_LITERAL);
    ADD_TOKEN(CHAR_LITERAL);
    ADD_TOKEN(FLOAT32_LITERAL);
    ADD_TOKEN(FLOAT64_LITERAL);
    ADD_TOKEN(ERROR_FLOAT_LITERAL);
    ADD_TOKEN(STRING_LITERAL);
    ADD_TOKEN(PLUS_PLUS);
    ADD_TOKEN(MINUS_MINUS);
    ADD_TOKEN(POST_PLUS_PLUS);
    ADD_TOKEN(POST_MINUS_MINUS);
    ADD_TOKEN(PLUS_EQ);           // +=
    ADD_TOKEN(MINUS_EQ);          // -=
    ADD_TOKEN(SLASH_EQ);          // /=
    ADD_TOKEN(STAR_EQ);           // *=
    ADD_TOKEN(MOD_EQ);            // %=
    ADD_TOKEN(AMP_EQ);            // &=
    ADD_TOKEN(BAR_EQ);            // |=
    ADD_TOKEN(CRT_EQ);            // ^=
    ADD_TOKEN(LT_LT);             // <<
    ADD_TOKEN(GT_GT);             // >>
    ADD_TOKEN(LT_LT_EQ);          // <<=
    ADD_TOKEN(GT_GT_EQ);          // >>=
    ADD_TOKEN(LT_EQ);             // <=
    ADD_TOKEN(GT_EQ);             // >=
    ADD_TOKEN(AMP_AMP);           // &&
    ADD_TOKEN(BAR_BAR);           // ||
    ADD_TOKEN(EQ_EQ);             // ==
    ADD_TOKEN(EXL_EQ);            // !=
    ADD_TOKEN(COL_EQ);            // :=
    ADD_TOKEN(COL_COL);           // ::
    ADD_TOKEN(DOT_DOT);           // ..
    ADD_TOKEN(DOT_DOT_LT);        // ..<
    ADD_TOKEN(DOT_DOT_EQ);        // ..=
    ADD_TOKEN(DOT_DOT_DOT);       // ...
    ADD_TOKEN(MINUS_MINUS_MINUS); // ---

#undef ADD_TOKEN

    int NumberOfTokens = 5000;
    std::ofstream FileStream("fuzz_gen.arco");

    llvm::SmallVector<u16> TokenKinds;
    for (int i = 0; i < NumberOfTokens; i++) {
        u16 Kind = ValidTokenKinds[rand() % ValidTokenKinds.size()];
        TokenKinds.push_back(Kind);
    }
    WriteTokensToFile(TokenKinds, FileStream, Context);

}

int main() {

    srand(time(nullptr));
   
    arco::Compiler Compiler;
    Compiler.PreInitContext();
    Compiler.Stage = arco::Compiler::Stages::PARSE_SEMCHECK_COMPILE_ONLY;
    
    GenLevel3ParseFuzz(Compiler.Context);

    llvm::SmallVector<arco::Source> Sources;
    Sources.push_back(arco::Source{ true, "default.program.module", "fuzz_gen.arco" });

    arco::TOTAL_ALLOWED_ERRORS = 350;
    Compiler.StandAlone = true;
    Compiler.Compile(Sources);
    

}