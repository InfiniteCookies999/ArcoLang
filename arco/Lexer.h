#ifndef ARCO_LEXER_H
#define ARCO_LEXER_H

#include "Tokens.h"
#include "Logger.h"

namespace arco {

	class ArcoContext;

	class Lexer {
	public:

		explicit Lexer(ArcoContext& Context, Logger& Log, const SourceBuf Buffer);

		Token NextToken();

		ulen GetLinesLexed() const { return LineNumber; }

	private:
		const char* CurPtr;
		ulen        LineNumber = 1;

		ArcoContext& Context;
		Logger&      Log;
	
		/// The next token is either a
		/// keyword or an identifier.
		///
		Token NextWord();

		/// The next token is a number.
		/// 
		Token NextNumber();
		Token FinishNumber(const char* TokStart, TokenKind Kind);

		/// Next token is text surounded by quotes.
		///
		Token NextString();

		/// Next token is a character literal in '' quotes.
		///
		Token NextChar();

		/// Continues eating characters until a new line
		/// (or EOF) but does not consume the new line.
		void EatTillEndOfLine();

		/// Comments of the form:
		/// '/* This may span
		///     multiple lines
		///  */'
		void EatMultilineComment();

		// Utility
		//

		inline llvm::StringRef CreateText(const char* TokStart) const {
			return llvm::StringRef(TokStart, CurPtr - TokStart);
		}

		inline Token CreateToken(u16 Kind, const char* TokStart) const {
			return Token(Kind, SourceLoc{ CreateText(TokStart), LineNumber });
		}

		inline Token CreateTokenAndEat(u16 Kind, const char* TokStart) {
			++CurPtr;
			return CreateToken(Kind, TokStart);
		}

		inline Token CreateToken(u16 Kind, llvm::StringRef Text) const {
			return Token(Kind, SourceLoc{ Text, LineNumber });
		}

		void Error(SourceLoc Loc, const char* Msg) {
			Log.BeginError(Loc, Msg);
			Log.EndError();
		}

		template<typename... TArgs>
		void Error(const char* CharPos, const char* Fmt, TArgs&&... Args) {
			Log.BeginError(SourceLoc{ llvm::StringRef(CharPos, 1), LineNumber },
				Fmt, std::forward<TArgs>(Args)...);
			Log.EndError();
		}
	};

}
#endif // ARCO_LEXER_H