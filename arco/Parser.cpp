#include "Parser.h"

#include <iostream>
#include <stack>

#include "Context.h"

namespace arco {
	template<typename N>
	static inline N* NewNode(Token STok) {
		N* Node = new N;
		Node->Loc = STok.Loc;
		return Node;
	}

	static inline BinaryOp* NewBinaryOp(Token OpTok, Expr* LHS, Expr* RHS) {
		BinaryOp* BinOp = NewNode<BinaryOp>(OpTok);
		BinOp->Op  = OpTok.Kind;
		BinOp->LHS = LHS;
		BinOp->RHS = RHS;
		return BinOp;
	}
	
	VarDecl* Parser::Scope::FindVariable(Identifier Name) {
		Scope* S = this;
		while (S) {
			auto Itr = S->VarDecls.find(Name);
			if (Itr != S->VarDecls.end()) {
				return Itr->second;
			}
			S = S->Parent;
		}
		return nullptr;
	}
}


#define PUSH_SCOPE()        \
Scope NewScope;             \
NewScope.Parent = LocScope; \
LocScope = &NewScope;

#define POP_SCOPE() \
LocScope = LocScope->Parent;


#define TYPE_KW_START_CASES \
     TokenKind::KW_INT:     \
case TokenKind::KW_UINT:    \
case TokenKind::KW_INT8:    \
case TokenKind::KW_INT16:   \
case TokenKind::KW_INT32:   \
case TokenKind::KW_INT64:   \
case TokenKind::KW_UINT8:   \
case TokenKind::KW_UINT16:  \
case TokenKind::KW_UINT32:  \
case TokenKind::KW_UINT64:  \
case TokenKind::KW_VOID:    \
case TokenKind::KW_CHAR:    \
case TokenKind::KW_CSTR

// P - Parsing code.
// L - The Source location to assign to.
#define CREATE_EXPANDED_SOURCE_LOC(P, L)           \
SourceLoc ExpandedStartLoc = CTok.Loc;             \
P;                                                 \
L.LineNumber = ExpandedStartLoc.LineNumber;        \
L.Text =                                           \
    llvm::StringRef(ExpandedStartLoc.Text.begin(), \
             PrevToken.GetText().end() - ExpandedStartLoc.Text.begin());
   

arco::Parser::Parser(ArcoContext& Context, Module* Mod, const char* FilePath, const SourceBuf FileBuffer)
	: Context(Context), Mod(Mod), Log(FilePath, FileBuffer), Lex(Context, Log, FileBuffer),
	  FScope(new FileScope( FilePath, FileBuffer ))
{
}

arco::FileScope* arco::Parser::Parse() {
	// TODO: If this ever becomes multithreaded this won't
	// work here. Same goes for any other uses of this.
	bool PrevNumErrors = TotalAccumulatedErrors;

	NextToken(); // Prime the parser.

	// Parsing imports.
	while (CTok.IsNot(TokenKind::TK_EOF)) {
		if (CTok.Is(TokenKind::KW_IMPORT)) {
			ParseImport();
		} else {
			break;
		}
	}

	// Parsing top level statements.
	while (CTok.IsNot(TokenKind::TK_EOF)) {
		AstNode* Stmt;
		ParseOptStmt(Stmt, TokenKind::TK_EOF);
		if (!Stmt) continue;

		if (Stmt->Is(AstKind::FUNC_DECL)) {
			FuncDecl* Func = static_cast<FuncDecl*>(Stmt);
			if (Func->Name == Context.MainIdentifier) {
				if (!Context.MainEntryFunc) {
					Context.MainEntryFunc = Func;
				} else {
					// Duplicate entry function.
					Error(Func->Loc,
						"Duplicate entry point found. First declared at: %s:%s",
						Context.MainEntryFunc->FScope->Path,
						Context.MainEntryFunc->Loc.LineNumber);
				}
			}

			Mod->Funcs[Func->Name].push_back(Func);
		} else if (Stmt->Is(AstKind::STRUCT_DECL)) {
			StructDecl* Struct = static_cast<StructDecl*>(Stmt);
			
			Mod->Structs[Struct->Name] = Struct;
		} else if (Stmt->Is(AstKind::VAR_DECL)) {
			VarDecl* Global = static_cast<VarDecl*>(Stmt);
			Global->IsGlobal = true;
			Context.UncheckedDecls.insert(Global);

			Mod->GlobalVars[Global->Name] = Global;
		} else {
			FScope->InvalidStmts.push_back({
				FileScope::InvalidScopeKind::GLOBAL,
				Stmt
				});
		}
	}

	if (PrevNumErrors != TotalAccumulatedErrors) {
		FScope->ParsingErrors = true;
	}

	return FScope;
}

void arco::Parser::ParseImport() {
	Token ImportTok = CTok;
	NextToken(); // Consuming 'import' token.

	Identifier ModName = ParseIdentifier("Expected identifier for import module name");
	Identifier StructName;

	if (CTok.Is('.')) {
		// Import must be for a struct.
		NextToken();
	
		StructName = ParseIdentifier("Expected identifier for struct name");
	}

	Match(';');

	auto Itr = Context.ModNamesToMods.find(ModName.Text);
	if (Itr == Context.ModNamesToMods.end()) {
		Error(ImportTok, "Could not find import module for '%s'", ModName.Text);
		return;
	}

	if (StructName.IsNull()) {
		if (FScope->ModImports.find(ModName) != FScope->ModImports.end()) {
			Error(ImportTok, "Duplicate import");
			return;
		}

		FScope->ModImports[ModName] = Itr->second;
	} else {
		if (FScope->StructImports.find(StructName) != FScope->StructImports.end()) {
			Error(ImportTok, "Already importing a struct with that given name");
			return;
		}

		FScope->StructImports[StructName] = {
			ImportTok.Loc,
			Itr->second,
			StructName
		};
	}
}

void arco::Parser::ParseOptStmt(AstNode*& Stmt, u16 TokenEndDelim) {
	while (CTok.Is(';'))
		NextToken(); // Consume any extra ';'.
	if (CTok.IsNot(TokenEndDelim) && CTok.IsNot(TokenKind::TK_EOF)) {
		Stmt = ParseStmt();
	}
}

arco::AstNode* arco::Parser::ParseStmt() {
	AstNode* Stmt;
	switch (CTok.Kind) {
	case TokenKind::KW_RETURN: Stmt = ParseReturn(); Match(';');      break;
	case TokenKind::KW_IF:     Stmt = ParseIf();                      break;
	case TokenKind::KW_LOOP:   Stmt = ParseLoop();                    break;
	case TokenKind::KW_CONTINUE:
	case TokenKind::KW_BREAK:  Stmt = ParseLoopControl(); Match(';'); break;
	case TokenKind::KW_NATIVE:
	case TokenKind::KW_CONST:
	case TokenKind::KW_PRIVATE: {
		Modifiers Mods = ParseModifiers();
		if (CTok.Is(TokenKind::KW_FN)) {
			return ParseFuncDecl(Mods);
		} else if (CTok.Is(TokenKind::IDENT)) {
			Stmt = ParseVarDecl(Mods);
			Match(';');
		} else {
			Error(CTok, "Expected declaration");
			SkipRecovery();
			return nullptr;
		}
		break;
	}
	case TokenKind::KW_FN:
		return ParseFuncDecl(0);
	case TokenKind::IDENT: {
		switch (PeekToken(1).Kind) {
		case IDENT:
		case TYPE_KW_START_CASES: {
			Stmt = ParseVarDecl(0);
			Match(';');
			break;
		}
		case TokenKind::KW_STRUCT:
			return ParseStructDecl(0);
		default:
			Stmt = ParseAssignmentAndExprs();
			Match(';');
			break;
		}
		break;
	}
	default:
		Stmt = ParseAssignmentAndExprs();
		Match(';');
		break;
	}
	return Stmt;
}

arco::FuncDecl* arco::Parser::ParseFuncDecl(Modifiers Mods) {
	
	ulen NumErrs = TotalAccumulatedErrors;

	NextToken(); // Consuming 'fn' keyword.

	FuncDecl* Func = NewNode<FuncDecl>(CTok);
	Func->Mod    = Mod;
	Func->FScope = FScope;
	Func->Name   = ParseIdentifier("Expected identifier for function declaration");
	Func->Mods   = Mods;

	Context.UncheckedDecls.insert(Func);

	CFunc = Func;

	PUSH_SCOPE();
	Match('(');
	if (CTok.IsNot(')')) {
		bool MoreParams = false;
		
		ulen ParamCount = 0;
		do {
			VarDecl* Param = ParseVarDecl(0);
			Param->ParamIdx = ParamCount++;
			Func->Params.push_back(Param);

			MoreParams = CTok.Is(',');
			if (MoreParams) {
				NextToken(); // Consuming ',' token
			}
		} while (MoreParams);
	}
	Match(')');

	if ((!(Mods & ModKinds::NATIVE) && CTok.IsNot('{')) ||
		  (Mods & ModKinds::NATIVE) && CTok.IsNot(';')) {
		Func->RetTy = ParseType();
	} else {
		Func->RetTy = Context.VoidType;
	}

	if (!(Func->Mods & ModKinds::NATIVE)) {
		ParseScopeStmts(Func->Scope);
	} else {
		Match(';');
	}
	POP_SCOPE();
	
	if (Func->RetTy == Context.VoidType) {
		if (Func->Scope.Stmts.empty() || Func->Scope.Stmts.back()->IsNot(AstKind::RETURN)) {
			// Implicit return.
			++Func->NumReturns;
		}
	}

	if (NumErrs != TotalAccumulatedErrors) {
		Func->ParsingError = true;
	}

	return Func;
}

arco::VarDecl* arco::Parser::ParseVarDecl(Modifiers Mods) {

	ulen NumErrs = TotalAccumulatedErrors;

	VarDecl* Var = NewNode<VarDecl>(CTok);
	Var->Mod    = Mod;
	Var->FScope = FScope;
	Var->Name   = ParseIdentifier("Expected identifier for variable declaration");
	Var->Mods   = Mods;

	Var->Ty = ParseType();

	if (LocScope && !Var->Name.IsNull()) {
		LocScope->VarDecls.insert({ Var->Name, Var });
	}

	if (CFunc) {
		CFunc->AllocVars.push_back(Var);
	}

	if (CTok.Is('=')) {
		NextToken(); // Consuming '=' token.
		Var->Assignment = ParseExpr();
	}

	if (NumErrs != TotalAccumulatedErrors) {
		Var->ParsingError = true;
	}

	return Var;
}

arco::StructDecl* arco::Parser::ParseStructDecl(Modifiers Mods) {

	ulen NumErrs = TotalAccumulatedErrors;

	StructDecl* Struct = NewNode<StructDecl>(CTok);
	Struct->Mod    = Mod;
	Struct->FScope = FScope;
	Struct->Name   = ParseIdentifier("Expected identifier for struct declaration");
	Struct->Mods   = Mods;
	Match(TokenKind::KW_STRUCT);

	Context.UncheckedDecls.insert(Struct);
	
	ulen FieldCount = 0;
	PUSH_SCOPE()
	Match('{');
	while (CTok.IsNot('}') && CTok.IsNot(TokenKind::TK_EOF)) {
		AstNode* Stmt;
		ParseOptStmt(Stmt, '}');
		if (!Stmt) continue;

		if (Stmt->Is(AstKind::VAR_DECL)) {
			VarDecl* Field = static_cast<VarDecl*>(Stmt);
			Field->FieldIdx = FieldCount++;
			Struct->Fields.push_back(Field);
		} else if (Stmt->Is(AstKind::FUNC_DECL)) {
			FuncDecl* Func = static_cast<FuncDecl*>(Stmt);
			if (!Func->Name.IsNull()) {
				Func->Struct = Struct;
				if (Func->Name == Struct->Name) {
					// TODO: Should it also go into the Funcs list?
					Func->IsConstructor = true;
					Struct->Constructors.push_back(Func);
					if (Func->Params.empty()) {
						Struct->DefaultConstructor = Func;
					}
				} else {
					Struct->Funcs[Func->Name].push_back(Func);
				}
			}
		} else {
			FScope->InvalidStmts.push_back({
				FileScope::InvalidScopeKind::STRUCT,
				Stmt
				});
		}
	}
	Match('}');
	POP_SCOPE()

	if (NumErrs != TotalAccumulatedErrors) {
		Struct->ParsingError = true;
	}

	return Struct;
}

void arco::Parser::ParseScopeStmts(LexScope& Scope) {
	Scope.StartLoc = CTok.Loc;
	Match('{');
	while (CTok.IsNot('}') && CTok.IsNot(TokenKind::TK_EOF)) {
		AstNode* Stmt = nullptr;
		ParseOptStmt(Stmt, '}');
		if (!Stmt) continue;
		
		Scope.Stmts.push_back(Stmt);
	}
	Scope.EndLoc = CTok.Loc;
	Match('}');
}

arco::ReturnStmt* arco::Parser::ParseReturn() {

	ReturnStmt* Ret = NewNode<ReturnStmt>(CTok);
	NextToken(); // Consuming 'return' token.

	if (CFunc) {
		++CFunc->NumReturns;
	}

	if (CTok.IsNot(';')) {
		Ret->Value = ParseExpr();
	}

	return Ret;
}

arco::IfStmt* arco::Parser::ParseIf() {
	IfStmt* If = NewNode<IfStmt>(CTok);
	NextToken(); // Consuming 'if' token
	
	AllowStructInitializer = false;
	If->Cond = ParseExpr();
	AllowStructInitializer = true;

	PUSH_SCOPE();
	ParseScopeStmts(If->Scope);
	POP_SCOPE();

	if (CTok.Is(TokenKind::KW_ELSE)) {
		NextToken(); // Consuming 'else' token
		if (CTok.Is(TokenKind::KW_IF)) {
			If->Else = ParseIf(); // 'else if'
		} else {
			If->Else = ParseNestedScope(); // 'else'
		}
	}

	return If;
}

arco::AstNode* arco::Parser::ParseLoop() {
	Token LoopTok = CTok;
	NextToken(); // Consuming 'loop' token.

	if (CTok.Is(';')) {
		return ParseRangeLoop(LoopTok);
	} else if (CTok.Is(TokenKind::IDENT)) {
		switch (PeekToken(1).Kind) {
		case TYPE_KW_START_CASES:
			return ParseRangeLoop(LoopTok);
		default:
			return ParsePredicateLoop(LoopTok);
		}
	} else {
		return ParsePredicateLoop(LoopTok);
	}
}

arco::LoopControlStmt* arco::Parser::ParseLoopControl() {
	LoopControlStmt* LoopControl = NewNode<LoopControlStmt>(CTok);
	
	if (CTok.Is(TokenKind::KW_CONTINUE)) {
		LoopControl->Kind = AstKind::CONTINUE;
	} else {
		LoopControl->Kind = AstKind::BREAK;
	}
		
	NextToken(); // Consuming 'break' or 'continue' token.
	return LoopControl;
}

arco::PredicateLoopStmt* arco::Parser::ParsePredicateLoop(Token LoopTok) {
	PredicateLoopStmt* Loop = NewNode<PredicateLoopStmt>(LoopTok);
	
	if (CTok.IsNot('{')) {
		AllowStructInitializer = false;
		Loop->Cond = ParseExpr();
		AllowStructInitializer = true;
	}

	PUSH_SCOPE();
	ParseScopeStmts(Loop->Scope);
	POP_SCOPE();
	
	return Loop;
}

arco::RangeLoopStmt* arco::Parser::ParseRangeLoop(Token LoopTok) {
	RangeLoopStmt* Loop = NewNode<RangeLoopStmt>(LoopTok);
	
	PUSH_SCOPE();
	if (CTok.IsNot(';')) {
		Loop->Decls.push_back(ParseVarDecl(0));
	}
	Match(';');

	if (CTok.IsNot(';')) {
		Loop->Cond = ParseExpr();
	}
	Match(';');

	if (CTok.IsNot('{')) {
		AllowStructInitializer = false;
		Loop->Incs.push_back(ParseAssignmentAndExprs());
		AllowStructInitializer = true;
	}

	ParseScopeStmts(Loop->Scope);
	POP_SCOPE();

	return Loop;
}

arco::NestedScopeStmt* arco::Parser::ParseNestedScope() {
	NestedScopeStmt* NestedScope = NewNode<NestedScopeStmt>(CTok);
	PUSH_SCOPE();
	ParseScopeStmts(NestedScope->Scope);
	POP_SCOPE();
	return NestedScope;
}

arco::Modifiers arco::Parser::ParseModifiers() {
	Modifiers Mods = 0;
	while (true) {
		switch (CTok.Kind) {
		case TokenKind::KW_NATIVE: {
			if (Mods & ModKinds::NATIVE)
				Error(CTok, "Duplicate modifier");
			Mods |= ModKinds::NATIVE;
			NextToken();
			break;
		}
		case TokenKind::KW_CONST: {
			if (Mods & ModKinds::CONST)
				Error(CTok, "Duplicate modifier");
			Mods |= ModKinds::CONST;
			NextToken();
			break;
		}
		case TokenKind::KW_PRIVATE: {
			if (Mods & ModKinds::PRIVATE)
				Error(CTok, "Duplicate modifier");
			Mods |= ModKinds::PRIVATE;
			NextToken();
			break;
		}
		default:
			return Mods;
	}
	}
}

arco::Type* arco::Parser::ParseType() {
	arco::Type* Ty = nullptr;
	
	switch (CTok.Kind) {
	case TokenKind::KW_INT:    Ty = Context.IntType;    NextToken(); break;
	case TokenKind::KW_UINT:   Ty = Context.UIntType;   NextToken(); break;
	case TokenKind::KW_INT8:   Ty = Context.Int8Type;   NextToken(); break;
	case TokenKind::KW_INT16:  Ty = Context.Int16Type;  NextToken(); break;
	case TokenKind::KW_INT32:  Ty = Context.Int32Type;  NextToken(); break;
	case TokenKind::KW_INT64:  Ty = Context.Int64Type;  NextToken(); break;
	case TokenKind::KW_UINT8:  Ty = Context.UInt8Type;  NextToken(); break;
	case TokenKind::KW_UINT16: Ty = Context.UInt16Type; NextToken(); break;
	case TokenKind::KW_UINT32: Ty = Context.UInt32Type; NextToken(); break;
	case TokenKind::KW_UINT64: Ty = Context.UInt64Type; NextToken(); break;
	case TokenKind::KW_CHAR:   Ty = Context.CharType;   NextToken(); break;
	case TokenKind::KW_VOID:   Ty = Context.VoidType;   NextToken(); break;
	case TokenKind::KW_CSTR:   Ty = Context.CStrType;   NextToken(); break;
	case TokenKind::IDENT: {
		Ty = StructType::Create(Identifier(CTok.GetText()), CTok.Loc, Context);
		NextToken();
		break;
	}
	default:
		Error(CTok, "Expected valid type");
		break;
	}

	// Parsing pointer type.
	if (CTok.Is('*')) {
		NextToken(); // Consuming '*' token
		ulen NumStars = 1;
		while (CTok.Is('*')) {
			NextToken(); // Consuming '*' token
			++NumStars;
		}
		for (ulen i = 0; i < NumStars; i++) {
			Ty = PointerType::Create(Ty, Context);
		}
	}

	if (CTok.Is('[')) {

		llvm::SmallVector<Expr*, 2>     LengthExprs;
		llvm::SmallVector<SourceLoc, 2> ExpandedErrorLocs;

		bool IsImplicit = false;
		bool AlreadyReportedImplicitError = false, EncounteredNonImplicit = false;
		while (CTok.Is('[')) {
			NextToken(); // Consuming '[' token.

			if (CTok.Is(']')) {
				IsImplicit = true;

				if (EncounteredNonImplicit && !AlreadyReportedImplicitError) {
					Error(CTok, "Implicit array subscripts require all subscripts to be implicit.");
					AlreadyReportedImplicitError = true;
				}

				LengthExprs.push_back(nullptr);
				ExpandedErrorLocs.push_back(SourceLoc{});

				// Size information will be taken on implicitly.
				NextToken(); // Consuming ']' token
			} else {
				EncounteredNonImplicit = true;
				
				SourceLoc ExpandedErrorLoc;
				CREATE_EXPANDED_SOURCE_LOC(
					LengthExprs.push_back(ParseExpr()),
					ExpandedErrorLoc
				)
				ExpandedErrorLocs.push_back(ExpandedErrorLoc);

				if (IsImplicit && !AlreadyReportedImplicitError) {
					Error(CTok, "Implicit array subscripts require all subscripts to be implicit.");
					AlreadyReportedImplicitError = true;
				}
			
				Match(']');
			}
		}

		for (long long i = LengthExprs.size() - 1; i >= 0; i--) {
			Ty = ArrayType::Create(Ty, LengthExprs[i], ExpandedErrorLocs[i], Context);
		}
	}

	return Ty;
}

//===-------------------------------===//
// Expressions
//===-------------------------------===//

arco::Expr* arco::Parser::ParseAssignmentAndExprs() {
	Expr* LHS = ParseExpr();
	switch (CTok.Kind) {
	case '=':
	case TokenKind::PLUS_EQ:
	case TokenKind::MINUS_EQ:
	case TokenKind::STAR_EQ:
	case TokenKind::SLASH_EQ:
	case TokenKind::MOD_EQ:
	case TokenKind::AMP_EQ:
	case TokenKind::BAR_EQ:
	case TokenKind::CRT_EQ:
	case TokenKind::LT_LT_EQ:
	case TokenKind::GT_GT_EQ: {
		Token OpTok = CTok;
		NextToken(); // Consuming assignment operator token
		Expr* E = ParseExpr();
		return NewBinaryOp(OpTok, LHS, E);
	}
	default:
		return ParseBinaryExpr(LHS);
	}
}

arco::Expr* arco::Parser::ParseExpr() {
	return ParseBinaryExpr(ParsePrimaryAndPostfixUnaryExpr());
}

arco::Expr* arco::Parser::ParseBinaryExpr(Expr* LHS) {
	// Since some operations have to be delayed
	// because of order of operations a stack
	// is formed keeping a backlog of those operations
	// that need to be processed later
	struct StackUnit {
		Token Op;
		Expr* E;
	};
	std::stack<StackUnit> OpStack;

	Token Op = CTok, NextOp;
	llvm::DenseMap<u16, u32>::iterator OpItr;
	while ((OpItr = Context.BinaryOpsPrecedence.find(Op.Kind))
		         != Context.BinaryOpsPrecedence.end()) {

		NextToken(); // Consuming the operator

		llvm::DenseMap<u16, u32>::iterator NextOpItr;
		Expr* RHS = ParsePrimaryAndPostfixUnaryExpr();
		NextOp = CTok;
		bool MoreOperators = (NextOpItr = Context.BinaryOpsPrecedence.find(NextOp.Kind))
			                           != Context.BinaryOpsPrecedence.end();
		if (MoreOperators && NextOpItr->second > OpItr->second) {
			// Delaying the operation until later since the next operator has a
			// higher precedence.
			StackUnit Unit = StackUnit{ Op, LHS };
			OpStack.push(Unit);
			LHS = RHS;
			Op  = NextOp;
		} else {
			LHS = NewBinaryOp(Op, LHS, RHS);

			while (!OpStack.empty()) {
				RHS = LHS;
				StackUnit Unit = OpStack.top();
				// Still possible to have the right side have higher precedence.
				if (MoreOperators &&
					Context.BinaryOpsPrecedence[NextOp.Kind] > Context.BinaryOpsPrecedence[Unit.Op.Kind]) {
					LHS = RHS;
					Op = NextOp;
					break;
				}

				OpStack.pop();
				LHS = Unit.E;

				// Apply the binary operator!
				LHS = NewBinaryOp(Unit.Op, LHS, RHS);
			}

			Op = CTok;
		}
	}
	return LHS;
}

arco::Expr* arco::Parser::ParsePrimaryAndPostfixUnaryExpr() {
	return ParsePrimaryAndPostfixUnaryExpr(ParsePrimaryExpr());
}

arco::Expr* arco::Parser::ParsePrimaryAndPostfixUnaryExpr(Expr* LHS) {
	if (CTok.Is(TokenKind::PLUS_PLUS)) {
		UnaryOp* UOP = NewNode<UnaryOp>(CTok);
		UOP->Op = TokenKind::POST_PLUS_PLUS;
		NextToken(); // Consuming the unary operator
		UOP->Value = LHS;
		return UOP;
	} else if (CTok.Is(TokenKind::MINUS_MINUS)) {
		UnaryOp* UOP = NewNode<UnaryOp>(CTok);
		UOP->Op = TokenKind::POST_MINUS_MINUS;
		NextToken(); // Consuming the unary operator
		UOP->Value = LHS;
		return UOP;
	} else {
		return LHS;
	}
}

arco::Expr* arco::Parser::ParsePrimaryExpr() {
	switch (CTok.Kind) {
	// ---- Literals ----
	case TokenKind::INT_LITERAL:     return ParseIntLiteral();
	case TokenKind::HEX_LITERAL:     return ParseHexLiteral();
	case TokenKind::BIN_LITERAL:     return ParseBinLiteral();
	case TokenKind::CHAR_LITERAL:    return ParseCharLiteral();
	case TokenKind::STRING_LITERAL:  return ParseStringLiteral();
	case TokenKind::IDENT: {

		if (AllowStructInitializer && PeekToken(1).Is('{')) {
			StructInitializer* StructInit = NewNode<StructInitializer>(CTok);

			Identifier StructName = Identifier(CTok.GetText());
			StructType* Ty = StructType::Create(StructName, CTok.Loc, Context);
			NextToken();

			NextToken(); // Consuming '{' token.
			StructInit->Ty = Ty;
			
			if (CTok.IsNot('}')) {
				ParseAggregatedValues(StructInit->Args, '}', true);
			}

			Match('}', "for struct initializer");

			return ParseIdentPostfix(StructInit);
		} else {

			IdentRef* IRef = NewNode<IdentRef>(CTok);
			IRef->Ident = Identifier(CTok.GetText());
			NextToken();

			// Even if the identifier is not in the current scope
			// of variable declarations it may be refering to
			// a function identifier, class, enum, ect.. so
			// no error is displayed and the process of determining
			// the identifier's declaration is determined during
			// semantic analysis.
			if (LocScope && IRef->Is(AstKind::IDENT_REF)) {
				if (VarDecl* FoundVar = LocScope->FindVariable(IRef->Ident)) {
					IRef->Var     = FoundVar;
					IRef->RefKind = IdentRef::RK::Var;
				}
			}

			return ParseIdentPostfix(IRef);
		}
	}
	// ---- Pre unary expressions ----
	case '-': case '+': {
		u16 Op = CTok.Kind;
		NextToken(); // Consuming the unary operator.

		Expr* E = ParsePrimaryExpr();

		if (E->Is(AstKind::NUMBER_LITERAL)) {
			// TODO: Restrict overflows?
		
			// No reason to create a new node since
			// it is simple enough to compute the result
			// right away.

			NumberLiteral* Num = static_cast<NumberLiteral*>(E);
			if (Op == '-') {
				switch (Num->Ty->GetKind()) {
				case TypeKind::Int8:
				case TypeKind::Int16:
				case TypeKind::Int32:
				case TypeKind::Int64:
				case TypeKind::Int:
					Num->SignedIntValue = -Num->SignedIntValue;
					break;
				case TypeKind::UnsignedInt8:
				case TypeKind::UnsignedInt16:
				case TypeKind::UnsignedInt32:
				case TypeKind::UnsignedInt64:
				case TypeKind::UnsignedInt:
					Num->UnsignedIntValue = -Num->UnsignedIntValue;
					break;
				default:
					assert(!"Failed to implement!");
					break;
				}
			} // else '+' operator does not modify
			return Num;
		}

		UnaryOp* UniOP = NewNode<UnaryOp>(CTok);
		UniOP->Op    = Op;
		UniOP->Value = E;
		return UniOP;
	}
	case '&': case '*':
	case TokenKind::PLUS_PLUS: case MINUS_MINUS: {
		if ((CTok.Kind == '*' && PeekToken(1).Kind == '&') ||
			(CTok.Kind == '&' && PeekToken(1).Kind == '*')
			) {
			NextToken();
			NextToken();
			return ParsePrimaryExpr();
		}

		UnaryOp* UniOP = NewNode<UnaryOp>(CTok);
		UniOP->Op = CTok.Kind;

		NextToken(); // Consuming the unary operator

		UniOP->Value = ParsePrimaryExpr();
		return UniOP;
	}
	case TokenKind::KW_CAST: {
		TypeCast* Cast = NewNode<TypeCast>(CTok);
		NextToken(); // Consuming 'cast' token.
		Match('(');
		Cast->ToType = ParseType();
		Match(')');
		Cast->Value = ParsePrimaryAndPostfixUnaryExpr();
		return Cast;
	}
	case '[':
		return ParseArray();
	case '(': {
		NextToken(); // Consuming '(' token.
		bool PrevAllowStructInitializer = AllowStructInitializer;
		AllowStructInitializer = true;
		Expr* E = ParseExpr();
		AllowStructInitializer =PrevAllowStructInitializer;
		Match(')');
		return ParseIdentPostfix(E);
	}
	case TokenKind::KW_NULL: {
		NullPtr* Null = NewNode<NullPtr>(CTok);
		NextToken(); // Consuming 'null'
		Null->Ty = Context.NullType;
		return Null;
	}
	case TokenKind::KW_THIS: {
		ThisRef* This = NewNode<ThisRef>(CTok);
		NextToken(); // Consuming 'this'
		return ParseIdentPostfix(This);
	}
	case TokenKind::KW_NEW: {
		HeapAlloc* Alloc = NewNode<HeapAlloc>(CTok);
		NextToken(); // Consuming 'new' token.
		Alloc->TypeToAlloc = ParseType();
		if (Alloc->TypeToAlloc == Context.ErrorType) {
			SkipRecovery();
		}
		return Alloc;
	}
	default:
		Error(CTok.Loc, "Expected an expression");
		ErrorNode* Err = NewNode<ErrorNode>(CTok);
		NextToken(); // Shouldn't be needed but helps prevent endless looping.
		SkipRecovery();
		return Err;
	}
}

arco::Expr* arco::Parser::ParseIdentPostfix(Expr* Site) {
	while (true) {
		switch (CTok.Kind) {
		case '(': {
			Site = ParseFuncCall(Site);
			break;
		}
		case '[': {
			ArrayAccess* Access = NewNode<ArrayAccess>(CTok);
			NextToken(); // Consuming '[' token.

			Access->Site  = Site;
			Access->Index = ParseExpr();

			Site = Access;
			Match(']');
			break;
		}
		case '.': {
			FieldAccessor* FieldAcc = NewNode<FieldAccessor>(CTok);
			NextToken(); // Consuming '.' token.

			FieldAcc->Ident = ParseIdentifier("Expected identifier for field");
			FieldAcc->Site = Site;

			Site = FieldAcc;
			break;
		}
		default:
			return Site;
		}
	}
}

arco::NumberLiteral* arco::Parser::ParseIntLiteral() {
	llvm::StringRef Text = CTok.GetText();

	ulen Idx = 0;
	u64 IntValue = 0, PrevValue;
	while (Idx < Text.size()) {
		char C = Text[Idx];
		if (C == NUMBER_SEPERATOR) {
			++Idx;
			continue;
		}
		if (!IsDigit(C)) break;
		++Idx;

		PrevValue = IntValue;
		IntValue  = IntValue * 10 + ((u64)C - '0');
	
		// Check for overflow
		if (IntValue / 10 < PrevValue) {
			Error(CTok, "Integer value is too large");
			break;
		}
	}

	return FinalizeIntLiteral(Idx, IntValue);
}

arco::NumberLiteral* arco::Parser::ParseHexLiteral() {
	llvm::StringRef Text = CTok.GetText();
	// TODO: replace with array
	static std::unordered_map<char, u64> HexToDecimalMapping =
	{
		{ '0', 0  }, { '1', 1  }, { '2', 2  }, { '3', 3  }, { '4', 4  },
		{ '5', 5  }, { '6', 6  }, { '7', 7  }, { '8', 8  }, { '9', 9  },
		{ 'a', 10 }, { 'b', 11 }, { 'c', 12 }, { 'd', 13 }, { 'e', 14 },
		{ 'f', 15 },
		{ 'A', 10 }, { 'B', 11 }, { 'C', 12 }, { 'D', 13 }, { 'E', 14 },
		{ 'F', 15 },
	};

	ulen Idx = 2; // Skip 0x
	u64 IntValue = 0, PrevValue;
	while (Idx < Text.size()) {
		char C = Text[Idx];
		if (C == NUMBER_SEPERATOR) {
			++Idx;
			continue;
		}
		if (!IsHex(C)) break;
		++Idx;
	
		PrevValue = IntValue;
		IntValue  = IntValue * 16 + HexToDecimalMapping[C];

		// Check for overflow
		if (IntValue / 16 < PrevValue) {
			Error(CTok, "Integer value is too large");
			break;
		}
	}

	return FinalizeIntLiteral(Idx, IntValue);
}

arco::NumberLiteral* arco::Parser::ParseBinLiteral() {
	llvm::StringRef Text = CTok.GetText();

	ulen Idx = 2; // Skip 0b
	u64 IntValue = 0, PrevValue;
	while (Idx < Text.size()) {
		char C = Text[Idx];
		if (C == '\'') {
			++Idx;
			continue;
		}
		if (!(C == '0' || C == '1')) break;
		++Idx;
	
		PrevValue = IntValue;
		IntValue = IntValue * 2 + ((u64)C - '0');

		// Check for overflow
		if (IntValue / 2 < PrevValue) {
			Error(CTok, "Integer value is too large");
			break;
		}
	}

	return FinalizeIntLiteral(Idx, IntValue);
}

// Returns 0xFF is invalid escape
static char GetEscapeChar(char C) {
	switch (C) {
	case '\\': return '\\';
	case 'n':  return '\n';
	case 't':  return '\t';
	case '0':  return '\0';
	case '"':  return '"';
	case 'a':  return '\a';
	case 'r':  return '\r';
	case 'v':  return '\v';
	case 'b':  return '\b';
	case 'f':  return '\f';
	case '?':  return '\?';
	case '\'': return '\'';
	default:
		return 0xFF;
	}
}

arco::NumberLiteral* arco::Parser::ParseCharLiteral() {
	NumberLiteral* Number = NewNode<NumberLiteral>(CTok);
	Number->Ty = Context.CharType;
	
	llvm::StringRef Text = CTok.GetText();
	if (Text.size() == 1) {
		// Invalid character
		NextToken(); // Consuming the character token.
		return Number;
	}

	if (Text[1] == '\\') {
		if (Text.size() == 2) {
			// Invalid character, missing closing quote.
			NextToken(); // Consuming the character token.
			return Number;
		}

		char Escape = GetEscapeChar(Text[2]);

		NextToken(); // Consuming the character token.

		if (Escape == 0xFF) {
			Error(CTok, "Unexpected escape sequence in char");
			return Number;
		} else {
			Number->SignedIntValue = Escape;
			return Number;
		}
	} else {
		Number->SignedIntValue = Text[1];

		NextToken(); // Consuming the character token.
		return Number;
	}
}

arco::NumberLiteral* arco::Parser::FinalizeIntLiteral(ulen Idx, u64 IntValue) {
	NumberLiteral* Number = NewNode<NumberLiteral>(CTok);
	Number->Ty = Context.IntType;
	Number->SignedIntValue = IntValue;

	NextToken();
	return Number;
}

arco::StringLiteral* arco::Parser::ParseStringLiteral() {

	StringLiteral* String = NewNode<StringLiteral>(CTok);
	String->Ty = Context.CStrType;
	
	llvm::StringRef Text = CTok.GetText();
	ulen Index = 1;
	while (Index < Text.size() - 1) {
		char Char = Text[Index++];
		switch (Char) {
		case '\\': {
			if (Index == Text.size() - 1) {
				// If this case occures then the lexer should have
				// already reported an error about missing the end
				// quotation.
				goto stringLiteralSequenceError;
			}

			char Escape = GetEscapeChar(Text[Index++]);
			if (Escape == 0xFF) {
				Error(CTok, "Unexpected escape sequence at index '%s'", Index - 2);
				goto stringLiteralSequenceError;
			} else {
				String->Characters += Escape;
			}

			break;
		}
		default:
			String->Characters += Char;
			break;
		}
	}
stringLiteralSequenceError:

	NextToken(); // Consuming string literal token

	return String;
}

arco::FuncCall* arco::Parser::ParseFuncCall(Expr* Site) {
	FuncCall* Call = NewNode<FuncCall>(CTok);
	Call->Site = Site;
	NextToken(); // Consuming '(' token

	if (CTok.IsNot(')')) {
		ParseAggregatedValues(Call->Args, ')', false);
	}

	Match(')', "for function call");
	return Call;
}

arco::Array* arco::Parser::ParseArray() {
	Array* Arr = NewNode<Array>(CTok);
	Match('[');
	
	if (ArrayDepthCount == 0) {
		// TODO: memset for performance?

		// Clearing the sizes for the new array.
		for (ulen i = 0; i < MAX_ARRAY_DEPTH; i++) {
			LargestArrayLengthAtDepth[i] = 0;
		}
	}

	++ArrayDepthCount;
	if (CTok.IsNot(']')) {
		bool MoreElements = false;
		do {

			Arr->Elements.push_back(ParseExpr());

			MoreElements = CTok.Is(',');
			if (MoreElements) {
				NextToken(); // Consuming ',' token.
			}
			if (CTok.Is(']')) {
				break; // Allow for extra ',' token at the end
			}
		} while (MoreElements);
	}
	--ArrayDepthCount;

	CalcLargestArrayLengthAtDepth(Arr);
	if (ArrayDepthCount == 0) {
		SetRequiredArrayLengthForArray(Arr);
	}

	Match(']');
	return Arr;
}

void arco::Parser::CalcLargestArrayLengthAtDepth(Array* Arr) {
	if (ArrayDepthCount < MAX_ARRAY_DEPTH) {
		// Finding the maximum length at the given nesting level.
		if (Arr->Elements.size() > LargestArrayLengthAtDepth[ArrayDepthCount]) {
			LargestArrayLengthAtDepth[ArrayDepthCount] = Arr->Elements.size();
		}
	} else {
		Log.BeginError(Arr->Loc, "Array exceeds maximum array depth allowed");
		Log.AddNoteLine([](llvm::raw_ostream& OS) {
			OS << "Maximum depth level: " << MAX_ARRAY_DEPTH << ".";
		});
		Log.EndError();
	}
}

void arco::Parser::SetRequiredArrayLengthForArray(Array* Arr, ulen CArrayDepth) {
	if (CArrayDepth >= MAX_ARRAY_DEPTH) return;
	Arr->RequiredNumElements = LargestArrayLengthAtDepth[CArrayDepth];
	for (Expr* Elm : Arr->Elements) {
		if (Elm->Is(AstKind::ARRAY)) {
			SetRequiredArrayLengthForArray(static_cast<Array*>(Elm), CArrayDepth + 1);
		}
	}
}

void arco::Parser::ParseAggregatedValues(llvm::SmallVector<NonNamedValue, 2>& Values,
	                                     u16 EndDelimTok,
	                                     bool AllowTrailingComma) {
	bool MoreValues = false;
	do {

		NonNamedValue NonNamedVal;
		CREATE_EXPANDED_SOURCE_LOC(
			NonNamedVal.E = ParseExpr(),
			NonNamedVal.ExpandedLoc
		)

		Values.push_back(NonNamedVal);

		if (CTok.Is(',')) {
			NextToken(); // Consuming ','
			MoreValues = true;
			if (AllowTrailingComma && CTok.Is(EndDelimTok)) {
				MoreValues = false;
			}
		} else if (CTok.Is(EndDelimTok)) {
			MoreValues = false;
		} else {
			Error(CTok, "Unexpected token when parsing values");
			SkipRecovery();
			MoreValues = false;
		}
	} while (MoreValues);

}

//===-------------------------------===//
// Utilities
//===-------------------------------===//

void arco::Parser::NextToken() {
	PrevToken = CTok;
	if (SavedTokensCount) {
		CTok = SavedTokens[0];
		std::rotate(SavedTokens, SavedTokens + 1, SavedTokens + SavedTokensCount);
		--SavedTokensCount;
	} else {
		CTok = Lex.NextToken();
	}
}

arco::Token arco::Parser::PeekToken(ulen n) {
	assert(n != 0 && "Cannot peek zero tokens");
	assert(n < MAX_SAVED_TOKENS && "Cannot peek more than the maximum peek token amount");
	for (ulen i = SavedTokensCount; i < n; i++) {
		SavedTokens[SavedTokensCount] = Lex.NextToken();
		++SavedTokensCount;
	}
	return SavedTokens[SavedTokensCount - 1];
}

void arco::Parser::Match(u16 Kind, const char* Purpose) {
	if (CTok.Is(Kind)) {
		NextToken(); // Consuming the matched token.
		return;
	}
	Error(PrevToken, "Expected '%s'%s%s", Token::TokenKindToString(Kind, Context),
		Purpose ? " " : "", Purpose);
}

void arco::Parser::SkipRecovery() {
	while (true) {
	switch (CTok.Kind) {
	// Statements
	case TokenKind::KW_RETURN:
	case TokenKind::KW_FN:
	case TokenKind::KW_LOOP:
	case TokenKind::KW_BREAK:
	case TokenKind::KW_CONTINUE:
		return;
	case ';':
	case TokenKind::TK_EOF:
		return;
	case TokenKind::IDENT:
		switch (PeekToken(1).Kind) {
		case TYPE_KW_START_CASES:
		case TokenKind::IDENT:
			// Variable declaration
			return;
		}
		// Skip and continue
		NextToken();
		break;
	default:
		// Skip and continue
		NextToken();
		break;
	}
	}
}

arco::Identifier arco::Parser::ParseIdentifier(const char* ErrorMessage) {
	if (CTok.IsNot(TokenKind::IDENT)) {
		Log.BeginError(CTok.Loc, ErrorMessage);
		if (CTok.IsKeyword()) {
			Log.AddNoteLine([=](llvm::raw_ostream& OS) {
				OS << "'" << CTok.GetText() << "'";
				OS << " is a keyword.";
			});
		}
		Log.EndError();
		return Identifier(); // Returning a null identifier.
	}
	llvm::StringRef Text = CTok.Loc.Text;
	NextToken(); // Consuming ident token.
	return Identifier(Text);
}
