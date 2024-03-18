#include "Parser.h"

#include <iostream>
#include <stack>

#include "Context.h"
#include "FloatConversions.h"

namespace arco {
    
    template<typename N>
    static inline N* NewNode(SourceLoc Loc) {
        N* Node = new N;
        Node->Loc = Loc;
        return Node;
    }
    
    template<typename N>
    static inline N* NewNode(Token STok) {
        return NewNode<N>(STok.Loc);
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

#define TYPE_KW_START_CASES_BUT_FN \
     TokenKind::KW_INT:     \
case TokenKind::KW_PTRSIZE: \
case TokenKind::KW_INT8:    \
case TokenKind::KW_INT16:   \
case TokenKind::KW_INT32:   \
case TokenKind::KW_INT64:   \
case TokenKind::KW_UINT8:   \
case TokenKind::KW_UINT16:  \
case TokenKind::KW_UINT32:  \
case TokenKind::KW_UINT64:  \
case TokenKind::KW_FLOAT32: \
case TokenKind::KW_FLOAT64: \
case TokenKind::KW_VOID:    \
case TokenKind::KW_CHAR:    \
case TokenKind::KW_BOOL:    \
case TokenKind::KW_CSTR     \

#define TYPE_KW_START_CASES \
TYPE_KW_START_CASES_BUT_FN: \
case TokenKind::KW_FN

// P - Parsing code.
// L - The Source location to assign to.
#define CREATE_EXPANDED_SOURCE_LOC(P, L)           \
SourceLoc ExpandedStartLoc = CTok.Loc;             \
P;                                                 \
L.LineNumber = ExpandedStartLoc.LineNumber;        \
L.Text =                                           \
    llvm::StringRef(ExpandedStartLoc.Text.begin(), \
             PrevToken.GetText().end() - ExpandedStartLoc.Text.begin());
   

arco::Parser::Parser(ArcoContext& Context, FileScope* FScope, Module* Mod, const SourceBuf FileBuffer)
    : Context(Context), Mod(Mod), Log(FScope, FileBuffer), Lex(Context, Log, FileBuffer), FScope(FScope)
{
}

void arco::Parser::Parse() {
    // TODO: If this ever becomes multithreaded this won't
    // work here. Same goes for any other uses of this.
    bool PrevNumErrors = TotalAccumulatedErrors;

    // -- DEBUG
    // llvm::outs() << "Parsing file: " << FScope->Path << "\n";

    NextToken(); // Prime the parser.

    FScope->Mod = Mod;
    NSpace = Mod->DefaultNamespace;

    if (CTok.Is(TokenKind::KW_NAMESPACE)) {
        NextToken();
        Identifier NamespaceName = ParseIdentifier("Expecified identifier to namespace");
        if (!NamespaceName.IsNull()) {
            auto Itr = Mod->Namespaces.find(NamespaceName);
            if (Itr == Mod->Namespaces.end()) {
                NSpace = new Namespace;
                Mod->Namespaces.insert({ NamespaceName, NSpace });
            } else {
                NSpace = Itr->second;
            }
        }

        FScope->UniqueNSpace = NSpace;
        Match(';');
    }

    // Parsing imports.
    while (CTok.IsNot(TokenKind::TK_EOF)) {
        if (CTok.Is(TokenKind::KW_IMPORT)) {
            ParseImport();
        } else {
            break;
        }
    }

    auto ProcessGlobal = [this](VarDecl* Global) {
        Global->IsGlobal = true;
        Context.UncheckedDecls.insert(Global);
        
        if (!Global->Name.IsNull()) {
            auto Itr = NSpace->Decls.find(Global->Name);
            if (Itr != NSpace->Decls.end()) {
                Decl* FirstDecl = Itr->second;
                Error(Global->Loc,
                    "Duplicate declaration of identifier '%s'. First declared at: %s:%s",
                    Global->Name,
                    FirstDecl->FScope->Path,
                    FirstDecl->Loc.LineNumber
                    );
            } else {
                NSpace->Decls[Global->Name] = Global;
            }
        }
    };

    // Parsing top level statements.
    while (CTok.IsNot(TokenKind::TK_EOF)) {
        AstNode* Stmt;
        ParseOptStmt(Stmt, TokenKind::TK_EOF);
        if (!Stmt) continue;

        if (Stmt->Is(AstKind::FUNC_DECL)) {
            FuncDecl* Func = static_cast<FuncDecl*>(Stmt);
            if (!Func->Name.IsNull()) {
                if (Func->Name == Context.MainIdentifier) {
                    llvm::SmallVector<VarDecl*, 2>& Params = Func->Params;
                    ulen NumParams = Func->Params.size();
                    if (!Func->IsVariadic &&
                        (NumParams == 0 ||
                            (NumParams == 2 && Params[0]->Ty->Equals(Context.IntType) && Params[1]->Ty->Equals(Context.CharPtrPtrType))
                            )) {
                        if (!Context.MainEntryFunc) {
                            if (!Func->RaisedErrors.empty()) {
                                Error(Func->Loc, "main function cannot raise errors");
                            }
                            Context.MainEntryFunc = Func;
                        } else {
                            // Duplicate entry function.
                            Error(Func->Loc,
                                "Duplicate entry point found. First declared at: %s:%s",
                                Context.MainEntryFunc->FScope->Path,
                                Context.MainEntryFunc->Loc.LineNumber);
                        }
                    }
                }
                
                NSpace->Funcs[Func->Name].push_back(Func);
            }
        } else if (Stmt->Is(AstKind::STRUCT_DECL) || Stmt->Is(AstKind::ENUM_DECL) || Stmt->Is(AstKind::INTERFACE_DECL)) {
            Decl* Dec = static_cast<Decl*>(Stmt);
            auto Itr = FScope->Imports.find(Dec->Name);
            if (Itr != FScope->Imports.end()) {
                Error(Dec->Loc, "Declaration conflicts with import");
            }

            if (!Dec->Name.IsNull()) {
                if (Dec->Mods & ModKinds::PRIVATE) {
                    Decl* FirstDec = FScope->FindDecl(Dec->Name);
                    if (!FirstDec) {
                        FScope->PrivateDecls.push_back(Dec);
                    } else {
                        Error(Dec->Loc, "Duplicate declaration of identifier '%s'. First declared at: %s:%s",
                            Dec->Name, FirstDec->FScope->Path, FirstDec->Loc.LineNumber);
                    }
                } else {
                    auto Itr = NSpace->Decls.find(Dec->Name);
                    if (Itr == NSpace->Decls.end()) {
                        NSpace->Decls[Dec->Name] = Dec;
                    } else {	
                        Decl* FirstDec = Itr->second;
                        Error(Dec->Loc, "Duplicate declaration of identifier '%s'. First declared at: %s:%s",
                            Dec->Name, FirstDec->FScope->Path, FirstDec->Loc.LineNumber);
                    }
                }
            }
        } else if (Stmt->Is(AstKind::VAR_DECL)) {
            VarDecl* Global = static_cast<VarDecl*>(Stmt);
            ProcessGlobal(Global);
        } else if (Stmt->Is(AstKind::VAR_DECL_LIST)) {
            VarDeclList* List = static_cast<VarDeclList*>(Stmt);
            for (VarDecl* Global : List->Decls) {
                ProcessGlobal(Global);
            }
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
}

void arco::Parser::ParseImport() {
    Token ImportTok = CTok;
    NextToken(); // Consuming 'import' token.

    Identifier ModOrNamespace = ParseIdentifier("Expected identifier for import module name");
    Identifier StructOrNamespace, StructName;

    if (CTok.Is('.')) {
        // Import must be for a namespace or struct.
        NextToken();
    
        StructOrNamespace = ParseIdentifier("Expected identifier for struct or namespace");

        if (CTok.Is('.')) {
            // Import must be for a struct.
            NextToken();

            StructName = ParseIdentifier("Expected identifier for struct");
        }
    }

    bool IsStaticImport = CTok.Is(TokenKind::KW_STATIC) && StructName.IsNull();
    if (IsStaticImport) {
        NextToken();
    }

    Match(';');

    if (!IsStaticImport) {

        // import namespace;
        // import mod;
        // import mod.namespace;
        // import mod.struct;
        // import mod.namespace.struct;

        Identifier LookupIdent = StructOrNamespace.IsNull() ? ModOrNamespace :
                                 StructName.IsNull()        ? StructOrNamespace : StructName;

        if (FScope->Imports.find(LookupIdent) != FScope->Imports.end()) {
            Error(ImportTok, "Duplicate import");
            return;
        }

        FScope->Imports[LookupIdent] = {
            ImportTok.Loc,
            ModOrNamespace,
            StructOrNamespace,
            StructName
        };

    } else {
        auto Itr = Context.ModNamesToMods.find(ModOrNamespace.Text);
        bool ModFound = Itr != Context.ModNamesToMods.end();

        // namespace        (need namespace)
        // module.namespace (need namespace)
        // module           (do not need namespace)
        FScope->StaticImports.push_back({
                ImportTok.Loc,
                ModFound ? Itr->second : nullptr,
                StructOrNamespace.IsNull() ? ModOrNamespace : StructOrNamespace,
                !StructOrNamespace.IsNull()
            });
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
    llvm::SmallVector<GenericType*> GenTys;
    switch (CTok.Kind) {
    case TokenKind::KW_RETURN: Stmt = ParseReturn(); Match(';');      break;
    case TokenKind::KW_IF:     Stmt = ParseIf();                      break;
    case TokenKind::KW_LOOP:   Stmt = ParseLoop();                    break;
    case TokenKind::KW_CONTINUE:
    case TokenKind::KW_BREAK:  Stmt = ParseLoopControl(); Match(';'); break;
    case TokenKind::KW_DELETE: Stmt = ParseDelete(); Match(';');      break;
    case TokenKind::KW_RAISE:  Stmt = ParseRaise(); Match(';');       break;
    case TokenKind::KW_GENERICS: {
        // TODO: likely copying?
        GenTys = ParseGenerics();
        [[fallthrough]];
    }
    case TokenKind::KW_NATIVE:
    case TokenKind::KW_PRIVATE:
    case TokenKind::KW_READONLY:
    case TokenKind::KW_WRITEONLY:
    case TokenKind::KW_DLLIMPORT: {
        // TODO: constructors!
        Modifiers Mods = ParseModifiers();
        if (CTok.Is(TokenKind::KW_FN)) {
            return ParseFuncDecl(Mods, std::move(GenTys));
        } else if (CTok.Is(TokenKind::IDENT)) {
            Token PeekTok = PeekToken(1);
            if (PeekTok.Is(',')) {
                Stmt = ParseVarDeclList(Mods);
                Match(';');
            } else if (PeekTok.Is(TokenKind::KW_STRUCT)) {
                Stmt = ParseStructDecl(Mods);
            } else if (PeekTok.Is(TokenKind::KW_ENUM)) {
                Stmt = ParseEnumDecl(Mods);
            } else if (PeekTok.Is(TokenKind::KW_INTERFACE)) {
                Stmt = ParseInterfaceDecl(Mods);
            } else {
                Stmt = ParseVarDecl(Mods);
                Match(';');
            }
        } else {
            Error(CTok, "Expected declaration");
            SkipRecovery();
            return nullptr;
        }
        break;
    }
    case TokenKind::KW_FN:
        return ParseFuncDecl(0, {});
    case TokenKind::IDENT: {
        if (CStruct && Identifier(CTok.GetText()) == CStruct->Name) {
            Stmt = ParseFuncDecl(0, {});
            break;
        }

        switch (PeekToken(1).Kind) {
        case IDENT:
        case TYPE_KW_START_CASES_BUT_FN:
        case TokenKind::KW_CONST:
        case TokenKind::COL_EQ:
        case TokenKind::COL_COL: {
            Stmt = ParseVarDecl(0);
            Match(';');
            break;
        }
        case TokenKind::KW_FN: {
            // Specifically dealing with error recovery for stuff like:
            // abc fn foo(a int) {}

            if (PeekToken(2).Kind == TokenKind::IDENT) {
                Error(CTok, "Unexpected identifier");
                NextToken(); // Skipping bad identifier token.
                Stmt = ParseFuncDecl(0, GenTys);
            } else {
                Stmt = ParseVarDecl(0);
                Match(';');
            }
            break;
        }
        case TokenKind::KW_STRUCT:
            return ParseStructDecl(0);
        case TokenKind::KW_ENUM:
            return ParseEnumDecl(0);
        case TokenKind::KW_INTERFACE:
            return ParseInterfaceDecl(0);
        case ',': {
            Stmt = ParseVarDeclList(0);
            Match(';');
            break;
        }
        default:
            Stmt = ParseAssignmentAndExprs();
            Match(';');
            break;
        }
        break;
    }
    case '~': {
        Token PeekTok = PeekToken(1);
        if (PeekTok.Is(TokenKind::IDENT)) {
            if (CStruct && Identifier(PeekTok.GetText()) == CStruct->Name) {
                // Destructor.
                Stmt = ParseFuncDecl(0, {});
                break;
            }
        }
        
        Stmt = ParseAssignmentAndExprs();
        Match(';');
        break;
    }
    case '{':
        Stmt = ParseNestedScope();
        break;
    default:
        Stmt = ParseAssignmentAndExprs();
        Match(';');
        break;
    }
    return Stmt;
}

arco::FuncDecl* arco::Parser::ParseFuncDecl(Modifiers Mods, llvm::SmallVector<GenericType*> GenTys) {
    
    ulen NumErrs = TotalAccumulatedErrors;

    FuncDecl* Func = NewNode<FuncDecl>(CTok);
    if (!GenTys.empty()) {
        // TODO: Use of call to new
        Func->GenData = new GenericData;
        Func->GenData->GenTys = GenTys;
    }

    if (CTok.Is(TokenKind::KW_FN)) {
        NextToken(); // Consuming 'fn' keyword.
        Func->Loc = CTok.Loc;
        if (CTok.Is('(')) {
            NextToken();
            Token Tok = CTok;
            Identifier CallingConv = ParseIdentifier("Expected identifier for calling convention");
            if (!CallingConv.IsNull()) {
                auto Itr = Context.CallConventions.find(CallingConv);
                if (Itr == Context.CallConventions.end()) {
                    Error(Tok, "%s is not a valid calling convention", CallingConv);
                }
            }
            Match(')', "Expected for calling convention");
            Func->CallingConv = CallingConv;
        }
    } else {
        // Constructor/Destructor.
        if (!CTok.Is('~')) {
            Func->Loc = CTok.Loc;
            Func->IsConstructor = true;
        } else {
            NextToken();
            Func->Loc = CTok.Loc;
            Func->IsDestructor = true;
        }
    }

    Func->Mod        = Mod;
    Func->FScope     = FScope;
    Token      NameTok = CTok;
    Identifier Name    = ParseIdentifier("Expected identifier for function declaration");
    if (Func->IsConstructor) {
        if (CTok.Is(TokenKind::KW_COPYOBJ)) {
            NextToken(); // Consuming 'copyobj' keyword.
            Func->IsCopyConstructor = true;
        } else if (CTok.Is(TokenKind::KW_MOVEOBJ)) {
            NextToken(); // Consuming 'moveobj' keyword.
            Func->IsMoveConstructor = true;
        }
    }
    if (Func->IsDestructor) {
        llvm::StringRef DestructorName =
            llvm::StringRef(NameTok.Loc.Text.begin() - 1, NameTok.GetText().size() + 1);
        Func->Name = Identifier(DestructorName);
    } else {
        Func->Name = Name;
    }
    Func->Mods       = Mods;
    Func->LinkageName = NativeModifierName;
    NativeModifierName = "";

    Context.UncheckedDecls.insert(Func);

    FuncDecl* PrevFunc = CFunc;
    CFunc = Func;

    PUSH_SCOPE();
    ParseFuncSignature(Func);

    if (Func->IsGeneric()) {
        for (VarDecl* Param : Func->Params) {
            if (Param->Ty->ContainsGenerics) {
                Param->GenericIdx = Func->GenData->NumQualifications;
                ++Func->GenData->NumQualifications;
            }
        }
    }

    if (!(Func->Mods & ModKinds::NATIVE)) {
        ParseScopeStmts(Func->Scope);
    } else {
        Match(';');
    }
    POP_SCOPE();
    
    if (Func->RetTy == Context.VoidType) {
        bool NoReturn = Func->Scope.Stmts.empty();
        if (!NoReturn) {
            AstNode* LastStmt = Func->Scope.Stmts.back();
            NoReturn |= LastStmt->IsNot(AstKind::RETURN) && LastStmt->IsNot(AstKind::RAISE);
        }

        if (NoReturn) {
            // Implicit return.
            ++Func->NumReturns;
        }
    }

    if (NumErrs != TotalAccumulatedErrors) {
        Func->ParsingError = true;
    }

    CFunc = PrevFunc;
    return Func;
}

void arco::Parser::ParseFuncSignature(FuncDecl* Func) {
    
    RecoveryStrat PrevStrat = RecStrat;
    RecStrat = RecoveryStrat::Params;
    Match('(');
    if (CTok.IsNot(')')) {
        bool MoreParams = false;

        ulen ParamCount = 0;
        do {
            VarDecl* Param = ParseVarDecl(0);
            if (Param) {
                if (Param->Ty && CTok.Is('^')) {
                    // TODO: Use of GetRealKind() might cause problems for enums!
                    if (Param->Ty->GetRealKind() != TypeKind::Array) {
                        NextToken(); // Implicit pointer type.
                        Param->Ty = PointerType::Create(Param->Ty, Context);
                        Param->ImplicitPtr = true;
                    }
                }
                Param->ParamIdx = ParamCount++;
                Func->Params.push_back(Param);
            } else {
                // Something went wrong during parsing.
            }
            
            MoreParams = CTok.Is(',');
            if (MoreParams) {
                NextToken(); // Consuming ',' token
            }
        } while (MoreParams);
    }
    RecStrat = PrevStrat;

    if (CTok.Is(TokenKind::DOT_DOT)) {
        Error(CTok, "Did you mean to type ... for variadic arguments?");
        NextToken();
    } else if (CTok.Is(TokenKind::DOT_DOT_DOT)) {
        if (Func->Params.empty()) {
            Error(Func->Loc, "Cannot variadic arguments without a parameter");
        }
        Func->IsVariadic = true;
        NextToken();
    }
    Match(')');

    if (CTok.IsNot('{') && CTok.IsNot(':') &&
        CTok.IsNot(TokenKind::KW_RAISES) &&
        !((Func->Mods & ModKinds::NATIVE) && CTok.Is(';'))) {
        if (CTok.Is(TokenKind::KW_CONST)) {
            NextToken(); // Consuming 'const' token.
            Func->ReturnsConstAddress = true;
        }
        Func->RetTy = ParseType(true);
    } else {
        if (CTok.Is(':')) {
            NextToken(); // Consuming ':' token.
            bool MoreInitializers = false;
            do {
                Token NameTok = CTok;
                Identifier FieldName = ParseIdentifier("Expected identifier for field of initializer value");
                Match('(', "For initializer value");
                Expr* Assignment = ParseExpr();
                Match(')', "For initializer value");
                Func->InitializerValues.push_back(FuncDecl::InitializerValue{
                    NameTok.Loc,
                    FieldName,
                    Assignment
                    });
                MoreInitializers = CTok.Is(',');
                if (MoreInitializers) {
                    NextToken();
                }
            } while (MoreInitializers);
        }
        Func->RetTy = Context.VoidType;
    }
    if (CTok.Is(TokenKind::KW_RAISES)) {
        NextToken(); // Consuming 'raises' keyword.
        bool MoreErrors = false;
        do {
            Token NameTok = CTok;
            Identifier Name = ParseIdentifier("Expected identifier for raised error");
            Func->RaisedErrors.push_back(
                FuncDecl::RaisedError{
                    Name,
                    NameTok.Loc
                });
            MoreErrors = CTok.Is(',');
            if (MoreErrors) {
                NextToken();
            }
        } while (MoreErrors);
    }
}

arco::VarDecl* arco::Parser::ParseVarDecl(Modifiers Mods) {

    ulen NumErrs = TotalAccumulatedErrors;

    // Have to pull these out because C++ is stupid and does not do statements in order
    // of arguments.
    Token NameTok = CTok;
    Identifier Name = ParseIdentifier("Expected identifier for variable declaration");
    if (Name.IsNull()) {
        if (RecStrat == RecoveryStrat::Params) {
            SkipRecovery();
        }
        return nullptr;
    }

    VarDecl* Var = CreateVarDecl(NameTok, Name, Mods);
    if (CTok.Is(TokenKind::KW_CONST)) {
        NextToken(); // Consuming 'const' token.
        Var->HasConstAddress = true;
        Var->ExplicitlyMarkedConst = true;
    }
    if (!Var->HasConstAddress && CTok.Is(TokenKind::COL_EQ)) {
        NextToken(); // Consuming ':=' token.
        Var->Assignment = ParseExprAndCatch();
        Var->Ty = Context.ErrorType;
        Var->TyIsInfered = true;
    } else if (!Var->HasConstAddress && CTok.Is(TokenKind::COL_COL)) {
        NextToken(); // Consuming '::' token.
        Var->Assignment = ParseExprAndCatch();
        Var->Ty = Context.ErrorType;
        Var->TyIsInfered = true;
        Var->HasConstAddress = true;
        Var->ExplicitlyMarkedConst = true;
    } else {
        Var->Ty = ParseType(true);
    
        if (CTok.Is('=')) {
            NextToken(); // Consuming '=' token.
            if (CTok.Is(TokenKind::MINUS_MINUS_MINUS)) {
                NextToken(); // Consuming '---' token.
                Var->LeaveUninitialized = true;
            } else {
                Var->Assignment = ParseExprAndCatch();
            }
        }
    }

    FinishVarDecl(Var);
    if (NumErrs != TotalAccumulatedErrors) {
        Var->ParsingError = true;
    }

    return Var;
}

arco::VarDeclList* arco::Parser::ParseVarDeclList(Modifiers Mods) {
    VarDeclList* List = NewNode<VarDeclList>(CTok);

    if (CTok.Is(TokenKind::IDENT) && PeekToken(1).Is(',')) {
    
        ulen NumErrs = TotalAccumulatedErrors;

        bool MoreDecls = false;
        do {
            Token NameTok = CTok;
            Identifier Name = ParseIdentifier("Expected identifier for variable declaration");
            VarDecl* Var = CreateVarDecl(NameTok, Name, Mods);
            Var->Ty = Context.ErrorType;

            List->Decls.push_back(Var);

            MoreDecls = CTok.Is(',');
            if (MoreDecls) {
                NextToken(); // Consuming ',' token.
            }
        } while (MoreDecls);

        // TODO: Allow for implicit array types? The problem with allowing them here
        // is that the same instance of the type is passed to each variable so the
        // each variable could end up with different size initializations.
        Type* Ty = Context.ErrorType;
        bool IsInfered = false, HasConstAddress = false;
        if (CTok.Is(TokenKind::COL_EQ)) {
            NextToken();
            IsInfered = true;
        } else if (CTok.Is(TokenKind::COL_COL)) {
            NextToken();
            IsInfered = true;
            HasConstAddress = true;
        } else {
            Ty = ParseType(false);
        }
        
        if (NumErrs != TotalAccumulatedErrors) {
            for (VarDecl* Var : List->Decls) {
                FinishVarDecl(Var);
                Var->ParsingError = true;
            }
            return List;
        }

        for (VarDecl* Var : List->Decls) {
            Var->Ty = Ty;
            Var->TyIsInfered = IsInfered;
            Var->HasConstAddress = HasConstAddress;
        }

        if (IsInfered || CTok.Is('=')) {
            if (!IsInfered) {
                NextToken(); // Consuming '=' token.
            }

            ulen Count = 0;
            while (true) {
                NumErrs = TotalAccumulatedErrors;

                if (CTok.Is(TokenKind::MINUS_MINUS_MINUS)) {
                    NextToken(); // Consuming '---' token.
                    List->Decls[Count]->LeaveUninitialized = true;
                } else {
                    List->Decls[Count]->Assignment = ParseExprAndCatch();
                }
                
                if (NumErrs != TotalAccumulatedErrors) {
                    List->Decls[Count]->ParsingError = true;
                    // TODO: error the remaining declarations?
                    break;
                }

                if (CTok.Is(',')) {
                    if (Count + 1 >= List->Decls.size()) {
                        Error(CTok, "Too many initializers for variable list");
                        NextToken();
                        SkipRecovery();
                    }
                    NextToken(); // Consuming ',' token.
                    ++Count;
                } else {
                    break;
                }
            }
        }

        for (VarDecl* Var : List->Decls) {
            FinishVarDecl(Var);
        }

    } else {
        // Assume its a single declaration.
        List->Decls.push_back(ParseVarDecl(0));
    }

    return List;
}

arco::VarDecl* arco::Parser::CreateVarDecl(Token Tok, Identifier Name, Modifiers Mods) {
    VarDecl* Var = NewNode<VarDecl>(Tok);
    Var->Mod        = Mod;
    Var->FScope     = FScope;
    Var->Name       = Name;
    Var->Mods       = Mods;
    Var->NativeName = NativeModifierName;
    NativeModifierName = "";
    return Var;
}

void arco::Parser::FinishVarDecl(VarDecl* Var) {
    if (LocScope && !Var->Name.IsNull()) {
        auto Itr = LocScope->VarDecls.find(Var->Name);
        if (Itr != LocScope->VarDecls.end()) {
            Error(Var->Loc, "Redeclaration of variable '%s'. First declared on line: %s",
                Var->Name, Itr->second->Loc.LineNumber);
        }
        LocScope->VarDecls.insert({ Var->Name, Var });

        if (CFunc) {
            CFunc->AllocVars.push_back(Var);
        }
    }
}

arco::StructDecl* arco::Parser::ParseStructDecl(Modifiers Mods) {

    ulen NumErrs = TotalAccumulatedErrors;

    StructDecl* Struct = NewNode<StructDecl>(CTok);
    Struct->UniqueTypeId = Context.UniqueTypeIdCounter++;
    Struct->Mod    = Mod;
    Struct->FScope = FScope;
    Struct->Name   = ParseIdentifier("Expected identifier for struct declaration");
    Struct->Mods   = Mods;
    Match(TokenKind::KW_STRUCT);
    if (CTok.Is(':')) {
        NextToken(); // Parsing ':' token.
        bool MoreInterfaces = false;
        do {
            Token InterfaceNameTok = CTok;
            Identifier InterfaceName = ParseIdentifier("Expected identifier for interface name");
            if (!InterfaceName.IsNull()) {
                Struct->InterfaceHooks.push_back(StructDecl::InterfaceHook{
                    InterfaceNameTok.Loc,
                    InterfaceName
                    });
            }

            MoreInterfaces = CTok.Is(',');
            if (MoreInterfaces) {
                NextToken(); // Consuming ',' token.
            }
        } while (MoreInterfaces);
    }

    Context.UncheckedDecls.insert(Struct);

    StructDecl* PrevStruct = CStruct;
    CStruct = Struct;

    auto ProcessField = [=](VarDecl* Field) {
        if (!Field->Name.IsNull()) {
            Field->FieldIdx = Struct->Fields.size();
            Struct->Fields.push_back(Field);
        }
    };

    Match('{');
    PUSH_SCOPE()
    while (CTok.IsNot('}') && CTok.IsNot(TokenKind::TK_EOF)) {
        AstNode* Stmt;
        ParseOptStmt(Stmt, '}');
        if (!Stmt) continue;

        if (Stmt->Is(AstKind::VAR_DECL)) {
            VarDecl* Field = static_cast<VarDecl*>(Stmt);
            ProcessField(Field);
        } else if (Stmt->Is(AstKind::VAR_DECL_LIST)) {
            VarDeclList* List = static_cast<VarDeclList*>(Stmt);
            for (VarDecl* Field : List->Decls) {
                ProcessField(Field);
            }
        } else if (Stmt->Is(AstKind::FUNC_DECL)) {
            FuncDecl* Func = static_cast<FuncDecl*>(Stmt);
            if (!Func->Name.IsNull()) {
                Func->Struct = Struct;
                if (Func->IsDestructor) {
                    if (Struct->Destructor) {
                        Error(Func->Loc, "Duplicate destructor");
                    }
                    if (!Func->Params.empty()) {
                        Error(Func->Loc, "Destructors cannot have parameters");
                    }
                    Struct->Destructor = Func;
                }
                
                if (Func->IsConstructor) {
                    // TODO: Should it also go into the Funcs list?
                    if (Func->IsCopyConstructor) {
                        if (Struct->CopyConstructor) {
                            Error(Func->Loc, "Duplicate copy constructor");
                        }
                        Struct->CopyConstructor = Func;
                    } else if (Func->IsMoveConstructor) {
                        if (Struct->MoveConstructor) {
                            Error(Func->Loc, "Duplicate move constructor");
                        }
                        Struct->MoveConstructor = Func;
                    } else {
                        Struct->Constructors.push_back(Func);
                    }
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
    POP_SCOPE()
    Match('}');

    if (NumErrs != TotalAccumulatedErrors) {
        Struct->ParsingError = true;
    }

    CStruct = PrevStruct;

    return Struct;
}

arco::EnumDecl* arco::Parser::ParseEnumDecl(Modifiers Mods) {
    
    ulen NumErrs = TotalAccumulatedErrors;

    EnumDecl* Enum = NewNode<EnumDecl>(CTok);
    Enum->UniqueTypeId = Context.UniqueTypeIdCounter++;
    Enum->Mod    = Mod;
    Enum->FScope = FScope;
    Enum->Name   = ParseIdentifier("Expected identifier for enum declaration");
    Enum->Mods   = Mods;
    Match(TokenKind::KW_ENUM);

    if (CTok.Is(':')) {
        NextToken(); // Consuming ':' token.
        Enum->ValuesType = ParseType(false);
    }

    Context.UncheckedDecls.insert(Enum);

    Match('{');
    bool MoreValues = false;
    while (CTok.IsNot('}') && CTok.IsNot(TokenKind::TK_EOF)) {
        Token NameTok = CTok;
        Identifier ValueName = ParseIdentifier("Expected identifier for enum value");
        Expr* Value = nullptr;
        if (CTok.Is(TokenKind::COL_COL)) {
            NextToken(); // Consuming '::' token.
            Value = ParseExpr();
        }

        Enum->Values.push_back(
            EnumDecl::EnumValue{
                NameTok.Loc,
                0,
                ValueName,
                Value
            });
        Match(';');
    }
    Match('}');
    
    if (NumErrs != TotalAccumulatedErrors) {
        Enum->ParsingError = true;
    }

    return Enum;
}

arco::InterfaceDecl* arco::Parser::ParseInterfaceDecl(Modifiers Mods) {

    ulen NumErrs = TotalAccumulatedErrors;

    Token NameTok = CTok;
    Identifier Name = Identifier(CTok.GetText());
    NextToken(); // Consuming identifier token.
    InterfaceDecl* Interface = NewNode<InterfaceDecl>(NameTok);
    Interface->Name = Name;
    Interface->UniqueTypeId = Context.UniqueTypeIdCounter++;
    Interface->Mod = Mod;
    Interface->FScope = FScope;
    Interface->Mods = Mods;
    NextToken(); // Consuming 'interface' token.

    Match('{');
    if (CTok.IsNot('}')) {
        while (CTok.IsNot('}') && CTok.IsNot(TokenKind::TK_EOF)) {

            // TODO: This should probably just parse any type of statement
            // and then just verify the statement makes sense in the end.
            if (CTok.Is(TokenKind::KW_FN)) {

                NextToken(); // Consuming 'fn' token.
                FuncDecl* Func = NewNode<FuncDecl>(CTok);
                Func->Name   = ParseIdentifier("Expected identifier for function of an interface");
                Func->Mod    = Mod;
                Func->FScope = FScope;
                Func->Mods   = 0; // TODO: Allow for modifiers?
                Func->Interface = Interface;
                Func->InterfaceIdx = Interface->NumFuncs;

                FuncDecl* PrevFunc = CFunc;
                CFunc = Func;

                PUSH_SCOPE();
                // TODO: Hacky: so that it doesn't complain about about the lack of a type.
                Func->Mods = ModKinds::NATIVE;
                ParseFuncSignature(Func);
                Func->Mods = 0;

                Match(';');
                POP_SCOPE();

                ++Interface->NumFuncs;
                Interface->Funcs.push_back(Func);
                CFunc = PrevFunc;

            } else {
                Error(CTok, "Expected function declaration for interface");
                NextToken();
                SkipRecovery();
            }
        }
    }
    Match('}');
    
    Context.UncheckedDecls.insert(Interface);

    if (NumErrs != TotalAccumulatedErrors) {
        Interface->ParsingError = true;
    }
    
    return Interface;
}

void arco::Parser::ParseScopeStmtOrStmts(LexScope& Scope) {
    if (CTok.Is('{')) {
        ParseScopeStmts(Scope);
    } else {
        // Assuming it is a single statement.
        Scope.StartLoc = CTok.Loc;
        Scope.Stmts.push_back(ParseStmt());
        Scope.EndLoc = PrevToken.Loc;
    }
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
        Ret->Value = ParseExprAndCatch();
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
    ParseScopeStmtOrStmts(If->Scope);
    POP_SCOPE();

    if (CTok.Is(TokenKind::KW_ELSE)) {
        NextToken(); // Consuming 'else' token
        if (CTok.Is(TokenKind::KW_IF)) {
            If->Else = ParseIf(); // 'else if'
        } else {
            NestedScopeStmt* NestedScope = NewNode<NestedScopeStmt>(CTok);
            If->Else = NestedScope;
            PUSH_SCOPE();
            ParseScopeStmtOrStmts(NestedScope->Scope);
            POP_SCOPE();
        }
    }

    return If;
}

arco::AstNode* arco::Parser::ParseLoop() {
    Token LoopTok = CTok;
    NextToken(); // Consuming 'loop' token.

    PUSH_SCOPE();
    if (CTok.Is(';')) { // loop ;
        return ParseRangeLoop(LoopTok, nullptr);
    } else if (CTok.Is(TokenKind::IDENT)) { // loop ident
        switch (PeekToken(1).Kind) {
        case TokenKind::IDENT: {
            Token Tok2 = PeekToken(2);
            // TODO: Once generics are supported disambiguation of the type
            // information will become needed.

            if (Tok2.Is('*') || Tok2.Is('[')) {
                // loop ident ident*
                // loop ident ident[4]
                VarDeclList* List = ParseVarDeclList(0);

                if (CTok.Is(':')) { // TODO: This should probably make sure there is not an assignment!
                // loop ident type :
                    return ParseIteratorLoop(LoopTok, List);
                } else if (CTok.Is(';')) {
                    // loop ident type = expr;
                    // loop ident type;
                    return ParseRangeLoop(LoopTok, List);
                } else {
                    return ParsePredicateLoop(LoopTok);
                }
            } else if (Tok2.Is(':')) {
                // loop ident ident :
                return ParseIteratorLoop(LoopTok, ParseVarDeclList(0));
            } else if (Tok2.Is(';')) {
                // loop ident ident ;
                return ParseRangeLoop(LoopTok, nullptr);
            }
            else {
                // This very well might be a parsing error but it
                // might be something like:   loop ident  foo();
                return ParsePredicateLoop(LoopTok);
            }
        }
        case TYPE_KW_START_CASES:
        case TokenKind::KW_CONST:
        case ',': {
            // TODO: May want to allow predicate assignment here later.

            VarDeclList* List = ParseVarDeclList(0);
            if (CTok.Is(':')) { // TODO: This should probably make sure there is not an assignment!
                // loop ident type :
                return ParseIteratorLoop(LoopTok, List);
            } else {
                // loop ident type = expr;
                // loop ident type;
                return ParseRangeLoop(LoopTok, List);
            }
        }
        case TokenKind::COL_EQ:
        case TokenKind::COL_COL: {
            // loop ident := expr
            // loop ident :: expr

            // TODO: May want to allow predicate assignment here later.
            return ParseRangeLoop(LoopTok, ParseVarDeclList(0));
        }
        case ':': {
            // loop ident :
            
            // Do not call ParseVarDeclList here because the declaration
            // part is infered.
            return ParseIteratorLoop(LoopTok, nullptr);
        }
        case '=': {
            // loop ident = 

            // TODO: May want to allow predicate assignment here later.
            return ParseRangeLoop(LoopTok, nullptr);
        }
        default:
            // loop ident
            return ParsePredicateLoop(LoopTok);
        }
    } else { // loop expr
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

int* foo() {
    return nullptr;
}

arco::PredicateLoopStmt* arco::Parser::ParsePredicateLoop(Token LoopTok) {

    PredicateLoopStmt* Loop = NewNode<PredicateLoopStmt>(LoopTok);
    
    if (CTok.IsNot('{')) {
        AllowStructInitializer = false;
        Loop->Cond = ParseExpr();
        if (CTok.Is(TokenKind::DOT_DOT_EQ) || CTok.Is(TokenKind::DOT_DOT_LT)) {
            Range* Rg = NewNode<Range>(CTok);
            Rg->Op = CTok.Kind;
            NextToken(); // Consuming '..='/'..<' token.
            Rg->LHS = Loop->Cond;
            Rg->RHS = ParseExpr();
            Loop->Cond = Rg;
        }
        AllowStructInitializer = true;
    }

    ParseScopeStmtOrStmts(Loop->Scope);
    POP_SCOPE();
    
    return Loop;
}

arco::RangeLoopStmt* arco::Parser::ParseRangeLoop(Token LoopTok, VarDeclList* List) {
    RangeLoopStmt* Loop = NewNode<RangeLoopStmt>(LoopTok);
    
    // PUSH_SCOPE(); -- Called in ParseLoop
    if (List) {
        Loop->InitNodes.push_back(List);
    } else if (CTok.IsNot(';')) {
        Loop->InitNodes.push_back(ParseAssignmentAndExprs());
    }
    
    Match(';');

    if (CTok.IsNot(';')) {
        Loop->Cond = ParseExpr();
    }
    Match(';');

    if (CTok.IsNot('{')) {
        AllowStructInitializer = false;
        Loop->Scope.StartLoc = CTok.Loc;
        Expr* E = ParseAssignmentAndExprs();
        AllowStructInitializer = true;
        if (CTok.Is(';')) {
            // loop expr; expr;    expr;
            Loop->Scope.EndLoc = PrevToken.Loc;
            Loop->Scope.Stmts.push_back(E);
        } else if (CTok.IsNot('{')) {
            // loop expr; expr; expr  expr;
            Loop->Incs.push_back(E);
            Loop->Scope.StartLoc = CTok.Loc;
            Loop->Scope.Stmts.push_back(ParseStmt());
            Loop->Scope.EndLoc = PrevToken.Loc;
        } else {
            // loop expr; expr; expr {
            Loop->Incs.push_back(E);
            ParseScopeStmts(Loop->Scope);
        }

        POP_SCOPE();
        return Loop;
    } else {
        // loop expr; expr; {

        ParseScopeStmts(Loop->Scope);
        POP_SCOPE();
        return Loop;
    }
}

arco::IteratorLoopStmt* arco::Parser::ParseIteratorLoop(Token LoopTok, VarDeclList* List) {
    IteratorLoopStmt* Loop = NewNode<IteratorLoopStmt>(LoopTok);

    
    // TODO: Deal with multiple declarations.
    if (List) {
        Loop->VarVal = List->Decls[0];
    } else if (CTok.Is(TokenKind::IDENT)) {
        // Infered type.
        Loop->VarVal = CreateVarDecl(CTok, Identifier(CTok.GetText()), 0);
        FinishVarDecl(Loop->VarVal);
        Loop->VarVal->Ty = nullptr;
        NextToken();
    }

    Match(':', "for iteration loop");
    AllowStructInitializer = false;
    Loop->IterOnExpr = ParseExpr();
    if (CTok.Is(TokenKind::DOT_DOT_EQ) || CTok.Is(TokenKind::DOT_DOT_LT)) {
        Range* Rg = NewNode<Range>(CTok);
        Rg->Op = CTok.Kind;
        NextToken(); // Consuming '..='/'..<' token.
        Rg->LHS = Loop->IterOnExpr;
        Rg->RHS = ParseExpr();
        Loop->IterOnExpr = Rg;
    }
    AllowStructInitializer = true;

    // PUSH_SCOPE(); -- Called in ParseLoop
    ParseScopeStmtOrStmts(Loop->Scope);
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

arco::DeleteStmt* arco::Parser::ParseDelete() {
    DeleteStmt* Delete = NewNode<DeleteStmt>(CTok);
    NextToken(); // Consuming 'delete' token.
    Delete->Value = ParseExpr();
    return Delete;
}

arco::RaiseStmt* arco::Parser::ParseRaise() {
    RaiseStmt* Raise = NewNode<RaiseStmt>(CTok);
    NextToken(); // Consuming 'raise' token.
    Raise->StructInit = ParseStructInitializer();
    if (CFunc) {
        CFunc->HasRaiseStmt = true;
    }
    return Raise;
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

            if (CTok.Is('(') && PeekToken(1).Is(TokenKind::STRING_LITERAL)) {
                if (Mods & ModKinds::LINKNAME) {
                    Error(CTok, "Linkage name already by linkname modifier");
                }
                NextToken(); // Consuming '(' token.
                NativeModifierName = CTok.GetText();

                NativeModifierName = NativeModifierName.substr(1); // Skip over " character.
                if (!NativeModifierName.empty() && *(NativeModifierName.end()-1) == '"') {
                    NativeModifierName = NativeModifierName.substr(0, NativeModifierName.size() - 1);
                }
                if (NativeModifierName.empty()) {
                    Error(CTok, "Linkage name cannot be empty");
                }

                NextToken(); // Consuming string literal.
                Match(')');
            }

            break;
        }
        case TokenKind::KW_LINKNAME: {
            if (Mods & ModKinds::LINKNAME)
                Error(CTok, "Duplicate modifier");
            Mods |= ModKinds::LINKNAME;
            NextToken();
            Match('(');
            // TODO: Better error recovery.
            if (CTok.Is(TokenKind::STRING_LITERAL)) {
                NativeModifierName = CTok.GetText();
                NextToken();

                NativeModifierName = NativeModifierName.substr(1); // Skip over " character.
                if (!NativeModifierName.empty() && *(NativeModifierName.end() - 1) == '"') {
                    NativeModifierName = NativeModifierName.substr(0, NativeModifierName.size() - 1);
                }
                if (NativeModifierName.empty()) {
                    Error(CTok, "Linkage name cannot be empty");
                }
            } else {
                Error(CTok, "Expected string literal for linkname");
                if (PeekToken(1).Is(')')) {
                    NextToken();
                }
            }
            Match(')');
            break;
        }
        case TokenKind::KW_PRIVATE: {
            if (Mods & ModKinds::PRIVATE)
                Error(CTok, "Duplicate modifier");
            Mods |= ModKinds::PRIVATE;
            NextToken();
            break;
        }
        case TokenKind::KW_READONLY: {
            if (Mods & ModKinds::READONLY)
                Error(CTok, "Duplicate modifier");
            Mods |= ModKinds::READONLY;
            NextToken();
            break;
        }
        case TokenKind::KW_WRITEONLY: {
            if (Mods & ModKinds::WRITEONLY)
                Error(CTok, "Duplicate modifier");
            Mods |= ModKinds::WRITEONLY;
            NextToken();
            break;
        }
        case TokenKind::KW_DLLIMPORT: {
            if (Mods & ModKinds::DLLIMPORT)
                Error(CTok, "Duplicate modifier");
            Mods |= ModKinds::DLLIMPORT;
            NextToken();
            break;
        }
        default:
            return Mods;
    }
    }
}

arco::Type* arco::Parser::ParseType(bool AllowImplicitArrayType, bool ForRetTy) {

    arco::Type* Ty = CTok.Is(TokenKind::KW_FN) ? ParseFunctionType() : ParseBasicType(ForRetTy);
    if (Ty == Context.ErrorType) {
        return Ty;
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

        if (PeekToken(1).Is('*')) {
            // Slice type. For now we only allow for a single dimensional
            // slice type. In the future it may be nice to allow for multidimensional
            // slice types.
            NextToken(); // Consuming '[' token.
            NextToken(); // Consuming ']' token.
            Match(']');
            Ty = SliceType::Create(Ty, Context);
            return Ty;
        }

        llvm::SmallVector<Expr*, 2>     LengthExprs;
        llvm::SmallVector<SourceLoc, 2> ExpandedErrorLocs;

        bool IsImplicit = false;
        bool AlreadyReportedImplicitError = false, EncounteredNonImplicit = false;
        while (CTok.Is('[')) {
            NextToken(); // Consuming '[' token.

            if (CTok.Is(']') && AllowImplicitArrayType) {
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

arco::Type* arco::Parser::ParseFunctionType() {
    Match(TokenKind::KW_FN);

    llvm::SmallVector<TypeInfo> ParamTypes;
    Match('(', "For function type");
    if (CTok.IsNot(')')) {
        bool MoreParamTypes = false;
        do {
            Type* Ty;
            bool  HasConstAddress = false;
            if (CTok.Is(TokenKind::IDENT)) {
                switch (PeekToken(1).Kind) {
                case KW_CONST:
                    NextToken(); // Consuming the identifier.
                    NextToken(); // Consuming 'const' token.
                    HasConstAddress = true;
                    Ty = ParseType(false);
                    break;
                case TYPE_KW_START_CASES:
                case TokenKind::IDENT:
                    // Allowing the parameters to have names.
                    NextToken(); // Consuming the name identifier.
                    Ty = ParseType(false);
                    break;
                default:
                    Ty = ParseType(false);
                    break;
                }
            } else {
                if (CTok.Is(TokenKind::KW_CONST)) {
                    NextToken(); // Consuming 'const' token.
                    HasConstAddress = true;
                }
                Ty = ParseType(false);
            }
            if (Ty == Context.ErrorType) {
                SkipRecovery({ ',', ')' });
            }

            ParamTypes.push_back(TypeInfo{ Ty, HasConstAddress });

            MoreParamTypes = CTok.Is(',');
            if (MoreParamTypes) {
                NextToken(); // Consuming ','.
            } else if (CTok.IsNot(')')) {
                Error(CTok, "Unexpected token when parsing function type parameter types");
                SkipRecovery({ ')' });
            }
        } while (MoreParamTypes);
    }
    Match(')', "For function type");
    bool ReturnsConstAddress = false;
    if (CTok.Is(TokenKind::KW_CONST)) {
        NextToken(); // Consuming 'const' token.
        ReturnsConstAddress = true;
    }
    
    // TODO: allow the return type to be optional and default to void?
    Type* RetTy = ParseType(false, true);

    return FunctionType::Create(TypeInfo{ RetTy, ReturnsConstAddress }, std::move(ParamTypes), Context);
}

arco::Type* arco::Parser::ParseBasicType(bool ForRetTy) {
    arco::Type* Ty = nullptr;
    switch (CTok.Kind) {
    case TokenKind::KW_INT:      Ty = Context.IntType;     NextToken(); break;
    case TokenKind::KW_PTRSIZE:  Ty = Context.PtrsizeType; NextToken(); break;
    case TokenKind::KW_INT8:     Ty = Context.Int8Type;    NextToken(); break;
    case TokenKind::KW_INT16:    Ty = Context.Int16Type;   NextToken(); break;
    case TokenKind::KW_INT32:    Ty = Context.Int32Type;   NextToken(); break;
    case TokenKind::KW_INT64:    Ty = Context.Int64Type;   NextToken(); break;
    case TokenKind::KW_UINT8:    Ty = Context.UInt8Type;   NextToken(); break;
    case TokenKind::KW_UINT16:   Ty = Context.UInt16Type;  NextToken(); break;
    case TokenKind::KW_UINT32:   Ty = Context.UInt32Type;  NextToken(); break;
    case TokenKind::KW_UINT64:   Ty = Context.UInt64Type;  NextToken(); break;
    case TokenKind::KW_FLOAT32:  Ty = Context.Float32Type; NextToken(); break;
    case TokenKind::KW_FLOAT64:  Ty = Context.Float64Type; NextToken(); break;
    case TokenKind::KW_CHAR:     Ty = Context.CharType;    NextToken(); break;
    case TokenKind::KW_VOID:     Ty = Context.VoidType;    NextToken(); break;
    case TokenKind::KW_CSTR:     Ty = Context.CStrType;    NextToken(); break;
    case TokenKind::KW_BOOL:     Ty = Context.BoolType;    NextToken(); break;
    case TokenKind::IDENT: {
        
        bool IsGeneric = false;
        Identifier Name = Identifier(CTok.GetText());
        if (CFunc && CFunc->IsGeneric()) {
            // It possibly refers to a generic type.
            auto* GenData = CFunc->GenData;
            auto Itr = std::find_if(GenData->GenTys.begin(), GenData->GenTys.end(),
                [&Name](const GenericType* GenTy) {
                    return GenTy->GetName() == Name;
                });
            if (Itr != GenData->GenTys.end()) {
                Ty = *Itr;
                IsGeneric = true;
            }
        }

        if (!IsGeneric) {
            Ty = StructType::Create(Name, CTok.Loc, Context);
        }
        
        NextToken();
        break;
    }
    default:
        if (!ForRetTy) {
            Error(CTok, "Expected valid type");
        } else {
            Error(CTok, "function type requires return type");
        }
        Ty = Context.ErrorType;
        break;
    }
    return Ty;
}

llvm::SmallVector<arco::GenericType*> arco::Parser::ParseGenerics() {
    NextToken(); // Consuming 'generics' token.

    // TODO: Should probably pass the error information on over to the declaration
    // that uses the generic types so that they may also be considered a parse error.

    llvm::SmallVector<GenericType*> Generics;
    Match('<');
    bool MoreGenerics = false;
    ulen GenericIdx = 0;

    do {
        Token NameTok = CTok;
        Identifier Name = ParseIdentifier("Expected identifier for generic type");        
        auto Itr = std::find_if(Generics.begin(), Generics.end(),
            [&Name](const GenericType* GenTy) {
                return GenTy->GetName() == Name;
            });
        if (Itr != Generics.end()) {
            Error(NameTok, "Duplicate generic by name '%s'", Name);
        } else if (!Name.IsNull()) {
            GenericType* GenTy = GenericType::Create(Name, GenericIdx, Context);
            ++GenericIdx;
            Generics.push_back(GenTy);
        }

        MoreGenerics = CTok.Is(',');
        if (MoreGenerics) {
            NextToken();
        }
    } while (MoreGenerics);

    Match('>');
    return Generics;
}

//===-------------------------------===//
// Expressions
//===-------------------------------===//

arco::Expr* arco::Parser::ParseAssignmentAndExprs() {
    Expr* LHS = ParseExprAndCatch();
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
        Expr* E = ParseExprAndCatch();
        return NewBinaryOp(OpTok, LHS, E);
    }
    default:
        return ParseBinaryExpr(LHS);
    }
}

arco::Expr* arco::Parser::ParseExprAndCatch() {
    Expr* E = ParseExpr();
    if (CTok.Is(TokenKind::KW_CATCH)) {
        CatchError* Catch = NewNode<CatchError>(CTok);
        NextToken();
        PUSH_SCOPE();
        Identifier ErrorName = ParseIdentifier("Expected identifier for caught error variable");
        
        if (!ErrorName.IsNull()) {
            Catch->ErrorVar = CreateVarDecl(CTok, ErrorName, 0);
            FinishVarDecl(Catch->ErrorVar);
        }
        
        ParseScopeStmts(Catch->Scope);
        POP_SCOPE();
        Catch->CaughtExpr = E;
        return Catch;
    }
    return E;
}

arco::Expr* arco::Parser::ParseExpr() {
    Expr* LHS = ParseBinaryExpr(ParsePrimaryAndPostfixUnaryExpr());

    if (CTok.Is('?')) {
        // Parsing ternary expression.
        Ternary* Tern = NewNode<Ternary>(CTok);
        Tern->Cond = LHS;
        NextToken();
        Tern->LHS = ParseBinaryExpr(ParsePrimaryAndPostfixUnaryExpr());
        Match(':', "for ternary expression");
        Tern->RHS = ParseBinaryExpr(ParsePrimaryAndPostfixUnaryExpr());
        return Tern;
    } else {
        return LHS;
    }
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
    //llvm::SmallVector<NumberLiteral, 32> FoldNumbers;

    Token Op = CTok, NextOp;
    llvm::DenseMap<u16, u32>::iterator OpItr;
    while ((OpItr = Context.BinaryOpsPrecedence.find(Op.Kind))
                 != Context.BinaryOpsPrecedence.end()) {

        NextToken(); // Consuming the operator

        // If an attempt is made to avoid node allocation again more careful
        // consideration of the lifetime of the nodes will need to be had.
        // Although considering at some point a linear allocator will just be
        // used for all the nodes it will likely be totally irrelevent to even
        // attempt the optimization so this code can likely stay as is.
        // 
        //
        /*Expr* RHS;
        if (LHS->Is(AstKind::NUMBER_LITERAL)) {
            // Checking to see if we can perform numeric folding.
            // TODO: Performance with not having to copy memory.
            NumberLiteral FoldNumber;
            if (CTok.Is(TokenKind::INT_LITERAL)) {
                ParseIntLiteral(&FoldNumber);
                FoldNumber.TempFold = true;
                FoldNumbers.push_back(FoldNumber);
                RHS = &FoldNumbers.back();
            } else if (CTok.Is(TokenKind::HEX_LITERAL)) {
                ParseHexLiteral(&FoldNumber);
                FoldNumber.TempFold = true;
                FoldNumbers.push_back(FoldNumber);
                RHS = &FoldNumbers.back();
            } else if (CTok.Is(TokenKind::FLOAT32_LITERAL) ||
                       CTok.Is(TokenKind::FLOAT64_LITERAL)) {
                ParseFloatLiteral(&FoldNumber);
                FoldNumber.TempFold = true;
                FoldNumbers.push_back(FoldNumber);
                RHS = &FoldNumbers.back();
            } else if (CTok.Is(TokenKind::BIN_LITERAL)) {
                ParseBinLiteral(&FoldNumber);
                FoldNumber.TempFold = true;
                FoldNumbers.push_back(FoldNumber);
                RHS = &FoldNumbers.back();
            } else {
                RHS = ParsePrimaryAndPostfixUnaryExpr();
            }
        } else {
            RHS = ParsePrimaryAndPostfixUnaryExpr();
        }*/
        Expr* RHS = ParsePrimaryAndPostfixUnaryExpr();
        NextOp = CTok;


        llvm::DenseMap<u16, u32>::iterator NextOpItr;
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
        const char* Buf = CTok.GetText().data();
        if (*(Buf - 1) == ' ') {
            // NOTE: hack this is used to disambiguate nonsense like:
            //       'if a++ b;'   and   'if a ++b;'
            return LHS;
        }

        UnaryOp* UOP = NewNode<UnaryOp>(CTok);
        UOP->Op =  TokenKind::POST_PLUS_PLUS;
        NextToken(); // Consuming the unary operator
        UOP->Value = LHS;
        return UOP;
    } else if (CTok.Is(TokenKind::MINUS_MINUS)) {
        const char* Buf = CTok.GetText().data();
        if (*(Buf - 1) == ' ') {
            // NOTE: hack this is used to disambiguate nonsense like:
            //       'if a++ b;'   and   'if a ++b;'
            return LHS;
        }

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
    case TokenKind::INT_LITERAL:     return ParseIntLiteral(NewNode<NumberLiteral>(CTok));
    case TokenKind::HEX_LITERAL:     return ParseHexLiteral(NewNode<NumberLiteral>(CTok));
    case TokenKind::BIN_LITERAL:     return ParseBinLiteral(NewNode<NumberLiteral>(CTok));
    case TokenKind::ERROR_FLOAT_LITERAL:
    case TokenKind::FLOAT32_LITERAL:
    case TokenKind::FLOAT64_LITERAL: return ParseFloatLiteral(NewNode<NumberLiteral>(CTok));
    case TokenKind::CHAR_LITERAL:    return ParseCharLiteral();
    case TokenKind::STRING_LITERAL:  return ParseStringLiteral();
    case TokenKind::IDENT: {

        if (AllowStructInitializer && PeekToken(1).Is('{')) {
            return ParseIdentPostfix(ParseStructInitializer());
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
                case TypeKind::UInt8:
                case TypeKind::UInt16:
                case TypeKind::UInt32:
                case TypeKind::UInt64:
                case TypeKind::Ptrsize:
                    Num->UnsignedIntValue = -Num->UnsignedIntValue;
                    break;
                case TypeKind::Float32:
                    Num->Float32Value = -Num->Float32Value;
                    break;
                case TypeKind::Float64:
                    Num->Float64Value = -Num->Float64Value;
                    break;
                case TypeKind::Char:
                    Num->SignedIntValue = -Num->SignedIntValue;
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
    case '&': case '*': case '!': case '~':
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
        Cast->ToType = ParseType(false);
        Match(')');
        Cast->Value = ParsePrimaryAndPostfixUnaryExpr();
        return Cast;
    }
    case TokenKind::KW_BITCAST: {
        TypeBitCast* Cast = NewNode<TypeBitCast>(CTok);
        NextToken(); // Consuming 'cast' token.
        Match('(');
        Cast->ToType = ParseType(false);
        Match(')');
        Cast->Value = ParsePrimaryAndPostfixUnaryExpr();
        return Cast;
    }
    case TokenKind::KW_SIZEOF: {
        SizeOf* SOf = NewNode<SizeOf>(CTok);
        NextToken(); // Consuming 'sizeof' token.
        Match('(');
        SOf->TypeToGetSizeOf = ParseType(false);
        SOf->Ty = Context.IntType;
        Match(')');
        return SOf;
    }
    case TokenKind::KW_TYPEOF: {
        TypeOf* TOf = NewNode<TypeOf>(CTok);
        NextToken(); // Consuming 'typeof' token.
        Match('(');
        TOf->TypeToGetTypeOf = ParseType(false);
        Match(')');
        return TOf;
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
        Alloc->TypeToAlloc = ParseType(false);
        if (Alloc->TypeToAlloc == Context.ErrorType) {
            SkipRecovery();
        }
        if (AllowStructInitializer && CTok.Is('{')) {
            NextToken(); // Consumign '{'.
            if (CTok.IsNot('}')) {
                ParseAggregatedValues(Alloc->Values, Alloc->NamedValues, RecoveryStrat::StructInitArgs);
            }
            Match('}', "for struct initializer");
        }
        return Alloc;
    }
    case TokenKind::KW_TRUE: {
        BoolLiteral* B = NewNode<BoolLiteral>(CTok);
        NextToken();
        B->TOF = true;
        B->Ty  = Context.BoolType;
        return B;
    }
    case TokenKind::KW_FALSE: {
        BoolLiteral* B = NewNode<BoolLiteral>(CTok);
        NextToken();
        B->TOF = false;
        B->Ty  = Context.BoolType;
        return B;
    }
    case TokenKind::KW_MOVEOBJ: {
        MoveObj* Move = NewNode<MoveObj>(CTok);
        NextToken();
        Match('(');
        Move->Value = ParseExpr();
        Match(')');
        return Move;
    }
    case TokenKind::KW_TRY: {
        TryError* Try = NewNode<TryError>(CTok);
        NextToken();
        Try->Value = ParseExpr();
        return Try;
    }
    default:
        Error(CTok.Loc, "Expected an expression");
        ErrorNode* Err = NewNode<ErrorNode>(CTok);
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
arco::NumberLiteral* arco::Parser::ParseIntLiteral(NumberLiteral* Number) {
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
        IntValue = IntValue * 10 + ((u64)C - '0');

        // Check for overflow
        if (IntValue / 10 < PrevValue) {
            Error(CTok, "Integer value is too large");
            break;
        }
    }

    return FinalizeIntLiteral(Idx, IntValue, Number);
}

arco::NumberLiteral* arco::Parser::ParseHexLiteral(NumberLiteral* Number) {
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
        IntValue = IntValue * 16 + HexToDecimalMapping[C];

        // Check for overflow
        if (IntValue / 16 < PrevValue) {
            Error(CTok, "Integer value is too large");
            break;
        }
    }

    return FinalizeIntLiteral(Idx, IntValue, Number);
}

arco::NumberLiteral* arco::Parser::ParseBinLiteral(NumberLiteral* Number) {
    llvm::StringRef Text = CTok.GetText();

    ulen Idx = 2; // Skip 0b
    u64 IntValue = 0, PrevValue;
    while (Idx < Text.size()) {
        char C = Text[Idx];
        if (C == NUMBER_SEPERATOR) {
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

    return FinalizeIntLiteral(Idx, IntValue, Number);
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

arco::NumberLiteral* arco::Parser::FinalizeIntLiteral(ulen Idx, u64 IntValue, NumberLiteral* Number) {
    
    llvm::StringRef Text = CTok.GetText();
    
    if (Idx < Text.size()) {
        if (Text[Idx] == 'u') {
            // The number is forced to be unsigned.
            if (IntValue <= std::numeric_limits<u32>::max()) {
                Number->Ty = Context.PtrsizeType;
            } else {
                Number->Ty = Context.UInt64Type;
            }
            Number->UnsignedIntValue = IntValue;
        } else {
            // The number is set to be an explicit type.

            ++Idx; // Skipping over '\'' character.
            if (Idx+1 < Text.size()) {
                switch (Text[Idx]) {
                case 'i':
                case 'u': {
                    bool Unsigned = Text[Idx] == 'u';
                    ++Idx;
                    if (Text[Idx] == '8') {
                        if (Unsigned) {
                            Number->Ty = Context.UInt8Type;
                            Number->UnsignedIntValue = IntValue;
                            if (IntValue > std::numeric_limits<u8>::max()) {
                                Error(CTok, "Value is too large for an unsigned 8 bit integer");
                            }
                        } else {
                            Number->Ty = Context.Int8Type;
                            Number->SignedIntValue = IntValue;
                            if (IntValue > std::numeric_limits<i8>::max()) {
                                Error(CTok, "Value is too large for a signed 8 bit integer");
                            }
                        }
                    } else if (Idx+1 < Text.size()) {
                        if (Text[Idx] == '1' && Text[Idx+1] == '6') {
                            if (Unsigned) {
                                Number->Ty = Context.UInt16Type;
                                Number->UnsignedIntValue = IntValue;
                                if (IntValue > std::numeric_limits<u16>::max()) {
                                    Error(CTok, "Value is too large for an unsigned 16 bit integer");
                                }
                            } else {
                                Number->Ty = Context.Int16Type;
                                Number->SignedIntValue = IntValue;
                                if (IntValue > std::numeric_limits<i16>::max()) {
                                    Error(CTok, "Value is too large for a signed 16 bit integer");
                                }
                            }
                        } else if (Text[Idx] == '3' && Text[Idx+1] == '2') {
                            if (Unsigned) {
                                Number->Ty = Context.UInt32Type;
                                Number->UnsignedIntValue = IntValue;
                                if (IntValue > std::numeric_limits<u32>::max()) {
                                    Error(CTok, "Value is too large for an unsigned 32 bit integer");
                                }
                            } else {
                                Number->Ty = Context.Int32Type;
                                Number->SignedIntValue = IntValue;
                                if (IntValue > std::numeric_limits<i32>::max()) {
                                    Error(CTok, "Value is too large for a signed 32 bit integer");
                                }
                            }
                        } else if (Text[Idx] == '6' && Text[Idx+1] == '4') {
                            if (Unsigned) {
                                Number->Ty = Context.UInt64Type;
                                Number->UnsignedIntValue = IntValue;
                            } else {
                                Number->Ty = Context.Int64Type;
                                Number->SignedIntValue = IntValue;
                                if (IntValue > std::numeric_limits<i64>::max()) {
                                    Error(CTok, "Value is too large for a signed 64 bit integer");
                                }
                            }
                        } else {
                            // Invalid type information from lexer.
                            Number->Ty = Context.ErrorType;
                        }
                    } else {
                        // Invalid type information from lexer.
                        Number->Ty = Context.ErrorType;
                    }
                    break;
                }
                default:
                    // Invalid type information from lexer.
                    Number->Ty = Context.ErrorType;
                    break;
                }
            } else {
                // Invalid type information from lexer.
                Number->Ty = Context.ErrorType;
            }
        }
    } else {
        // The number is an integer based on the size of
        // the integer.

        bool Unsigned = false;
        if (IntValue <= std::numeric_limits<i32>::max()) {
            Number->Ty = Context.IntType;
        } else if (IntValue <= std::numeric_limits<i64>::max()) {
            Number->Ty = Context.Int64Type;
        } else {
            Number->Ty = Context.UInt64Type;
            Unsigned = true;
        }

        if (Unsigned) {
            Number->UnsignedIntValue = IntValue;
        } else {
            Number->SignedIntValue = IntValue;
        }
    }

    NextToken();
    return Number;
}

arco::NumberLiteral* arco::Parser::ParseFloatLiteral(NumberLiteral* Number) {
    if (CTok.Is(TokenKind::ERROR_FLOAT_LITERAL)) {
        // Error generated during lexing so not even going
        // to attempt to parse.
        Number->Ty = Context.Float64Type;
    } else if (CTok.Is(TokenKind::FLOAT32_LITERAL)) {
        Number->Ty = Context.Float32Type;
        FD::FloatParseError E;
        Number->Float32Value = FD::ToIEEESingle(CTok.GetText(), E);
        if (E != FD::FloatParseError::NONE) {
            if (E == FD::FloatParseError::OVERFLOWED) {
                Error(CTok, "Float value too large");
            } else {
                Error(CTok, "Float value underflowed (It is too small)");
            }
        }
    } else {
        Number->Ty = Context.Float64Type;
        FD::FloatParseError E;
        Number->Float64Value = FD::ToIEEEDouble(CTok.GetText(), E);
        if (E != FD::FloatParseError::NONE) {
            if (E == FD::FloatParseError::OVERFLOWED) {
                Error(CTok, "Float value too large");
            } else {
                Error(CTok, "Float value underflowed (It is too small)");
            }
        }
    }

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
        ParseAggregatedValues(Call->Args, Call->NamedArgs, RecoveryStrat::CallArgs);
    }

    Match(')', "for function call");
    return Call;
}

arco::StructInitializer* arco::Parser::ParseStructInitializer() {

    StructInitializer* StructInit = NewNode<StructInitializer>(CTok);

    Identifier StructName = Identifier(CTok.GetText());
    StructType* Ty = StructType::Create(StructName, CTok.Loc, Context);
    NextToken();

    NextToken(); // Consuming '{' token.
    StructInit->Ty = Ty;

    if (CTok.IsNot('}')) {
        ParseAggregatedValues(StructInit->Args, StructInit->NamedArgs, RecoveryStrat::StructInitArgs);
    }

    Match('}', "for struct initializer");
    
    return StructInit;
}

arco::Array* arco::Parser::ParseArray() {
    Array* Arr = NewNode<Array>(CTok);
    Match('[');

    switch (CTok.Kind) {
    case TYPE_KW_START_CASES:
        Arr->ReqBaseType = ParseBasicType();
        Match(']');
        Match('[');
        break;
    }

    if (ArrayDepthCount == 0) {
        // TODO: memset for performance?

        // Clearing the sizes for the new array.
        for (ulen i = 0; i < MAX_ARRAY_DEPTH; i++) {
            LargestArrayLengthAtDepth[i] = 0;
        }
    }

    ++ArrayDepthCount;
    if (CTok.IsNot(']')) {
        RecoveryStrat PrevStrat = RecStrat;
        RecStrat = RecoveryStrat::Array;
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
        RecStrat = PrevStrat;
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

void arco::Parser::ParseAggregatedValues(llvm::SmallVector<NonNamedValue>& Values,
                                         llvm::SmallVector<NamedValue>& NamedValues,
                                         RecoveryStrat Strat) {
    u16 EndDelimTok = Strat == RecoveryStrat::CallArgs ? ')' : '}';
    bool AllowTrailingComma = Strat != RecoveryStrat::CallArgs;

    RecoveryStrat PrevStrat = RecStrat;
    RecStrat = Strat;

    bool MoreValues = false;
    bool AlreadyReportedErrAboutNamedValuesOrder = false;
    do {

        if (CTok.Is(TokenKind::IDENT) && PeekToken(1).Is('=')) {

            NamedValue NamedVal;
            NamedVal.NameLoc = CTok.Loc;
            CREATE_EXPANDED_SOURCE_LOC(
                NamedVal.Name = ParseIdentifier("Expected identifier for argument name");
                NextToken(); // Consuming '=' token.
                NamedVal.AssignValue = ParseExpr();,
                NamedVal.ExpandedLoc
            );

            auto Itr = std::find_if(NamedValues.begin(), NamedValues.end(),
                [&NamedVal](const NamedValue& V) {
                    return V.Name == NamedVal.Name;
                });
            if (Itr != NamedValues.end()) {
                Error(NamedVal.ExpandedLoc, "Duplicate named value");
            }

            NamedValues.push_back(NamedVal);
        } else {

            NonNamedValue NonNamedVal;
            CREATE_EXPANDED_SOURCE_LOC(
                NonNamedVal.E = ParseExpr(),
                NonNamedVal.ExpandedLoc
            );

            Values.push_back(NonNamedVal);

            if (!NamedValues.empty() && NonNamedVal.E->Ty != Context.ErrorType &&
                !AlreadyReportedErrAboutNamedValuesOrder) {
                Error(NonNamedVal.ExpandedLoc, "Non-named argumens should come before named arguments");
                AlreadyReportedErrAboutNamedValuesOrder = true;
            }
        }

        if (CTok.Is(',')) {
            NextToken(); // Consuming ','
            MoreValues = true;
            if (AllowTrailingComma && CTok.Is(EndDelimTok)) {
                MoreValues = false;
            }
        } else if (CTok.Is(EndDelimTok)) {
            MoreValues = false;
        } else {
            Error(CTok,
                  "Unexpected token when parsing values for %s",
                   Strat == RecoveryStrat::CallArgs ? "function call" : "struct initializer");
            SkipRecovery();
            MoreValues = false;
        }
    } while (MoreValues);
    RecStrat = PrevStrat;
}

template<typename T>
T arco::Parser::FoldInt(Token OpTok, T LHS, T RHS, bool& OpApplies) {
    OpApplies = true;
    switch (OpTok.Kind) {
    case '+': {
        if (RHS > 0 && LHS > std::numeric_limits<T>::max() - RHS) {
            Error(OpTok.Loc, "Operation results in overflow for type '%s'",
                Type::GetIntTypeBasedOnByteSize(sizeof(T), std::is_signed_v<T>, Context)->ToString());
        }
        if (RHS < 0 && LHS < std::numeric_limits<T>::min() - RHS) {
            Error(OpTok.Loc, "Operation results in underflow for type '%s'",
                Type::GetIntTypeBasedOnByteSize(sizeof(T), std::is_signed_v<T>, Context)->ToString());
        }
        
        T Result = LHS + RHS;
        return Result;
    }
    case '-': {
        if (RHS < 0 && LHS > std::numeric_limits<T>::max() + RHS) {
            Error(OpTok.Loc, "Operation results in overflow for type '%s'",
                Type::GetIntTypeBasedOnByteSize(sizeof(T), std::is_signed_v<T>, Context)->ToString());
        }
        if (RHS > 0 && LHS < std::numeric_limits<T>::min() + RHS) {
            Error(OpTok.Loc, "Operation results in underflow for type '%s'",
                Type::GetIntTypeBasedOnByteSize(sizeof(T), std::is_signed_v<T>, Context)->ToString());
        }

        T Result = LHS - RHS;
        return Result;
    }
    case '*': {
        if (RHS != 0 && LHS != 0) {
            if (LHS > std::numeric_limits<T>::max() / RHS) {
                Error(OpTok.Loc, "Operation results in overflow for type '%s'",
                    Type::GetIntTypeBasedOnByteSize(sizeof(T), std::is_signed_v<T>, Context)->ToString());
            }
            if (LHS < std::numeric_limits<T>::min() / RHS) {
                Error(OpTok.Loc, "Operation results in underflow for type '%s'",
                    Type::GetIntTypeBasedOnByteSize(sizeof(T), std::is_signed_v<T>, Context)->ToString());
            }
        }
        
        T Result = LHS * RHS;
        return Result;
    }
    case '%': {
        if (RHS == 0) {
            Error(OpTok.Loc, "Division by zero");
            return 0;
        }
        T Result = LHS % RHS;
        return Result;
    }
    case '/': {
        if (RHS == 0) {
            Error(OpTok.Loc, "Division by zero");
            return 0;
        }
        T Result = LHS / RHS;
        return Result;
    }
    case TokenKind::LT_LT: { 
        if (RHS < 0) {
            Error(OpTok.Loc, "Cannot shift using a negative value");
        } else if (static_cast<ulen>(RHS) - 1 > sizeof(T) * 8) {
            Error(OpTok.Loc, "Shifting bits larger than bit size of type '%s'",
                Type::GetIntTypeBasedOnByteSize(sizeof(T), std::is_signed_v<T>, Context)->ToString());
        }
        T Result = LHS << RHS;
        return Result;
    }
    case TokenKind::GT_GT: {
        if (RHS < 0) {
            Error(OpTok.Loc, "Cannot shift using a negative value");
        } else if (static_cast<ulen>(RHS) - 1 > sizeof(T) * 8) {
            Error(OpTok.Loc, "Shifting bits larger than bit size of type '%s'",
                Type::GetIntTypeBasedOnByteSize(sizeof(T), std::is_signed_v<T>, Context)->ToString());
        }
        T Result = LHS >> RHS;
        return Result;
    }
    case '<':               return LHS < RHS;
    case '>':               return LHS > RHS;
    case TokenKind::LT_EQ:  return LHS <= RHS;
    case TokenKind::GT_EQ:  return LHS >= RHS;
    case TokenKind::EQ_EQ:  return LHS == RHS;
    case TokenKind::EXL_EQ: return LHS != RHS;
    case '&':               return LHS & RHS;
    case '^':               return LHS ^ RHS;
    case '|':               return LHS | RHS;
    default:
        OpApplies = false;
        return 0;
    }
}

template<typename T>
T arco::Parser::FoldFloat(Token OpTok, T LHSVal, T RHSVal, bool& OpApplies) {
    // TODO: Check for underflow and overflow?

    OpApplies = true;
    switch (OpTok.Kind) {
    case '+': {
        T Result = LHSVal + RHSVal;
        return Result;
    }
    case '-': {
        T Result = LHSVal - RHSVal;
        return Result;
    }
    case '*': {
        T Result = LHSVal * RHSVal;
        return Result;
    }
    case '/': {
        if (RHSVal == 0) {
            Error(OpTok.Loc, "Division by zero");
            return 0;
        }
        T Result = LHSVal / RHSVal;
        return Result;
    }
    case '<':               return LHSVal < RHSVal;
    case '>':               return LHSVal > RHSVal;
    case TokenKind::LT_EQ:  return LHSVal <= RHSVal;
    case TokenKind::GT_EQ:  return LHSVal >= RHSVal;
    case TokenKind::EQ_EQ:  return LHSVal == RHSVal;
    case TokenKind::EXL_EQ: return LHSVal != RHSVal;
    default:
        OpApplies = false;
        return 0;
    }
}

arco::Expr* arco::Parser::FoldNumbers(Token OpTok, NumberLiteral* LHS, NumberLiteral* RHS, NumberLiteral* Result) {
    // TODO: Could probably optimize this with adding bitsets to the kind information
    //       of the type kinds.

    u16 Op = OpTok.Kind;

#define APPLY_OP(V, FoldFunc) \
bool OpApplies;               \
Result->V = FoldFunc(OpTok,  \
    LHS->V,                   \
    RHS->V,                   \
    OpApplies);
    

#define FOLD(V, FoldFunc) {  \
    APPLY_OP(V, FoldFunc)    \
if (OpApplies)               \
    return Result; }

    Type* OrgLHSTy = LHS->Ty, * OrgRHSTy = RHS->Ty;
    switch (OrgLHSTy->GetKind()) {
    case TypeKind::Float32:
        if (OrgRHSTy->GetKind() == TypeKind::Float64) {
            LHS->Ty = Context.Float64Type;
            FOLD(Float64Value, FoldFloat);
        } else {
            // Promote everything else to float32
            FOLD(Float32Value, FoldFloat);
        }
        break;
    case TypeKind::Float64:
        FOLD(Float64Value, FoldFloat)
            break;
    case TypeKind::Int:
    case TypeKind::Ptrsize:
    case TypeKind::Int8:
    case TypeKind::UInt8:
    case TypeKind::Int16:
    case TypeKind::UInt16:
    case TypeKind::Int32:
    case TypeKind::UInt32:
    case TypeKind::Int64:
    case TypeKind::UInt64:
    case TypeKind::Char:
        if (OrgRHSTy->GetKind() == TypeKind::Float64) {
            LHS->Ty = Context.Float64Type;
            FOLD(Float64Value, FoldFloat);
        } else if (OrgRHSTy->GetKind() == TypeKind::Float32) {
            LHS->Ty = Context.Float32Type;
            FOLD(Float32Value, FoldFloat);
        } else {
            bool Error = false;
            Type* UseType;
            if (OrgLHSTy->GetKind() == TypeKind::Int) {
                UseType = OrgRHSTy;
            } else if (OrgRHSTy->GetKind() == TypeKind::Int) {
                UseType = OrgLHSTy;
            } else {
                if (OrgLHSTy->IsSigned() != OrgRHSTy->IsSigned()) {
                    // Not allowing this. Signed unsigned mismatch will be considered
                    // an error unless they are using the system integer type.
                    Error = true;
                } else if (OrgLHSTy->GetKind() == TypeKind::Ptrsize) {
                    UseType = OrgRHSTy;
                } else if (OrgRHSTy->GetKind() == TypeKind::Ptrsize) {
                    UseType = OrgLHSTy;
                } else {
                    ulen Size1 = OrgLHSTy->GetTrivialTypeSizeInBytes();
                    ulen Size2 = OrgRHSTy->GetTrivialTypeSizeInBytes();
                    ulen UseSize = Size1 > Size2 ? Size1 : Size2;
                    UseType = Type::GetIntTypeBasedOnByteSize(UseSize, OrgLHSTy->IsSigned(), Context);
                }
            }
            if (!Error) {
                LHS->Ty = UseType;
                switch (UseType->GetKind()) {
                case TypeKind::Int: {
                    APPLY_OP(SignedIntValue, FoldInt);
                    // Promote the type to a larger size if it did not fit into 32 bits.
                    if (LHS->SignedIntValue > std::numeric_limits<i32>::max()) {
                        LHS->Ty = Context.Int64Type;
                    }
                    if (OpApplies)
                        return LHS;
                    break;
                }
                case TypeKind::Int8:
                case TypeKind::Char:
                    FOLD(SignedInt8Value, FoldInt);
                    break;
                case TypeKind::UInt8:
                    FOLD(UnsignedInt8Value, FoldInt);
                    break;
                case TypeKind::Int16:
                    FOLD(SignedInt16Value, FoldInt);
                    break;
                case TypeKind::UInt16:
                    FOLD(UnsignedInt16Value, FoldInt);
                    break;
                case TypeKind::Int32:
                    FOLD(SignedInt32Value, FoldInt);
                    break;
                case TypeKind::UInt32:
                    FOLD(UnsignedInt32Value, FoldInt);
                    break;
                case TypeKind::Int64:
                    FOLD(SignedIntValue, FoldInt);
                    break;
                case TypeKind::UInt64:
                case TypeKind::Ptrsize:
                    FOLD(UnsignedIntValue, FoldInt);
                    break;
                default:
                    assert(!"unreachable");
                    break;
                }
            }
        }
        break;
    }

    // If we get here it means there is some type of semantic error so
    // just going to create the node and let the semantic analysis deal
    // with the problem.

    BinaryOp* BinOp = NewNode<BinaryOp>(OpTok);
    BinOp->Op = OpTok.Kind;
    if (LHS->TempFold) {
        BinOp->LHS = NewNode<NumberLiteral>(LHS->Loc);
    } else {
        BinOp->LHS = LHS;
    }
    BinOp->LHS->Ty = OrgLHSTy; // Assign original type info so it reports correctly in semantic analysis.
    if (RHS->TempFold) {
        BinOp->RHS = NewNode<NumberLiteral>(RHS->Loc);
    } else {
        BinOp->RHS = RHS;
    }
    BinOp->RHS->Ty = OrgRHSTy; // Assign original type info so it reports correctly in semantic analysis.
    return BinOp;

#undef FOLD
#undef APPLY_OP
}

arco::Expr* arco::Parser::NewBinaryOp(Token OpTok, Expr* LHS, Expr* RHS) {
    if (LHS->Is(AstKind::NUMBER_LITERAL) && RHS->Is(AstKind::NUMBER_LITERAL)) {
        // We fold the numbers!
        NumberLiteral* LHSNumber = static_cast<NumberLiteral*>(LHS);
        NumberLiteral* RHSNumber = static_cast<NumberLiteral*>(RHS);

        if (!LHSNumber->TempFold) {
            return FoldNumbers(OpTok, LHSNumber, RHSNumber, LHSNumber);
        } if (!RHSNumber->TempFold) {
            return FoldNumbers(OpTok, LHSNumber, RHSNumber, RHSNumber);
        } else {
            // Neither has memory that can be used, well going to create some then!
            // TODO: Is this actually needed? Can we not just rely on the stack based
            //       node since we know there will be a node later on that has heap memory?
            NumberLiteral* ResultNode = NewNode<NumberLiteral>(LHSNumber->Loc);
            // TODO: memcpying here seems really messy.
            memcpy(ResultNode, LHSNumber, sizeof(NumberLiteral));
            ResultNode->TempFold = false;
            return FoldNumbers(OpTok, LHSNumber, RHSNumber, ResultNode);
        }
    }/* else if (LHS->Is(AstKind::NUMBER_LITERAL)) {
        BinaryOp* BinOp = NewNode<BinaryOp>(OpTok);
        BinOp->Op = OpTok.Kind;
        NumberLiteral* LHSNumber = static_cast<NumberLiteral*>(LHS);
        if (LHSNumber->TempFold) {
            BinOp->LHS = NewNode<NumberLiteral>(LHS->Loc);
            BinOp->LHS->Ty = LHS->Ty;
        } else {
            BinOp->LHS = LHS;
        }
        BinOp->RHS = RHS;
        return BinOp;
    } else if (RHS->Is(AstKind::NUMBER_LITERAL)) {
        BinaryOp* BinOp = NewNode<BinaryOp>(OpTok);
        BinOp->Op = OpTok.Kind;
        BinOp->LHS = LHS;
        NumberLiteral* RHSNumber = static_cast<NumberLiteral*>(LHS);
        if (RHSNumber->TempFold) {
            BinOp->RHS = NewNode<NumberLiteral>(RHS->Loc);
            BinOp->RHS->Ty = RHS->Ty;
        } else {
            BinOp->RHS = RHS;
        }
        return BinOp;
    }*/ else {
        BinaryOp* BinOp = NewNode<BinaryOp>(OpTok);
        BinOp->Op = OpTok.Kind;
        BinOp->LHS = LHS;
        BinOp->RHS = RHS;
        return BinOp;
    }
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

void arco::Parser::SkipRecovery(llvm::DenseSet<u16> IncludeSet) {
    bool IncludeVarDeclAndAssign = true;
    switch (RecStrat) {
    case RecoveryStrat::Params:
        IncludeSet.insert(',');
        IncludeSet.insert(')');
        break;
    case RecoveryStrat::CallArgs:
        IncludeSet.insert(',');
        IncludeSet.insert(')');
        // In case the user forgot to type 'fn' keyword and meant for
        // the call to actually be a function declaration.
        IncludeVarDeclAndAssign = false;
        break;
    case RecoveryStrat::StructInitArgs:
        IncludeSet.insert(',');
        // Note: } is already considered.
        break;
    case RecoveryStrat::Array:
        IncludeSet.insert(',');
        IncludeSet.insert(']');
        break;
    default:
        break;
    }
    
    while (true) {
    switch (CTok.Kind) {
    // Statements
    case TokenKind::KW_RETURN:
    case TokenKind::KW_FN:
    case TokenKind::KW_LOOP:
    case TokenKind::KW_IF:
    case TokenKind::KW_BREAK:
    case TokenKind::KW_CONTINUE:
    case TokenKind::KW_DELETE:
    case TokenKind::KW_NATIVE:
    case TokenKind::KW_PRIVATE:
    case TokenKind::KW_READONLY:
    case TokenKind::KW_WRITEONLY:
    case '{':
    case '}':
        return;
    case ';':
    case TokenKind::TK_EOF:
        return;
    case TokenKind::IDENT:
        if (!IncludeVarDeclAndAssign) {
            // Skip and continue
            NextToken();
            break;
        }

        switch (PeekToken(1).Kind) {
        case TYPE_KW_START_CASES:
        case TokenKind::IDENT:
        case TokenKind::KW_CONST:
        case TokenKind::COL_EQ:
        case TokenKind::COL_COL:
        case '=':
            // Variable declaration or assignment
            return;
        }
        // Skip and continue
        NextToken();
        break;
    default:
        if (IncludeSet.contains(CTok.Kind)) {
            return;
        }

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
