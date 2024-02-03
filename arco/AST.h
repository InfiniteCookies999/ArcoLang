#ifndef ARCO_AST_H
#define ARCO_AST_H

#include <llvm/ADT/DenseMap.h>
#include <llvm/ADT/SmallVector.h>

#include "Source.h"
#include "Identifier.h"
#include "Types.h"

namespace llvm {
	class Function;
	class Value;
	class StructType;
	namespace Intrinsic {
		typedef unsigned ID;
	}
}

namespace arco {

	struct AstNode;
	struct FuncDecl;
	struct VarDecl;
	struct Expr;
	using ScopeStmts = llvm::SmallVector<AstNode*, 8>;
	using FuncsList  = llvm::SmallVector<FuncDecl*, 4>;

	enum class AstKind {
		
		ERROR,

		FUNC_DECL,
		VAR_DECL,
		VAR_DECL_LIST,
		STRUCT_DECL,

		RETURN,
		IF,
		BREAK,
		CONTINUE,
		PREDICATE_LOOP,
		RANGE_LOOP,
		NESTED_SCOPE,
		DELETE,

		NUMBER_LITERAL,
		STRING_LITERAL,
		NULLPTR,
		BOOL_LITERAL,
		BINARY_OP,
		UNARY_OP,
		IDENT_REF,
		FIELD_ACCESSOR,
		THIS_REF,
		FUNC_CALL,
		ARRAY,
		ARRAY_ACCESS,
		TYPE_CAST,
		STRUCT_INITIALIZER,
		HEAP_ALLOC,
		SIZEOF

	};

	enum ModKinds {
		NATIVE  = 0x01,
		CONST   = 0x02,
		PRIVATE = 0x04,
	};
	using Modifiers = u16;

	struct Namespace {

		llvm::DenseMap<Identifier, FuncsList>   Funcs;

		llvm::DenseMap<Identifier, StructDecl*> Structs;

		llvm::DenseMap<Identifier, VarDecl*>    GlobalVars;
	};

	struct Module {

		llvm::StringRef Name;

		llvm::DenseMap<Identifier, Namespace*> Namespaces;
		Namespace* DefaultNamespace;
	};

	struct FileScope {

		struct StructOrNamespaceImport {
			SourceLoc   ErrorLoc;
			Module*     Mod;
			Identifier  StructOrNamespace;
			Identifier  StructName;
			StructDecl* Struct;
		};

		struct StaticImport {
			SourceLoc  ErrorLoc;
			Module*    Mod;
			Identifier NamespaceName;
			Namespace* NSpace;
		};

		std::string                                         Path;
		SourceBuf                                           Buffer;
		llvm::DenseMap<Identifier, Namespace*>              NamespaceImports;
		llvm::DenseMap<Identifier, StructOrNamespaceImport> StructOrNamespaceImports;
		llvm::SmallVector<StaticImport>                     StaticImports;

		Module*    Mod;
		Namespace* UniqueNSpace = nullptr;

		bool ParsingErrors = false;

		enum class InvalidScopeKind {
			GLOBAL,
			STRUCT
		};

		// When encountering statements it is possible
		// that the statement is not considered valid in the
		// given context. If it is not, it is placed here and
		// reported about after parsing is completed.
		llvm::SmallVector<std::tuple<InvalidScopeKind, AstNode*>, 8> InvalidStmts;

		FileScope(std::string Path, SourceBuf Buffer)
			: Path(Path), Buffer(Buffer) {}
	};

	struct LexScope {
		SourceLoc  StartLoc;
		SourceLoc  EndLoc;
		ScopeStmts Stmts;
	};

	struct AstNode {
		SourceLoc Loc;
		AstKind   Kind;

		AstNode(AstKind Kind)
			: Kind(Kind) {}
	
		inline bool Is(AstKind Kind) { return this->Kind == Kind; }
		inline bool IsNot(AstKind Kind) { return this->Kind != Kind; }

	};

	struct Decl : AstNode {
		Decl(AstKind Kind) : AstNode(Kind) {}

		bool GenRequestedAlready = false;
		bool ParsingError        = false;
		bool HasBeenChecked      = false;

		Module*     Mod;

		FileScope* FScope;

		Modifiers  Mods;
		Identifier Name;
	};

	struct FuncDecl : Decl {
		FuncDecl() : Decl(AstKind::FUNC_DECL) {}

		llvm::Function* LLFunction = nullptr;

		// Zero means it is not a LLVMIntrinsic.
		llvm::Intrinsic::ID LLVMIntrinsicID = 0;

		bool ParamTypesChecked   = false;
		// If this is true then the function will return
		// a struct type as an integer and then the caller
		// will bitcast back to the struct type.
		bool UsesOptimizedIntRet = false;
		// If this is true then the function will pass
		// the return value as a parameter rather than
		// returning it.
		bool UsesParamRetSlot    = false;
		bool IsConstructor       = false;

		// Non-nullptr if the function is a member function.
		StructDecl* Struct = nullptr;

		Type*                          RetTy;
		llvm::SmallVector<VarDecl*, 2> Params;

		// Storing the variables that appear in the
		// function so they can be allocated at the
		// start of the function
		llvm::SmallVector<VarDecl*, 4> AllocVars;

		// If not empty it defines the explicit name for a linked
		// function.
		llvm::StringRef NativeName;

		ulen NumReturns = 0;

		LexScope Scope;

	};

	struct VarDecl : Decl {
		VarDecl() : Decl(AstKind::VAR_DECL) {}

		Type* Ty;

		ulen ParamIdx = -1;
		ulen FieldIdx = -1;

		// If the variable is declared inside a function
		// and returned.
		bool IsLocalRetValue = false;
		bool IsBeingChecked  = false;
		bool IsGlobal        = false;

		// One variable may depend on another variable in its
		// declaration.
		//
		// Ex.
		//    a int = 4;
		//    b int = a + 4;  // b depends on a.
		//  
		// When performing semantic analysis this variable stores
		// the variable it depends on.
		VarDecl* DepD = nullptr;

		llvm::Value* LLAddress = nullptr;

		// If not empty it defines the explicit name for a linked
		// variable.
		llvm::StringRef NativeName;

		Expr* Assignment = nullptr;

		inline bool IsParam() const {
			return ParamIdx != -1;
		}

		inline bool IsField() const {
			return FieldIdx != -1;
		}
	};

	struct StructDecl : Decl {
		StructDecl() : Decl(AstKind::STRUCT_DECL) {}

		llvm::SmallVector<VarDecl*>           Fields;
		FuncsList                             Constructors;
		FuncDecl*                             DefaultConstructor = nullptr;
		llvm::DenseMap<Identifier, FuncsList> Funcs; // Member functions.

		llvm::StructType* LLStructTy         = nullptr;
		llvm::Function* LLDefaultConstructor = nullptr;

		// At least one field has assignment.
		bool FieldsHaveAssignment = false;

		inline VarDecl* FindField(Identifier Name) {
			auto Itr = std::find_if(Fields.begin(), Fields.end(), [=](VarDecl* Field) {
				return Field->Name == Name;
			});
			if (Itr == Fields.end()) {
				return nullptr;
			}
			return *Itr;
		}
	};

	// Ex.   'a, b, c int = 4, 6, 3;'
	struct VarDeclList : AstNode {
		VarDeclList() : AstNode(AstKind::VAR_DECL_LIST) {}
		
		llvm::SmallVector<VarDecl*> List;
	};

	// Ex.   'if cond {}'
	struct IfStmt : AstNode {
		IfStmt() : AstNode(AstKind::IF) {}

		Expr*    Cond;
		AstNode* Else = nullptr; // 'else' or 'else if'
		LexScope Scope;
	};

	// Ex.  'return 4;'
	struct ReturnStmt : AstNode {
		ReturnStmt() : AstNode(AstKind::RETURN) {}

		Expr* Value = nullptr;
	};

	// Ex.  'break' or 'continue'
	struct LoopControlStmt : AstNode {
		// Kind is set during parsing.
		LoopControlStmt() : AstNode(AstKind::ERROR) {}
		
		// How many loops to break/continue from.
		ulen LoopCount = 1;
	};

	// Ex.  'loop cond {}'
	struct PredicateLoopStmt : AstNode {
		PredicateLoopStmt() : AstNode(AstKind::PREDICATE_LOOP) {}

		Expr* Cond = nullptr;
		
		LexScope Scope;
	};

	// Ex.  'loop i int = 0; i < 5; i++ {}'
	struct RangeLoopStmt : AstNode {
		RangeLoopStmt() : AstNode(AstKind::RANGE_LOOP) {}

		llvm::SmallVector<VarDecl*> Decls;
		Expr*                       Cond = nullptr;
		// Multiple increments are allowed by adding ','
		llvm::SmallVector<Expr*>    Incs;

		LexScope Scope;
	};

	// Ex.  '{ ... }'
	struct NestedScopeStmt : AstNode {
		NestedScopeStmt() : AstNode(AstKind::NESTED_SCOPE) {}

		LexScope Scope;
	};

	// Ex.  'delete a;'
	struct DeleteStmt : AstNode {
		DeleteStmt() : AstNode(AstKind::DELETE) {}
		
		Expr* Value;

	};

	struct Expr : AstNode {
		Expr(AstKind Kind) : AstNode(Kind) {}

		// Typically set during type checking.
		Type* Ty;
		// Somtimes an expression has a
		// cast type because LLVM expects
		// all types to be the same when
		// applying operators to them.
		//
		// The cast type behaves like an extra
		// node inserted into the tree as a way
		// to resolve the conflict.
		//
		//   Transformation:
		//         +                       +
		//       /   \         =>        /   \
		//      /     \                 /     \
		//  num(i16)  num(i32)    cast(i32)   num(i32)
		//                             |
		//                         num(i16)
		Type* CastTy = nullptr;

		// This indicates if the value
		// of the expression can be "folded"
		// by llvm. This means the value
		// can result in a constant value at
		// compile time rather than requiring
		// runtime to determine the expression's
		// result.
		bool IsFoldable = true;

		// If the expression results in an lvalue
		// for an address marked const then this
		// is true.
		bool HasConstAddress = false;

	};

	struct ErrorNode : Expr {
		ErrorNode() : Expr(AstKind::ERROR) {}
	};

	// Ex.  '43'
	struct NumberLiteral : Expr {
		NumberLiteral() : Expr(AstKind::NUMBER_LITERAL) {}

		union {
			i64    SignedIntValue;
			u64    UnsignedIntValue;
			float  Float32Value;
			double Float64Value;
		};
	};

	// Ex.  '"Hello World!"'
	struct StringLiteral : Expr {
		StringLiteral() : Expr(AstKind::STRING_LITERAL) {}

		std::string Characters;

	};

	// Ex.  'null'
	struct NullPtr : Expr {
		NullPtr() : Expr(AstKind::NULLPTR) {}
	};

	// Ex.  'true' or 'false'
	struct BoolLiteral : Expr {
		bool TOF;

		BoolLiteral()
			: Expr(AstKind::BOOL_LITERAL) {}
	};

	// Ex.  'a + b'
	struct BinaryOp : Expr {
		BinaryOp() : Expr(AstKind::BINARY_OP) {}

		u16   Op;
		Expr* LHS;
		Expr* RHS;
	};

	struct UnaryOp : Expr {
		UnaryOp() : Expr(AstKind::UNARY_OP) {}
	
		u16   Op;
		Expr* Value;
	};

	/// Reference to an identifier. Could be
	/// the identifier to a variable, a function
	/// or part of a scope.
	struct IdentRef : Expr {
		IdentRef() : Expr(AstKind::IDENT_REF) {}
		IdentRef(AstKind Kind) : Expr(Kind) {}

		Identifier Ident;

		inline bool IsFound() const {
			return RefKind != IdentRef::RK::NotFound;
		}

		enum class RK {
			Var,
			Funcs, // Plural because of function overloading.
			Struct,
			Import,
			NotFound
		} RefKind = RK::NotFound;

		union {
			FuncsList* Funcs;
			VarDecl*   Var;
			Namespace* NSpace;
		};
	};

	// Ex.  'obj.field'
	struct FieldAccessor : IdentRef {
		FieldAccessor() : IdentRef(AstKind::FIELD_ACCESSOR) {}

		// Request for the length of an array.
		bool IsArrayLength = false;

		Expr* Site;
	};

	// Ex.  'this'
	struct ThisRef : Expr {
		ThisRef() : Expr(AstKind::THIS_REF) {}
	};

	struct NonNamedValue {
		SourceLoc ExpandedLoc; // Start to end of the expression.
		Expr*     E;
	};

	struct FuncCall : Expr {
		FuncCall() : Expr(AstKind::FUNC_CALL) {}
	
		llvm::SmallVector<NonNamedValue, 2> Args;

		// What is being "called". For example,
		// "func(4)" is a call on the site "func"
		// which is an identifier.
		//
		// "abc[4]()" is a call on the site of a
		// variable within an array.
		Expr*     Site;
		FuncDecl* CalledFunc = nullptr;

	};

	// Ex.  '[ 4, 2, 42 ]'
	struct Array : Expr {
		Array() : Expr(AstKind::ARRAY) {}
	
		// This represents the number of elements
		// actually generated by the array.
		//
		// The reason this is needed is to ensure
		// that the size of an array at a certain
		// depth is equal to all other arrays at that
		// same depth.
		//
		// Example:
		//    [ [ 2, 54, 2 ], [ 66 ] ]
		//
		// At depth 0 there is only a single array consisting
		// of two elements, each of which are arrays.
		// At depth 1 there are two arrays. The first being
		// '[ 2, 54, 2 ]' and the second '[ 66 ]'. Every array
		// at depth two will take on the maximum length of the
		// arrays at that level. So both arrays will be length 3.
		ulen RequiredNumElements;
		Type* ReqBaseType = nullptr;

		llvm::SmallVector<Expr*, 4> Elements;

	};

	// Ex.  'a[4]'
	struct ArrayAccess : Expr {
		ArrayAccess() : Expr(AstKind::ARRAY_ACCESS) {}
	
		Expr* Site;
		Expr* Index;
	};

	// Ex.  'cast(int)'
	struct TypeCast : Expr {
		TypeCast() : Expr(AstKind::TYPE_CAST) {}

		Type* ToType;
		Expr* Value;

	};

	// Ex.  'StructName { 4, 2 }'
	struct StructInitializer : Expr {
		StructInitializer() : Expr(AstKind::STRUCT_INITIALIZER) {}

		// If not nullptr then it initializes by calling the
		// a constructor
		FuncDecl* CalledConstructor = nullptr;

		llvm::SmallVector<NonNamedValue, 2> Args;

	};

	// Ex.  'new int'
	struct HeapAlloc : Expr {
		HeapAlloc() : Expr(AstKind::HEAP_ALLOC) {}

		Type* TypeToAlloc;

		FuncDecl* CalledConstructor = nullptr;

		llvm::SmallVector<NonNamedValue, 2> Values;

	};

	// Ex.  'sizeof(int)'
	struct SizeOf : Expr {
		SizeOf() : Expr(AstKind::SIZEOF) {}

		Type* TypeToGetSizeOf;

	};
}

#endif // ARCO_AST_H