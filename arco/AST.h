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
    class Constant;
    class StructType;
    class GlobalVariable;
    namespace Intrinsic {
        typedef unsigned ID;
    }
}

namespace arco {

    struct AstNode;
    struct FuncDecl;
    struct VarDecl;
    struct Expr;
    struct Decl;
    using ScopeStmts = llvm::SmallVector<AstNode*>;
    using FuncsList  = llvm::SmallVector<FuncDecl*>;
    class DebugInfoEmitter;

    enum class AstKind {
        
        ERROR,

        FUNC_DECL,
        VAR_DECL,
        VAR_DECL_LIST,
        STRUCT_DECL,
        ENUM_DECL,
        INTERFACE_DECL,

        RETURN,
        IF,
        BREAK,
        CONTINUE,
        PREDICATE_LOOP,
        RANGE_LOOP,
        ITERATOR_LOOP,
        NESTED_SCOPE,
        DELETE,
        RANGE,

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
        SIZEOF,
        TYPEOF

    };

    enum ModKinds {
        NATIVE    = 0x01,
        PRIVATE   = 0x02,
        READONLY  = 0x04,
        WRITEONLY = 0x08,
        DLLIMPORT = 0x10,
    };
    using Modifiers = u16;

    struct Namespace {

        llvm::DenseMap<Identifier, FuncsList> Funcs;
        llvm::DenseMap<Identifier, Decl*>     Decls;

    };

    struct Module {

        llvm::StringRef Name;

        llvm::DenseMap<Identifier, Namespace*> Namespaces;
        Namespace* DefaultNamespace;
    };

    struct FileScope {

        struct StructOrNamespaceImport {
            SourceLoc   ErrorLoc;
            Identifier  ModOrNamespace;
            Identifier  StructOrNamespace;
            Identifier  StructName;
            Decl*       Decl   = nullptr;
            Namespace*  NSpace = nullptr;
        };

        struct StaticImport {
            SourceLoc  ErrorLoc;
            Module*    Mod;
            Identifier NamespaceName;
            bool       IsNamespaceUnderMod;
            Namespace* NSpace = nullptr;
        };

        std::string                                         Path;
        std::string                                         FullPath;
        SourceBuf                                           Buffer;
        llvm::DenseMap<Identifier, StructOrNamespaceImport> Imports;
        llvm::SmallVector<StaticImport>                     StaticImports;

        Module*    Mod;
        Namespace* UniqueNSpace = nullptr;

        bool ParsingErrors = false;

        enum class InvalidScopeKind {
            GLOBAL,
            STRUCT
        };

        DebugInfoEmitter* DIEmitter;

        // When encountering statements it is possible
        // that the statement is not considered valid in the
        // given context. If it is not, it is placed here and
        // reported about after parsing is completed.
        llvm::SmallVector<std::tuple<InvalidScopeKind, AstNode*>, 8> InvalidStmts;

        // TODO: May be better for performance to just store them in the default namespace
        // and to instead have a linked list to the next declaration.
        llvm::SmallVector<FuncsList> PrivateFuncs;
        llvm::SmallVector<Decl*>     PrivateDecls;

        FuncsList* FindFuncsList(Identifier Name);
        Decl* FindDecl(Identifier Name);

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
        bool IsBeingChecked      = false;

        Module*     Mod;

        FileScope* FScope;

        Modifiers  Mods;
        Identifier Name;

        inline bool IsStructLike() {
            return Kind == AstKind::STRUCT_DECL || Kind == AstKind::ENUM_DECL ||
                   Kind == AstKind::INTERFACE_DECL;
        }
    };

    struct StructDecl : Decl {
        StructDecl() : Decl(AstKind::STRUCT_DECL) {}

        // All interfaces that this struct implements.
        llvm::SmallVector<InterfaceDecl*> Interfaces;
        struct InterfaceHook {
            SourceLoc  ErrorLoc;
            Identifier Name;
        };
        // Needed until the interfaces are fixed up during semantic analysis.
        llvm::SmallVector<InterfaceHook>  InterfaceHooks;

        llvm::SmallVector<VarDecl*>           Fields;
        FuncsList                             Constructors;
        FuncDecl*                             DefaultConstructor = nullptr;
        FuncDecl*                             CopyConstructor    = nullptr;
        FuncDecl*                             MoveConstructor    = nullptr;
        FuncDecl*                             Destructor         = nullptr;
        llvm::DenseMap<Identifier, FuncsList> Funcs; // Member functions.

        u32 UniqueTypeId;

        // When the struct implements interfaces this offset is the offset
        // past the interface data.
        ulen VirtualOffset = 0;

        llvm::StructType* LLStructTy         = nullptr;
        llvm::Function* LLDefaultConstructor = nullptr;
        llvm::Function* LLInitVTableFunc     = nullptr;

        // At least one field has assignment.
        // This also takes into account fields which are structs
        // which themselves need assignment.
        bool FieldsHaveAssignment = false;
        // This is true if either their is a user defined
        // destructor or if the structure contains another
        // structure which needs destruction.
        bool NeedsDestruction = false;

        VarDecl* FindField(Identifier Name);
    };

    struct EnumDecl : Decl {
        EnumDecl() : Decl(AstKind::ENUM_DECL) {}

        struct EnumValue {
            SourceLoc  Loc;
            ulen       Index;
            Identifier Name;
            Expr*      Assignment;
        };
        llvm::SmallVector<EnumValue> Values;
        
        u32 UniqueTypeId;

        Type* ValuesType = nullptr;
        bool  IndexingInOrder = true;
        // If IndexingInOrder is true then a global array
        // is generated and the reordered indexes point into
        // the array to get the values out.
        llvm::Value* LLGlobalArray = nullptr;

        const EnumValue* FindValue(Identifier Name) const;
    };

    struct InterfaceDecl : Decl {
        InterfaceDecl() : Decl(AstKind::INTERFACE_DECL) {}

        u32 UniqueTypeId;
        ulen NumFuncs = 0;

        // Functions that must be implemented by any struct
        // which implements this interface.
        llvm::DenseMap<Identifier, FuncsList> Funcs;

    };

    struct FuncDecl : Decl {
        FuncDecl() : Decl(AstKind::FUNC_DECL) {}

        llvm::Function* LLFunction = nullptr;

        // Zero means it is not a LLVMIntrinsic.
        llvm::Intrinsic::ID LLVMIntrinsicID = 0;

        Identifier CallingConv;

        // TODO: This should be turned into a bitset.
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
        bool ReturnsConstAddress = false;
        bool IsDestructor        = false;
        bool IsCopyConstructor   = false;
        bool IsMoveConstructor   = false;
        bool IsVariadic          = false;
        
        // When this is not -1 it indicates the index number of
        // the function in an interface.
        u16 InterfaceIdx = -1;

        // Non-nullptr if the function is a member function.
        StructDecl* Struct = nullptr;
        // Non-nullptr if the function is an interface function.
        InterfaceDecl* Interface = nullptr;

        // When this function is a function of a struct that implements
        // an interface this is the function of the interface that this
        // function is mapped to.
        FuncDecl* MappedInterfaceFunc = nullptr;

        Type*                          RetTy;
        llvm::SmallVector<VarDecl*, 2> Params;

        // Storing the variables that appear in the
        // function so they can be allocated at the
        // start of the function
        llvm::SmallVector<VarDecl*, 4> AllocVars;

        struct InitializerValue {
            Identifier FieldName;
            Expr*      Assignment;
        };
        // Initializer values for constructors.
        llvm::SmallVector<InitializerValue> InitializerValues;

        Expr* GetInitializerValue(VarDecl* Field);

        // If not empty it defines the explicit name for a linked
        // function.
        llvm::StringRef NativeName;

        ulen NumReturns     = 0;
        ulen NumDefaultArgs = 0;

        LexScope Scope;

    };

    struct VarDecl : Decl {
        VarDecl() : Decl(AstKind::VAR_DECL) {}

        Type* Ty;

        ulen ParamIdx = -1;
        ulen FieldIdx = -1;
        // This is not equivalent to FieldIdx because if the struct implements
        // an interface it is offset so that it does not interfere with the
        // interface data.
        ulen LLFieldIdx;

        // If the variable is declared inside a function
        // and returned.
        bool IsLocalRetValue    = false;
        bool IsGlobal           = false;
        bool HasConstAddress    = false;
        bool TyIsInfered        = false;
        bool ImplicitPtr        = false;

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

        llvm::Value*    LLAddress     = nullptr;
        llvm::Constant* LLComptimeVal = nullptr;

        // If not empty it defines the explicit name for a linked
        // variable.
        llvm::StringRef NativeName;

        Expr* Assignment = nullptr;

        inline bool IsComptime() const {
            return Ty->IsNumber() && HasConstAddress;
        }

        inline bool IsParam() const {
            return ParamIdx != -1;
        }

        inline bool IsField() const {
            return FieldIdx != -1;
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

        llvm::SmallVector<AstNode*> InitNodes;
        Expr*                       Cond = nullptr;
        // Multiple increments are allowed by adding ','
        llvm::SmallVector<Expr*>    Incs;

        LexScope Scope;
    };

    // Ex.  'loop val : vals {}'
    struct IteratorLoopStmt : AstNode {
        IteratorLoopStmt() : AstNode(AstKind::ITERATOR_LOOP) {}

        VarDecl* VarVal;
        Expr*    IterOnExpr;

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
        // If true then calling a varargs function from a varargs
        // function which passes it's varargs.
        bool VarArgsPassAlong;

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

        // For some comparisons the type the LHS and
        // RHS are casted to is not the same as the
        // BinaryOp. Such as 5 < 3. The BinaryOp will
        // have type bool. The ResultType is the best
        // chosen type that the LHS and RHS are cast
        // to.
        Type* ResultType;

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
            Enum,
            Import,
            NotFound
        } RefKind = RK::NotFound;

        union {
            FuncsList*  Funcs;
            VarDecl*    Var;
            StructDecl* Struct;
            EnumDecl*   Enum;
            Namespace*  NSpace;
        };
    };

    // Ex.  'obj.field'
    struct FieldAccessor : IdentRef {
        FieldAccessor() : IdentRef(AstKind::FIELD_ACCESSOR) {}

        // Request for the length of an array.
        bool IsArrayLength = false;
        bool IsSliceLength = false;
        // Something like Day.MONDAY
        const EnumDecl::EnumValue* EnumValue = nullptr;

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

    // This structure represents when calling a function
    // or initializing a struct by including explicitly
    // which parameter/field the value is to be associated
    // with by the name of the parameter/field.
    struct NamedValue {
        SourceLoc  ExpandedLoc;
        SourceLoc  NameLoc;
        Identifier Name;
        Expr*      AssignValue;
        VarDecl*   VarRef = nullptr;
    };

    struct FuncCall : Expr {
        FuncCall() : Expr(AstKind::FUNC_CALL) {}
    
        llvm::SmallVector<NonNamedValue> Args;
        llvm::SmallVector<NamedValue>    NamedArgs;

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
        bool VarArgsPassAlong = false;

        llvm::SmallVector<NonNamedValue> Args;
        llvm::SmallVector<NamedValue>    NamedArgs;

    };

    // Ex.  'new int'
    struct HeapAlloc : Expr {
        HeapAlloc() : Expr(AstKind::HEAP_ALLOC) {}

        Type* TypeToAlloc;

        FuncDecl* CalledConstructor = nullptr;
        bool VarArgsPassAlong = false;

        llvm::SmallVector<NonNamedValue> Values;
        llvm::SmallVector<NamedValue>    NamedValues;

    };

    // Ex.  'sizeof(int)'
    struct SizeOf : Expr {
        SizeOf() : Expr(AstKind::SIZEOF) {}

        Type* TypeToGetSizeOf;

    };

    // Ex.  'typeof(int)'
    struct TypeOf : Expr {
        TypeOf() : Expr(AstKind::TYPEOF) {}

        Type* TypeToGetTypeOf;
    };

    // TODO: Should this just be a binary operator?
    // Ex.  '0..5'
    struct Range : Expr {
        Range() : Expr(AstKind::RANGE) {}

        u16 Op;

        Expr* LHS;
        Expr* RHS;

    };
}

#endif // ARCO_AST_H