#include "ExpandedLoc.h"

namespace arco {

#define MIN_MAX(L)   \
Min = L.Text.data(); \
Max = Min + L.Text.size();

#define MIN_MAX_DESC(V)        \
{                              \
const char* Min2, *Max2;       \
GetMinAndMaxLocationNode(      \
    V, Min2, Max2);            \
Min = Min2 < Min ? Min2 : Min; \
Max = Max2 > Max ? Max2 : Max; \
}

void GetMinAndMaxLocationNode(Expr* Node, const char*& Min, const char*& Max);

void GetMinAndMaxLocation(BinaryOp* BinOp, const char*& Min, const char*& Max) {
    MIN_MAX(BinOp->Loc);
    MIN_MAX_DESC(BinOp->LHS);
    MIN_MAX_DESC(BinOp->RHS);
}

void GetMinAndMaxLocation(UnaryOp* UniOp, const char*& Min, const char*& Max) {
    MIN_MAX(UniOp->Loc);
    MIN_MAX_DESC(UniOp->Value);
}

void GetMinAndMaxLocation(IdentRef* IRef, const char*& Min, const char*& Max) {
    MIN_MAX(IRef->Loc);
}

void GetMinAndMaxLocation(FieldAccessor* FieldAcc, const char*& Min, const char*& Max) {
    MIN_MAX(FieldAcc->Loc);
    MIN_MAX_DESC(FieldAcc->Site);
}

void GetMinAndMaxLocation(ThisRef* This, const char*& Min, const char*& Max) {
    MIN_MAX(This->Loc);
}

void GetMinAndMaxLocation(FuncCall* Call, const char*& Min, const char*& Max) {
    // TODO
    MIN_MAX(Call->Loc);
}

void GetMinAndMaxLocation(Array* Arr, const char*& Min, const char*& Max) {
    // TODO
    MIN_MAX(Arr->Loc);
}

void GetMinAndMaxLocation(TypeCast* Cast, const char*& Min, const char*& Max) {
    // TODO
    MIN_MAX(Cast->Loc);
}

void GetMinAndMaxLocation(TypeBitCast* Cast, const char*& Min, const char*& Max) {
    // TODO
    MIN_MAX(Cast->Loc);
}

void GetMinAndMaxLocation(StructInitializer* StructInit, const char*& Min, const char*& Max) {
    // TODO
    MIN_MAX(StructInit->Loc);
}

void GetMinAndMaxLocation(HeapAlloc* Alloc, const char*& Min, const char*& Max) {
    // TODO
    MIN_MAX(Alloc->Loc);
}

void GetMinAndMaxLocation(SizeOf* SOf, const char*& Min, const char*& Max) {
    // TODO
    MIN_MAX(SOf->Loc);
}

void GetMinAndMaxLocation(TypeOf* TOf, const char*& Min, const char*& Max) {
    // TODO
    MIN_MAX(TOf->Loc);
}

void GetMinAndMaxLocation(TypeId* TId, const char*& Min, const char*& Max) {
    // TODO
    MIN_MAX(TId->Loc);
}

void GetMinAndMaxLocation(ArrayAccess* Access, const char*& Min, const char*& Max) {
    // TODO
    MIN_MAX(Access->Loc);
}

void GetMinAndMaxLocation(MoveObj* Move, const char*& Min, const char*& Max) {
    // TODO
    MIN_MAX(Move->Loc);
}

void GetMinAndMaxLocation(Ternary* Tern, const char*& Min, const char*& Max) {
    MIN_MAX(Tern->Loc);
    MIN_MAX_DESC(Tern->Cond);
    MIN_MAX_DESC(Tern->RHS);
}

void GetMinAndMaxLocation(TryError* Try, const char*& Min, const char*& Max) {
    MIN_MAX(Try->Loc);
    MIN_MAX_DESC(Try->Value);
}

void GetMinAndMaxLocation(CatchError* Catch, const char*& Min, const char*& Max) {
    MIN_MAX(Catch->Loc);
    MIN_MAX_DESC(Catch->CaughtExpr);
}

void GetMinAndMaxLocationNode(Expr* Node, const char*& Min, const char*& Max) {
    switch (Node->Kind) {
    case AstKind::BINARY_OP:
        GetMinAndMaxLocation(static_cast<BinaryOp*>(Node), Min, Max);
        break;
    case AstKind::UNARY_OP:
        GetMinAndMaxLocation(static_cast<UnaryOp*>(Node), Min, Max);
        break;
    case AstKind::IDENT_REF:
        GetMinAndMaxLocation(static_cast<IdentRef*>(Node), Min, Max);
        break;
    case AstKind::FIELD_ACCESSOR:
        GetMinAndMaxLocation(static_cast<FieldAccessor*>(Node), Min, Max);
        break;
    case AstKind::THIS_REF:
        GetMinAndMaxLocation(static_cast<ThisRef*>(Node), Min, Max);
        break;
    case AstKind::FUNC_CALL:
        GetMinAndMaxLocation(static_cast<FuncCall*>(Node), Min, Max);
        break;
    case AstKind::ARRAY:
        GetMinAndMaxLocation(static_cast<Array*>(Node), Min, Max);
        break;
    case AstKind::ARRAY_ACCESS:
        GetMinAndMaxLocation(static_cast<ArrayAccess*>(Node), Min, Max);
        break;
    case AstKind::TYPE_CAST:
        GetMinAndMaxLocation(static_cast<TypeCast*>(Node), Min, Max);
        break;
    case AstKind::TYPE_BITCAST:
        GetMinAndMaxLocation(static_cast<TypeBitCast*>(Node), Min, Max);
        break;
    case AstKind::STRUCT_INITIALIZER:
        GetMinAndMaxLocation(static_cast<StructInitializer*>(Node), Min, Max);
        break;
    case AstKind::HEAP_ALLOC:
        GetMinAndMaxLocation(static_cast<HeapAlloc*>(Node), Min, Max);
        break;
    case AstKind::SIZEOF:
        GetMinAndMaxLocation(static_cast<SizeOf*>(Node), Min, Max);
        break;
    case AstKind::TYPEOF:
        GetMinAndMaxLocation(static_cast<TypeOf*>(Node), Min, Max);
        break;
    case AstKind::TYPEID:
        GetMinAndMaxLocation(static_cast<TypeId*>(Node), Min, Max);
        break;
    case AstKind::MOVEOBJ:
        GetMinAndMaxLocation(static_cast<MoveObj*>(Node), Min, Max);
        break;
    case AstKind::TERNARY:
        GetMinAndMaxLocation(static_cast<Ternary*>(Node), Min, Max);
        break;
    case AstKind::TRY_ERROR:
        GetMinAndMaxLocation(static_cast<TryError*>(Node), Min, Max);
        break;
    case AstKind::CATCH_ERROR:
        GetMinAndMaxLocation(static_cast<CatchError*>(Node), Min, Max);
        break;
    case AstKind::NUMBER_LITERAL:
    case AstKind::STRING_LITERAL:
    case AstKind::NULLPTR:
    case AstKind::BOOL_LITERAL: {
        MIN_MAX(Node->Loc);
        break;
    }
    default:
        assert(!"Failed to implement CheckNode case!");
        break;
    }
}

}

arco::SourceLoc arco::GetExpandedLoc(Expr* Node) {
    const char* Min = nullptr, *Max = nullptr;
    GetMinAndMaxLocationNode(Node, Min, Max);
    SourceLoc ExpandedLoc;
    ExpandedLoc.Text = llvm::StringRef(
        Min,
        Max - Min
    );
    ExpandedLoc.LineNumber = Node->Loc.LineNumber; // TODO: This should be the minimum line number!
    return ExpandedLoc;
}
