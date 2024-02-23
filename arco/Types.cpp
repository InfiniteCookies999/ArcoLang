#include "Types.h"

#include <llvm/IR/Module.h>

#include "Context.h"

arco::TypeKind arco::Type::GetKind() const {
    if (Kind == TypeKind::Enum) {
        Type* ValuesType = static_cast<const StructType*>(this)->GetEnum()->ValuesType;
        return ValuesType->GetKind();
    }
    return Kind;
}

bool arco::Type::Equals(const Type* Ty) const {
    return UniqueId == Ty->UniqueId;
}

bool arco::Type::IsNumber() const {
    switch (GetKind()) {
    case TypeKind::Int8:
    case TypeKind::Int16:
    case TypeKind::Int32:
    case TypeKind::Int64:
    case TypeKind::UInt8:
    case TypeKind::UInt16:
    case TypeKind::UInt32:
    case TypeKind::UInt64:
    case TypeKind::Int:
    case TypeKind::UInt:
    case TypeKind::Char:
    case TypeKind::Float32:
    case TypeKind::Float64:
        return true;
    default:
        return false;
    }
}

bool arco::Type::IsInt() const {
    switch (GetKind()) {
    case TypeKind::Int8:
    case TypeKind::Int16:
    case TypeKind::Int32:
    case TypeKind::Int64:
    case TypeKind::UInt8:
    case TypeKind::UInt16:
    case TypeKind::UInt32:
    case TypeKind::UInt64:
    case TypeKind::Int:
    case TypeKind::UInt:
    case TypeKind::Char:
        return true;
    default:
        return false;
    }
}

bool arco::Type::IsFloat() const {
    TypeKind K = GetKind();
    return K == TypeKind::Float32 || K == TypeKind::Float64;
}

bool arco::Type::IsSigned() const {
    switch (GetKind()) {
    case TypeKind::Int8:
    case TypeKind::Int16:
    case TypeKind::Int32:
    case TypeKind::Int64:
    case TypeKind::Int:
    case TypeKind::Char:
    case TypeKind::Float32:
    case TypeKind::Float64:
        return true;
    default:
        return false;
    }
}

bool arco::Type::IsSystemInt() const {
    TypeKind Kind = GetKind();
    return Kind == TypeKind::Int || Kind == TypeKind::UInt;
}

bool arco::Type::IsPointer() const {
    TypeKind Kind = GetKind();
    return Kind == TypeKind::Pointer || Kind == TypeKind::CStr ||
           Kind == TypeKind::Null    || Kind == TypeKind::Function;
}

arco::ContainerType* arco::Type::AsContainerType() {
    assert((GetKind() == TypeKind::Pointer || GetKind() == TypeKind::Array || GetKind() == TypeKind::Slice) && "Not a container type");
    return static_cast<ContainerType*>(Unbox());
}

arco::PointerType* arco::Type::AsPointerTy() {
    assert(GetKind() == TypeKind::Pointer && "Not a pointer type");
    return static_cast<PointerType*>(Unbox());
}

arco::SliceType* arco::Type::AsSliceTy() {
    assert(GetKind() == TypeKind::Slice && "Not a slice type");
    return static_cast<SliceType*>(Unbox());
}

arco::ArrayType* arco::Type::AsArrayTy() {
    assert(GetKind() == TypeKind::Array && "Not an array type");
    return static_cast<ArrayType*>(Unbox());
}

arco::StructType* arco::Type::AsStructType() {
    assert((GetKind() == TypeKind::Struct || GetKind() == TypeKind::Interface) && "Not a struct type");
    return static_cast<StructType*>(Unbox());
}

arco::FunctionType* arco::Type::AsFunctionType() {
    assert(GetKind() == TypeKind::Function && "Not a function type");
    return static_cast<FunctionType*>(Unbox());
}

arco::Type* arco::Type::Unbox() {
    if (Kind == TypeKind::Enum) {
        // Enums are alias for their value types.
        StructType* StructTy = static_cast<StructType*>(this);
        return StructTy->GetEnum()->ValuesType;
    }
    return this;
}

const arco::ContainerType* arco::Type::AsContainerType() const {
    assert((GetKind() == TypeKind::Pointer || GetKind() == TypeKind::Array) && "Not a container type");
    return static_cast<const ContainerType*>(Unbox());
}

const arco::PointerType* arco::Type::AsPointerTy() const {
    assert(GetKind() == TypeKind::Pointer && "Not a pointer type");
    return static_cast<const PointerType*>(Unbox());
}

const arco::SliceType* arco::Type::AsSliceTy() const {
    assert(GetKind() == TypeKind::Slice && "Not a slice type");
    return static_cast<const SliceType*>(Unbox());
}

const arco::ArrayType* arco::Type::AsArrayTy() const {
    assert(GetKind() == TypeKind::Array && "Not an array type");
    return static_cast<const ArrayType*>(Unbox());
}

const arco::StructType* arco::Type::AsStructType() const {
    assert((GetKind() == TypeKind::Struct || GetKind() == TypeKind::Interface) && "Not a struct type");
    return static_cast<const StructType*>(Unbox());
}

const arco::FunctionType* arco::Type::AsFunctionType() const {
    assert(GetKind() == TypeKind::Function && "Not a function type");
    return static_cast<const FunctionType*>(Unbox());
}

const arco::Type* arco::Type::Unbox() const {
    if (Kind == TypeKind::Enum) {
        // Enums are alias for their value types.
        const StructType* StructTy = static_cast<const StructType*>(this);
        return StructTy->GetEnum()->ValuesType;
    }
    return this;
}

bool arco::Type::TypeNeedsDestruction() const {
    if (GetKind() == TypeKind::Struct) {
        const StructDecl* Struct = static_cast<const StructType*>(this)->GetStruct();
        return Struct->NeedsDestruction;
    } else if (GetKind() == TypeKind::Array) {
        const ArrayType* ArrayTy = static_cast<const ArrayType*>(this);
        const Type*      BaseTy  = ArrayTy->GetBaseType();

        if (BaseTy->GetKind() == TypeKind::Struct) {
            const StructDecl* Struct = static_cast<const StructType*>(BaseTy)->GetStruct();
            return Struct->NeedsDestruction;
        }
    }
    return false;
}

arco::Type* arco::Type::GetPointerElementType(ArcoContext& Context) const {
    if (GetKind() == TypeKind::CStr) {
        return Context.CharType;
    }
    return AsContainerType()->GetElementType();
}

ulen arco::Type::GetTrivialTypeSizeInBytes() const {
    switch (GetKind()) {
    case TypeKind::Int8:
    case TypeKind::UInt8:
    case TypeKind::Char:
        return 1;
    case TypeKind::Int16:
    case TypeKind::UInt16:
        return 2;
    case TypeKind::Int32:
    case TypeKind::UInt32:
    case TypeKind::Float32:
        return 4;
    case TypeKind::Int64:
    case TypeKind::UInt64:
    case TypeKind::Float64:
        return 8;
    default:
        assert(!"unreachable!");
        return 0;
    }
}

ulen arco::Type::GetSizeInBytes(llvm::Module& LLModule) const {
    switch (GetKind()) {
    case TypeKind::Int8:
    case TypeKind::UInt8:
    case TypeKind::Char:
        return 1;
    case TypeKind::Int16:
    case TypeKind::UInt16:
        return 2;
    case TypeKind::Int32:
    case TypeKind::UInt32:
    case TypeKind::Float32:
        return 4;
    case TypeKind::Int64:
    case TypeKind::UInt64:
    case TypeKind::Float64:
        return 8;
    case TypeKind::Int:
    case TypeKind::UInt:
        return LLModule.getDataLayout().getPointerSize();
    default:
        assert(!"unreachable!");
        return 0;
    }
}

arco::Type* arco::Type::GetIntTypeBasedOnByteSize(ulen Size, bool Signed, ArcoContext& Context) {
    switch (Size) {
    case 1: return Signed ? Context.Int8Type  : Context.UInt8Type;
    case 2: return Signed ? Context.Int16Type : Context.UInt16Type;
    case 4: return Signed ? Context.Int32Type : Context.UInt32Type;
    case 8: return Signed ? Context.Int64Type : Context.UInt64Type;
    default:
        assert(!"Invalid memory size");
        return nullptr;
    }
}

arco::Type* arco::Type::GetFloatTypeBasedOnByteSize(ulen Size, ArcoContext& Context) {
    switch (Size) {
    case 4: return Context.Float32Type;
    case 8: return Context.Float64Type;
    default:
        assert(!"Bad memory size");
        return nullptr;
    }
}

std::string arco::Type::ToString() const {
    switch (GetRealKind()) {
    case TypeKind::Int8:		    return "int8";
    case TypeKind::Int16:		    return "int16";
    case TypeKind::Int32:		    return "int32";
    case TypeKind::Int64:		    return "int64";
    case TypeKind::UInt8:           return "uint8";
    case TypeKind::UInt16:          return "uint16";
    case TypeKind::UInt32:          return "uint32";
    case TypeKind::UInt64:          return "uint64";
    case TypeKind::Float32:         return "f32";
    case TypeKind::Float64:         return "f64";
    case TypeKind::Char:            return "char";
    case TypeKind::Int:             return "int";
    case TypeKind::UInt:            return "uint";
    case TypeKind::Void:            return "void";
    case TypeKind::Null:            return "null";
    case TypeKind::CStr:            return "cstr";
    case TypeKind::Import:          return "import";
    case TypeKind::Bool:            return "bool";
    case TypeKind::EmptyArrayElm:   return "";
    case TypeKind::Error:           return "error";
    case TypeKind::FuncRef:         return "fn reference";
    case TypeKind::StructRef:       return "struct reference";
    case TypeKind::EnumRef:         return "enum reference";
    case TypeKind::InterfaceRef:    return "interface reference";
    case TypeKind::Pointer: {
        if (!this->UniqueId) {
            return "error";
        }

        const PointerType* PtrTy = static_cast<const PointerType*>(this);
        return PtrTy->GetElementType()->ToString() + "*";
    }
    case TypeKind::Slice: {
        if (!this->UniqueId) {
            return "error";
        }

        const SliceType* SliceTy = static_cast<const SliceType*>(this);
        return SliceTy->GetElementType()->ToString() + "[*]";
    }
    case TypeKind::Array: {
        if (!this->UniqueId) {
            return "error";
        }

        const ArrayType* ArrayTy = static_cast<const ArrayType*>(this);
        std::string Val = ArrayTy->GetBaseType()->ToString();
        while (true) {
            Val += "[" + std::to_string(ArrayTy->GetLength()) + "]";
            if (ArrayTy->GetElementType()->GetKind() == TypeKind::Array)
                ArrayTy = ArrayTy->GetElementType()->AsArrayTy();
            else return Val;
        }
        return Val;
    }
    case TypeKind::Struct:
    case TypeKind::Enum:
    case TypeKind::Interface: {
        if (!this->UniqueId) {
            return "error";
        }

        const StructType* StructTy = static_cast<const StructType*>(this);
        return StructTy->GetStructName().Text.str();
    }
    case TypeKind::Function: {
        if (!this->UniqueId) {
            return "error";
        }

        const FunctionType* FuncTy = static_cast<const FunctionType*>(this);
        std::string Val = "fn(";
        for (ulen i = 0; i < FuncTy->ParamTypes.size(); i++) {
            Val += FuncTy->ParamTypes[i].ConstMemory ? "const " : "";
            Val += FuncTy->ParamTypes[i].Ty->ToString();
            if (i+1 != FuncTy->ParamTypes.size()) {
                Val += ", ";
            }
        }
        Val += ") ";
        Val += FuncTy->RetTyInfo.ConstMemory ? "const " : "";
        Val += FuncTy->RetTyInfo.Ty->ToString();
        return Val;
    }
    default:
        assert(!"Unhandled ToString() for type");
        return "";
    }
}

arco::Type* arco::ContainerType::GetBaseType() const {
    if (ElmTy->GetKind() == GetKind())
        return ElmTy->AsContainerType()->GetBaseType();
    return ElmTy;
}

ulen arco::ContainerType::GetDepthLevel() const {
    if (ElmTy->GetKind() == GetKind())
        return ElmTy->AsContainerType()->GetDepthLevel() + 1;
    return 1;
}

arco::PointerType* arco::PointerType::Create(Type* ElmTy, ArcoContext& Context) {
    // If the element type does not have a unique id then the unique id
    // is resolved in FixupType for pointers.
    u32 UniqueId = ElmTy->GetUniqueId();
    if (UniqueId != 0) {
        // The element type has a unique key already so we can save memory
        // and use the cache.
        auto Itr = Context.PointerTyCache.find(UniqueId);
        if (Itr != Context.PointerTyCache.end()) {
            return Itr->second;
        }
    }
    PointerType* Ty = new PointerType;
    Ty->ElmTy = ElmTy;
    if (UniqueId != 0) {
        Ty->UniqueId = Context.UniqueTypeIdCounter++;
        Context.PointerTyCache.insert({ UniqueId, Ty });
    }
    return Ty;
}

arco::SliceType* arco::SliceType::Create(Type* ElmTy, ArcoContext& Context) {
    // If the element type does not have a unique id then the unique id
    // is resolved in FixupType for slices.
    u32 UniqueId = ElmTy->GetUniqueId();
    if (UniqueId != 0) {
        // The element type has a unique key already so we can save memory
        // and use the cache.
        auto Itr = Context.SliceTyCache.find(UniqueId);
        if (Itr != Context.SliceTyCache.end()) {
            return Itr->second;
        }
    }
    SliceType* Ty = new SliceType;
    Ty->ElmTy = ElmTy;
    if (UniqueId != 0) {
        Ty->UniqueId = Context.UniqueTypeIdCounter++;
        Context.SliceTyCache.insert({ UniqueId, Ty });
    }
    return Ty;
}

arco::ArrayType* arco::ArrayType::Create(Type* ElmTy, ulen Length, ArcoContext& Context) {
    // The element type has a unique ID at this pointer so we can rely on the cache.
    std::pair<u32, ulen> UniqueKey = { ElmTy->GetUniqueId(), Length };
    auto Itr = Context.ArrayTyCache.find(UniqueKey);
    if (Itr != Context.ArrayTyCache.end()) {
        // an exact version of this array type already exists.
        return Itr->second;
    } else {
        ArrayType* Ty = new ArrayType;
        Ty->UniqueId = Context.UniqueTypeIdCounter++;
        Context.ArrayTyCache.insert({ UniqueKey, Ty });
        Ty->ElmTy  = ElmTy;
        Ty->Length = Length;
        return Ty;
    }
}

arco::ArrayType* arco::ArrayType::Create(Type* ElmTy,
                                         Expr* LengthExpr,
                                         SourceLoc LengthExprErrorLoc,
                                         ArcoContext& Context) {
    ArrayType* Ty = new ArrayType;
    Ty->ElmTy              = ElmTy;
    Ty->LengthExprErrorLoc = LengthExprErrorLoc;
    Ty->LengthExpr         = LengthExpr;
    return Ty;
}

ulen arco::ArrayType::GetTotalLinearLength() const {
    if (ElmTy->GetKind() == TypeKind::Array)
        return Length * static_cast<ArrayType*>(ElmTy)->GetTotalLinearLength();
    return Length;
}

arco::StructType* arco::StructType::Create(Identifier StructName, SourceLoc ErrorLoc, ArcoContext& Context) {
    // TODO: Caching?
    StructType* Ty = new StructType(TypeKind::Struct); // TODO: Should this default to an indetermined type?
    Ty->StructName = StructName;
    Ty->ErrorLoc   = ErrorLoc;
    return Ty;
}

arco::StructType* arco::StructType::Create(EnumDecl* Enum, ArcoContext& Context) {
    // TODO: Caching?
    StructType* Ty = new StructType(TypeKind::Enum);
    Ty->UniqueId   = Enum->UniqueTypeId;
    Ty->StructName = Enum->Name;
    Ty->Enum       = Enum;
    return Ty;
}

arco::StructType* arco::StructType::Create(StructDecl* Struct, ArcoContext& Context) {
    // TODO: Caching?
    StructType* Ty = new StructType(TypeKind::Struct);
    Ty->UniqueId   = Struct->UniqueTypeId;
    Ty->StructName = Struct->Name;
    Ty->Struct     = Struct;
    return Ty;
}

arco::FunctionType* arco::FunctionType::Create(TypeInfo RetTy, llvm::SmallVector<TypeInfo> ParamTypes, ArcoContext& Context) {
    bool AllUnique = true;
    for (const TypeInfo& ParamType : ParamTypes) {
        if (!ParamType.Ty->GetUniqueId()) {
            AllUnique = false;
        }
    }
    if (!RetTy.Ty->GetUniqueId()) {
        AllUnique = false;
    }
    llvm::SmallVector<u32> UniqueKey = GetUniqueHashKey(RetTy, ParamTypes);
    if (AllUnique) {
        auto Itr = Context.FunctionTyCache.find(UniqueKey);
        if (Itr != Context.FunctionTyCache.end()) {
            return Itr->second;
        }
    }

    FunctionType* FuncTy = new FunctionType;
    if (AllUnique) {
        FuncTy->UniqueId = Context.UniqueTypeIdCounter++;
        Context.FunctionTyCache.insert({ UniqueKey, FuncTy });
    }
    FuncTy->RetTyInfo  = RetTy;
    FuncTy->ParamTypes = std::move(ParamTypes);
    return FuncTy;
}

llvm::SmallVector<u32> arco::FunctionType::GetUniqueHashKey(TypeInfo RetTy, const llvm::SmallVector<TypeInfo>& ParamTypes) {
    // We use the high bit of the u32 bit integer to indicate if the memory is const so that we may
    // use a vector of u32s to hash.
    llvm::SmallVector<u32> UniqueKey;
    UniqueKey.reserve(ParamTypes.size() + 1);
    u32 RetTyKey = RetTy.Ty->GetUniqueId() | (static_cast<u32>(RetTy.ConstMemory) << 31);
    UniqueKey.push_back(RetTyKey);
    for (const TypeInfo& ParamType : ParamTypes) {
        u32 ParamTyKey = ParamType.Ty->GetUniqueId() | (static_cast<u32>(ParamType.ConstMemory) << 31);
        UniqueKey.push_back(ParamTyKey);
    }
    return UniqueKey;
}
