#include "Types.h"

#include <llvm/IR/Module.h>

#include "Context.h"

arco::TypeKind arco::Type::GetKind() const {
    return Unbox()->GetRealKind();
}

bool arco::Type::TypeHasStorage(bool FromPtr) const {
    switch (GetRealKind()) {
    case TypeKind::Void:
        return FromPtr;
    case TypeKind::Null:
    case TypeKind::Import:
    case TypeKind::FuncRef:
    case TypeKind::StructRef:
    case TypeKind::EnumRef:
    case TypeKind::InterfaceRef:
    case TypeKind::EmptyArrayElm:
        return false;
    case TypeKind::Pointer:
    case TypeKind::Slice:
    case TypeKind::Array: {
        const ContainerType* ContainerTy = static_cast<const ContainerType*>(this);
        Type* ElmTy = nullptr;
        while (true) {
            ElmTy = ContainerTy->GetElementType();
            if (ElmTy->GetRealKind() == TypeKind::Pointer ||
                ElmTy->GetRealKind() == TypeKind::Slice ||
                ElmTy->GetRealKind() == TypeKind::Array) {
                ContainerTy = static_cast<const ContainerType*>(this);
            } else {
                break;
            }
        }
        return ElmTy->TypeHasStorage(GetRealKind() == TypeKind::Pointer);
    }
    default:
        return true;
    }
}

bool arco::Type::Equals(const Type* Ty) const {
    
    if (Ty->ContainsGenerics || this->ContainsGenerics) {
        return GenericsEquals(Ty);
    }

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
    case TypeKind::Ptrsize:
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
    case TypeKind::Ptrsize:
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
    return Kind == TypeKind::Int || Kind == TypeKind::Ptrsize;
}

bool arco::Type::IsPointer() const {
    TypeKind Kind = GetKind();
    return Kind == TypeKind::Pointer || Kind == TypeKind::CStr ||
           Kind == TypeKind::Null    || Kind == TypeKind::Function;
}

bool arco::Type::IsQualifiedPointer() const {
    TypeKind Kind = GetKind();
    return Kind == TypeKind::Pointer || Kind == TypeKind::CStr ||
           Kind == TypeKind::Function;
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
    } else if (Kind == TypeKind::Generic) {
        // Still have to unbind because of enums.
        GenericType* GenTy = static_cast<GenericType*>(this);
        return GenTy->GetBoundTy()->Unbox();
    }
    return this;
}

arco::Type* arco::Type::UnboxGeneric() {
    if (Kind == TypeKind::Generic) {
        GenericType* GenTy = static_cast<GenericType*>(this);
        return GenTy->GetBoundTy();
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
    } else if (Kind == TypeKind::Generic) {
        // Still have to unbox the type again in case it is an enum.
        const GenericType* GenTy = static_cast<const GenericType*>(this);
        return GenTy->GetBoundTy()->Unbox();
    }
    return this;
}

const arco::Type* arco::Type::UnboxGeneric() const {
    if (Kind == TypeKind::Generic) {
        const GenericType* GenTy = static_cast<const GenericType*>(this);
        return GenTy->GetBoundTy();
    }
    return this;
}

bool arco::Type::TypeNeedsDestruction() const {
    if (GetKind() == TypeKind::Struct) {
        const StructDecl* Struct = AsStructType()->GetStruct();
        return Struct->NeedsDestruction;
    } else if (GetKind() == TypeKind::Array) {
        const ArrayType* ArrayTy = static_cast<const ArrayType*>(this);
        const Type*      BaseTy  = ArrayTy->GetBaseType();

        if (BaseTy->GetKind() == TypeKind::Struct) {
            const StructDecl* Struct = BaseTy->AsStructType()->GetStruct();
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
    case TypeKind::Ptrsize:
    case TypeKind::Pointer:
    case TypeKind::Null:
    case TypeKind::Function:
    case TypeKind::CStr:
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

bool arco::Type::HasTypeBound(const llvm::SmallVector<Type*, 8>& PartiallyBoundTys) const {
    switch (GetRealKind()) {
    case TypeKind::Generic: {
        const GenericType* GenTy = static_cast<const GenericType*>(this);
        return PartiallyBoundTys[GenTy->GetIdx()] != nullptr;
    }
    case TypeKind::Pointer:
    case TypeKind::Slice:
    case TypeKind::Array: {
        // NOTE: Cannot use GetBaseType() because it relies on Unboxing but when this
        // function is called the types are not bound.
        const ContainerType* ContainerTy = static_cast<const ContainerType*>(this);
        Type* ElmTy = nullptr;
        while (true) {
            ElmTy = ContainerTy->GetElementType();
            if (ElmTy->GetRealKind() == TypeKind::Pointer ||
                ElmTy->GetRealKind() == TypeKind::Slice ||
                ElmTy->GetRealKind() == TypeKind::Array) {
                ContainerTy = static_cast<const ContainerType*>(this);
            } else {
                break;
            }
        }
        return ElmTy->HasTypeBound(PartiallyBoundTys);
    }
    case TypeKind::Function: {
        const FunctionType* FuncTy = static_cast<const FunctionType*>(this);
        if (FuncTy->RetTyInfo.Ty->HasTypeBound(PartiallyBoundTys)) {
            return true;
        }
        for (const TypeInfo& PInfo : FuncTy->ParamTypes) {
            if (PInfo.Ty->HasTypeBound(PartiallyBoundTys)) {
                return true;
            }
        }
        return false;
    }
    default:
        return false;
    }
}

std::string arco::Type::ToString(bool                               ShowFullGenericTy,
                                 const llvm::SmallVector<Type*, 8>* PartiallyBoundTys,
                                 bool                               ReplaceGenTyWithBoundTy) const {
    if (ContainsGenerics && !ReplaceGenTyWithBoundTy) {
        if (ShowFullGenericTy) {
            std::string S = ToString(false);
            S += "=";
            S += ToString(ShowFullGenericTy, PartiallyBoundTys, true);
            return S;
        } else if (PartiallyBoundTys) {
            if (HasTypeBound(*PartiallyBoundTys)) {
                std::string S = ToString(false);
                S += "=";
                S += ToString(ShowFullGenericTy, PartiallyBoundTys, true);
                return S;
            } else {
                PartiallyBoundTys = nullptr;
            }
        }
    }

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
    case TypeKind::Ptrsize:         return "ptrsize";
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
    case TypeKind::Generic: {
        
        const GenericType* GenTy = static_cast<const GenericType*>(this);
        std::string S = "";
        const Type* BoundTy = nullptr;
        if (ReplaceGenTyWithBoundTy) {
            if (PartiallyBoundTys) {
                BoundTy = (*PartiallyBoundTys)[GenTy->GetIdx()];
            } else {
                BoundTy = GenTy->GetBoundTy();
            }
        }

        if (BoundTy) {
            S += BoundTy->ToString(false);
        } else {
            S += GenTy->GetName().Text.str();
        }
        return S;
    }
    case TypeKind::Pointer: {
        if (!this->UniqueId) {
            return "error";
        }

        const PointerType* PtrTy = static_cast<const PointerType*>(this);
        return PtrTy->GetElementType()->ToString(ShowFullGenericTy, PartiallyBoundTys, ReplaceGenTyWithBoundTy) + "*";
    }
    case TypeKind::Slice: {
        if (!this->UniqueId) {
            return "error";
        }

        const SliceType* SliceTy = static_cast<const SliceType*>(this);
        return SliceTy->GetElementType()->ToString(ShowFullGenericTy, PartiallyBoundTys, ReplaceGenTyWithBoundTy) + "[*]";
    }
    case TypeKind::Array: {
        if (!this->UniqueId) {
            return "error";
        }

        const ArrayType* ArrayTy = static_cast<const ArrayType*>(this);

        Type* BaseTy = ArrayTy->GetElementType();
        while (BaseTy->GetRealKind() == ArrayTy->GetRealKind()) {
            BaseTy = static_cast<ArrayType*>(BaseTy)->GetElementType();
        }

        std::string Val = BaseTy->ToString(ShowFullGenericTy, PartiallyBoundTys, ReplaceGenTyWithBoundTy);
        while (true) {
            Val += "[" + std::to_string(ArrayTy->GetLength()) + "]";
            if (ArrayTy->GetElementType()->GetRealKind() == TypeKind::Array)
                ArrayTy = static_cast<ArrayType*>(ArrayTy->GetElementType());
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
            Val += FuncTy->ParamTypes[i].Ty->ToString(ShowFullGenericTy, PartiallyBoundTys, ReplaceGenTyWithBoundTy);
            if (i+1 != FuncTy->ParamTypes.size()) {
                Val += ", ";
            }
        }
        Val += ") ";
        Val += FuncTy->RetTyInfo.ConstMemory ? "const " : "";
        Val += FuncTy->RetTyInfo.Ty->ToString(ShowFullGenericTy, PartiallyBoundTys, ReplaceGenTyWithBoundTy);
        return Val;
    }
    default:
        assert(!"Unhandled ToString() for type");
        return "";
    }
}

bool arco::Type::GenericsEquals(const Type* Ty) const {
    
    const Type* TyUnboxed   = Ty->UnboxGeneric();
    const Type* ThisUnboxed = this->UnboxGeneric();

    switch (ThisUnboxed->GetRealKind()) {
    case TypeKind::Pointer: {
        if (TyUnboxed->GetRealKind() != TypeKind::Pointer) return false;
        
        const PointerType* PtrTy     = TyUnboxed->AsPointerTy();
        const PointerType* ThisPtrTy = ThisUnboxed->AsPointerTy();
        if (ThisPtrTy->GetDepthLevel() != PtrTy->GetDepthLevel()) {
            return false;
        }

        return ThisPtrTy->GetBaseType()->GenericsEquals(PtrTy->GetBaseType());
    }
    case TypeKind::Slice: {
        if (TyUnboxed->GetRealKind() != TypeKind::Slice) return false;

        const SliceType* SliceTy     = TyUnboxed->AsSliceTy();
        const SliceType* ThisSliceTy = ThisUnboxed->AsSliceTy();
        if (ThisSliceTy->GetDepthLevel() != SliceTy->GetDepthLevel()) {
            return false;
        }

        return ThisSliceTy->GetBaseType()->GenericsEquals(SliceTy->GetBaseType());
    }
    case TypeKind::Array: {
        if (TyUnboxed->GetRealKind() != TypeKind::Array) return false;

        // TODO: Change this to be non-recursive.

        const ArrayType* ArrTy     = TyUnboxed->AsArrayTy();
        const ArrayType* ThisArrTy = ThisUnboxed->AsArrayTy();
       
        if (ThisArrTy->GetLength() != ArrTy->GetLength()) return false;

        return ThisArrTy->GetElementType()->GenericsEquals(ArrTy->GetElementType());
    }
    case TypeKind::Function: {
        if (TyUnboxed->GetRealKind() != TypeKind::Function) return false;

        const FunctionType* FuncTy     = TyUnboxed->AsFunctionType();
        const FunctionType* ThisFuncTy = ThisUnboxed->AsFunctionType();

        if (ThisFuncTy->ParamTypes.size() != FuncTy->ParamTypes.size()) {
            return false;
        }

        if (!ThisFuncTy->RetTyInfo.Ty->GenericsEquals(FuncTy->RetTyInfo.Ty)) {
            return false;
        }
        if (ThisFuncTy->RetTyInfo.ConstMemory != FuncTy->RetTyInfo.ConstMemory) {
            return false;
        }
        for (ulen i = 0; i < ThisFuncTy->ParamTypes.size(); i++) {
            const TypeInfo& ThisPInfo = ThisFuncTy->ParamTypes[i];
            const TypeInfo& PInfo     = FuncTy->ParamTypes[i];
            if (!ThisPInfo.Ty->GenericsEquals(PInfo.Ty)) {
                return false;
            }
            if (ThisPInfo.ConstMemory != PInfo.ConstMemory) {
                return false;
            }
        }

        return true;
    }
    default:
        return ThisUnboxed->Equals(TyUnboxed);
    }
}

arco::Type* arco::ContainerType::GetBaseType() const {
    Type* BaseTy = ElmTy;
    while (BaseTy->GetKind() == this->GetKind()) {
        BaseTy = BaseTy->AsContainerType()->GetElementType();
    }
    return BaseTy;
}

ulen arco::ContainerType::GetDepthLevel() const {
    ulen Depth = 1;
    const Type* ElmTy = this->ElmTy;
    while (ElmTy->GetKind() == this->GetKind()) {
        ++Depth;
        ElmTy = ElmTy->AsContainerType()->GetElementType();
    }
    return Depth;
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
    if (Ty->ElmTy->ContainsGenerics) {
        Ty->ContainsGenerics = true;
    }
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
    if (Ty->ElmTy->ContainsGenerics) {
        Ty->ContainsGenerics = true;
    }
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
        if (Ty->ElmTy->ContainsGenerics) {
            Ty->ContainsGenerics = true;
        }
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
    if (Ty->ElmTy->ContainsGenerics) {
        Ty->ContainsGenerics = true;
    }
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

arco::StructType* arco::StructType::Create(InterfaceDecl* Interface, ArcoContext& Context) {
    // TODO: Caching
    StructType* Ty = new StructType(TypeKind::Interface);
    Ty->UniqueId   = Interface->UniqueTypeId;
    Ty->StructName = Interface->Name;
    Ty->Interface  = Interface;
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
    for (TypeInfo& PInfo : ParamTypes) {
        if (PInfo.Ty->ContainsGenerics) {
            FuncTy->ContainsGenerics = true;
            break;
        }
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

arco::GenericType* arco::GenericType::Create(Identifier Name, ulen Idx, ArcoContext& Context) {
    // NOTE: there is no need to cache since every instance of a created generic
    //       type is known during parsing and only exists within a local scope.
    GenericType* GenTy = new GenericType;
    GenTy->UniqueId = Context.UniqueTypeIdCounter++;
    GenTy->Name = Name;
    GenTy->ContainsGenerics = true;
    GenTy->Idx = Idx;
    return GenTy;
}

namespace llvm {
    raw_ostream& llvm::operator<<(raw_ostream& OS, const arco::Type* Ty) {
        OS << Ty->ToString();
        return OS;
    }
}
