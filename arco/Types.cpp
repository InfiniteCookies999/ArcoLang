#include "Types.h"

#include <llvm/IR/Module.h>

#include "Context.h"

bool arco::Type::Equals(Type* Ty) const {
	switch (Ty->GetKind()) {
	case TypeKind::Array: {
		if (this->GetKind() != TypeKind::Array) {
			return false;
		}
		
		const ArrayType* ArrayTy    = static_cast<const ArrayType*>(Ty);
		const ArrayType* ThisArrayTy = static_cast<const ArrayType*>(this);
		
		return ArrayTy->GetLength() == ThisArrayTy->GetLength() &&
			   ArrayTy->GetElementType()->Equals(ThisArrayTy->GetElementType());
	}
	case TypeKind::Struct: {
		if (this->GetKind() != TypeKind::Struct) {
			return false;
		}
		
		const StructType* StructTy     = static_cast<const StructType*>(Ty);
		const StructType* ThisStructTy = static_cast<const StructType*>(Ty);

		return StructTy->GetStruct() == ThisStructTy->GetStruct();
	}
	default:
		return this == Ty;
	}
}

bool arco::Type::IsNumber() const {
	switch (GetKind()) {
	case TypeKind::Int8:
	case TypeKind::Int16:
	case TypeKind::Int32:
	case TypeKind::Int64:
	case TypeKind::UnsignedInt8:
	case TypeKind::UnsignedInt16:
	case TypeKind::UnsignedInt32:
	case TypeKind::UnsignedInt64:
	case TypeKind::Int:
	case TypeKind::UnsignedInt:
	case TypeKind::Char:
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
	case TypeKind::UnsignedInt8:
	case TypeKind::UnsignedInt16:
	case TypeKind::UnsignedInt32:
	case TypeKind::UnsignedInt64:
	case TypeKind::Int:
	case TypeKind::UnsignedInt:
	case TypeKind::Char:
		return true;
	default:
		return false;
	}
}

bool arco::Type::IsSigned() const {
	switch (GetKind()) {
	case TypeKind::Int8:
	case TypeKind::Int16:
	case TypeKind::Int32:
	case TypeKind::Int64:
	case TypeKind::Int:
	case TypeKind::Char:
		return true;
	default:
		return false;
	}
}

bool arco::Type::IsSystemInt() const {
	TypeKind Kind = GetKind();
	return Kind == TypeKind::Int || Kind == TypeKind::UnsignedInt;
}

bool arco::Type::IsPointer() const {
	TypeKind Kind = GetKind();
	return Kind == TypeKind::Pointer || Kind == TypeKind::CStr;
}

arco::Type* arco::Type::GetPointerElementType(ArcoContext& Context) const {
	if (GetKind() == TypeKind::CStr) {
		return Context.CharType;
	}
	return static_cast<const PointerType*>(this)->GetElementType();
}

ulen arco::Type::GetTrivialTypeSizeInBytes() const {
	switch (GetKind()) {
	case TypeKind::Int8:
	case TypeKind::UnsignedInt8:
	case TypeKind::Char:
		return 1;
	case TypeKind::Int16:
	case TypeKind::UnsignedInt16:
		return 2;
	case TypeKind::Int32:
	case TypeKind::UnsignedInt32:
		return 4;
	case TypeKind::Int64:
	case TypeKind::UnsignedInt64:
		return 8;
	default:
		assert(!"unreachable!");
		return 0;
	}
}

ulen arco::Type::GetSizeInBytes(llvm::Module& LLModule) const {
	switch (GetKind()) {
	case TypeKind::Int8:
	case TypeKind::UnsignedInt8:
	case TypeKind::Char:
		return 1;
	case TypeKind::Int16:
	case TypeKind::UnsignedInt16:
		return 2;
	case TypeKind::Int32:
	case TypeKind::UnsignedInt32:
		return 4;
	case TypeKind::Int64:
	case TypeKind::UnsignedInt64:
		return 8;
	case TypeKind::Int:
	case TypeKind::UnsignedInt:
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

std::string arco::Type::ToString() const {
	switch (GetKind()) {
	case TypeKind::Int8:		    return "int8";
	case TypeKind::Int16:		    return "int16";
	case TypeKind::Int32:		    return "int32";
	case TypeKind::Int64:		    return "int64";
	case TypeKind::UnsignedInt8:    return "uint8";
	case TypeKind::UnsignedInt16:   return "uint16";
	case TypeKind::UnsignedInt32:   return "uint32";
	case TypeKind::UnsignedInt64:   return "uint64";
	case TypeKind::Char:            return "char";
	case TypeKind::Int:             return "int";
	case TypeKind::UnsignedInt:     return "uint";
	case TypeKind::Void:            return "void";
	case TypeKind::Null:            return "null";
	case TypeKind::CStr:            return "cstr";
	case TypeKind::Import:          return "import";
	case TypeKind::EmptyArrayElm:   return "";
	case TypeKind::Pointer: {
		const PointerType* PtrTy = static_cast<const PointerType*>(this);
		return PtrTy->GetElementType()->ToString() + "*";
	}
	case TypeKind::Array: {
		const ArrayType* ArrayTy = static_cast<const ArrayType*>(this);
		std::string Val = ArrayTy->GetBaseType()->ToString();
		while (true) {
			Val += "[" + std::to_string(ArrayTy->GetLength()) + "]";
			if (ArrayTy->GetElementType()->GetKind() == TypeKind::Array)
				ArrayTy = static_cast<const ArrayType*>(ArrayTy->GetElementType());
			else return Val;
		}
		return Val;
	}
	case TypeKind::Struct: {
		const StructType* StructTy = static_cast<const StructType*>(this);
		return StructTy->GetStruct()->Name.Text.str();
	}
	default:
		assert(!"Unhandled ToString() for type");
		return "";
	}
}

arco::Type* arco::ContainerType::GetBaseType() const {
	if (ElmTy->GetKind() == GetKind())
		return static_cast<const ContainerType*>(ElmTy)->GetBaseType();
	return ElmTy;
}

ulen arco::ContainerType::GetDepthLevel() const {
	if (ElmTy->GetKind() == GetKind())
		return static_cast<ContainerType*>(ElmTy)->GetDepthLevel() + 1;
	return 1;
}

arco::PointerType* arco::PointerType::Create(Type* ElmTy, ArcoContext& Context) {
	auto Itr = Context.PointerCache.find(ElmTy);
	if (Itr != Context.PointerCache.end()) {
		return Itr->second;
	}
	PointerType* Ty = new PointerType;
	Ty->ElmTy = ElmTy;
	Context.PointerCache.insert({ ElmTy, Ty });
	return Ty;
}

arco::ArrayType* arco::ArrayType::Create(Type* ElmTy, ulen Length, ArcoContext& Context) {
	auto Itr = Context.ArrayCache.find({ ElmTy, Length });
	if (Itr != Context.ArrayCache.end()) {
		return Itr->second;
	}
	ArrayType* Ty = new ArrayType;
	Ty->ElmTy  = ElmTy;
	Ty->Length = Length;
	Context.ArrayCache.insert({ { ElmTy, Length }, Ty });
	return Ty;
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
	StructType* Ty = new StructType;
	Ty->StructName = StructName;
	Ty->ErrorLoc   = ErrorLoc;
	return Ty;
}

arco::StructType* arco::StructType::Create(StructDecl* Struct, ArcoContext& Context) {
	// TODO: Caching?
	StructType* Ty = new StructType;
	Ty->StructName = Struct->Name;
	Ty->Struct     = Struct;
	return Ty;
}
