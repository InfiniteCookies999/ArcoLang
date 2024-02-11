#ifndef ARCO_TYPES_H
#define ARCO_TYPES_H

#include <string>
#include <llvm/ADT/SmallVector.h>

#include "Source.h"
#include "Identifier.h"

namespace llvm {
	class Module;
}

namespace arco {

	class ArcoContext;
	struct Expr;
	struct StructDecl;
	struct EnumDecl;
	class ContainerType;
	class PointerType;
	class ArrayType;
	class StructType;
	class FunctionType;

	enum class TypeKind {
		Int,
		UnsignedInt,

		Int8,
		Int16,
		Int32,
		Int64,
		UnsignedInt8,
		UnsignedInt16,
		UnsignedInt32,
		UnsignedInt64,
	
		Float32,
		Float64,

		Char,

		Void,
		Bool,
		CStr,
		Pointer,
		Array,
		Struct,
		Enum,

		Null,
		Error,
		// When an array is declared with no elements.
		EmptyArrayElm,
		// When an identifier is an import the type is set to this.
		Import,
		Function,
		// When referencing an identifier that refers to one or more functions.
		FuncRef,
		// When referencing an identifier that refers to a struct.
		StructRef,
		// When referencing an identifier that refers to an enum.
		EnumRef,

	};

	class Type {
	public:

		Type(TypeKind Kind)
			: Kind(Kind) {}

		Type(TypeKind Kind, u32 UniqueId)
			: Kind(Kind), UniqueId(UniqueId) {}

		TypeKind GetKind() const;
		inline TypeKind GetRealKind() const {
			return Kind;
		}

		bool Equals(const Type* Ty) const;

		bool IsNumber() const;
		bool IsInt() const;
		bool IsFloat() const;
		bool IsSigned() const;
		bool IsSystemInt() const;
		bool IsPointer() const;

		ContainerType* AsContainerType();
		PointerType* AsPointerTy();
		ArrayType* AsArrayTy();
		StructType* AsStructType();
		FunctionType* AsFunctionType();
		
		Type* Unbox();

		const ContainerType* AsContainerType() const;
		const PointerType* AsPointerTy() const;
		const ArrayType* AsArrayTy() const;
		const StructType* AsStructType() const;
		const FunctionType* AsFunctionType() const;
		
		const Type* Unbox() const;

		bool TypeNeedsDestruction() const;

		Type* GetPointerElementType(ArcoContext& Context) const;

		ulen GetTrivialTypeSizeInBytes() const;
		ulen GetSizeInBytes(llvm::Module& LLModule) const;

		static Type* GetIntTypeBasedOnByteSize(ulen Size, bool Signed, ArcoContext& Context);
		static Type* GetFloatTypeBasedOnByteSize(ulen Size, ArcoContext& Context);

		std::string ToString() const;

		inline void SetUniqueId(u32 Id) {
			UniqueId = Id;
		}

		inline u32 GetUniqueId() {
			return UniqueId;
		}

	protected:
		TypeKind Kind;
		u32      UniqueId = 0;
	};

	struct TypeInfo {
		Type* Ty;
		bool  ConstMemory;
	};

	class ContainerType : public Type {
	public:

		Type* GetElementType() const { return ElmTy; }

		/// Traverses the elements recursively until
		/// there is a mismatch in the type of container
		/// and it's element.
		///
		/// For example 'i32***' would return a i32 type.
		Type* GetBaseType() const;

		ulen GetDepthLevel() const;

	protected:

		Type* ElmTy;

		ContainerType(TypeKind Kind)
			: Type(Kind) {}

	};

	class PointerType : public ContainerType {
	public:

		static PointerType* Create(Type* ElmTy, ArcoContext& Context);

	private:
		PointerType()
			: ContainerType(TypeKind::Pointer) {}
	};

	class ArrayType : public ContainerType {
	public:

		static ArrayType* Create(Type* ElmTy, ulen Length, ArcoContext& Context);
		static ArrayType* Create(Type* ElmTy,
			                     Expr* LengthExpr,
			                     SourceLoc LengthExprErrorLoc,
			                     ArcoContext& Context);

		ulen GetLength() const { return Length; }

		Expr* GetLengthExpr() const { return LengthExpr; }

		SourceLoc GetLengthExprErrorLoc() const { return LengthExprErrorLoc; }

		/// This is the product of all the lengths
		/// of the dimensions of the array.
		ulen GetTotalLinearLength() const;

		void AssignLength(ulen Length) {
			this->Length = Length;
		}

	private:
		ArrayType()
			: ContainerType(TypeKind::Array) {}

		SourceLoc LengthExprErrorLoc;
		Expr*     LengthExpr = nullptr;
		ulen      Length;
	};

	class StructType : public Type {
	public:

		static StructType* Create(Identifier StructName, SourceLoc ErrorLoc, ArcoContext& Context);
		static StructType* Create(StructDecl* Struct, ArcoContext& Context);
		static StructType* Create(EnumDecl* Enum, ArcoContext& Context);

		StructDecl* GetStruct() const { return Struct; }
		EnumDecl*   GetEnum() const { return Enum; }
		void AssignStruct(StructDecl* Struct) {
			this->Struct = Struct;
		}
		void AssignEnum(EnumDecl* Enum) {
			this->Enum = Enum;
			Kind = TypeKind::Enum;
		}

		Identifier GetStructName() const { return StructName; }

		SourceLoc GetErrorLoc() const { return ErrorLoc; }

	private:
		StructType(TypeKind Kind)
			: Type(Kind) {}
		
		SourceLoc   ErrorLoc;
		Identifier  StructName;
		union {
			StructDecl* Struct;
			EnumDecl*   Enum;
		};
	};

	class FunctionType : public Type {
	public:
		
		TypeInfo RetTyInfo;
		llvm::SmallVector<TypeInfo> ParamTypes;

		static FunctionType* Create(TypeInfo RetTy, llvm::SmallVector<TypeInfo> ParamTypes, ArcoContext& Context);

		static llvm::SmallVector<u32> GetUniqueHashKey(TypeInfo RetTy, const llvm::SmallVector<TypeInfo>& ParamTypes);

	private:
		FunctionType()
			:Type(TypeKind::Function) {}

	};


}

#endif // ARCO_TYPES_H