#ifndef ARCO_TYPES_H
#define ARCO_TYPES_H

#include <string>

#include "Source.h"
#include "Identifier.h"

namespace llvm {
	class Module;
}

namespace arco {

	class ArcoContext;
	struct Expr;
	struct StructDecl;

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
	
		Char,

		Void,
		Bool,
		CStr,
		Pointer,
		Array,
		Null,
		Error,
		// When an array is declared with no elements.
		EmptyArrayElm,
		Struct,
		// When an identifier is an import the type is set to this.
		Import,

	};

	class Type {
	public:

		Type(TypeKind Kind)
			: Kind(Kind) {}

		inline TypeKind GetKind() const {
			return Kind;
		}

		bool Equals(Type* Ty) const;

		bool IsNumber() const;
		bool IsInt() const;
		bool IsSigned() const;
		bool IsSystemInt() const;
		bool IsPointer() const;

		Type* GetPointerElementType(ArcoContext& Context) const;

		ulen GetTrivialTypeSizeInBytes() const;
		ulen GetSizeInBytes(llvm::Module& LLModule) const;

		static Type* GetIntTypeBasedOnByteSize(ulen Size, bool Signed, ArcoContext& Context);
		//static Type* GetFloatTypeBasedOnByteSize(usize Size, ArcoContext& Context);

		std::string ToString() const;

	private:
		TypeKind Kind;
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

		StructDecl* GetStruct() const { return Struct; }
		void AssignStruct(StructDecl* Struct) {
			this->Struct = Struct;
		}

		Identifier GetStructName() const { return StructName; }

		SourceLoc GetErrorLoc() const { return ErrorLoc; }

	private:
		StructType()
			: Type(TypeKind::Struct) {}
		
		SourceLoc   ErrorLoc;
		Identifier  StructName;
		StructDecl* Struct;
	};

}

#endif // ARCO_TYPES_H