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
    struct InterfaceDecl;
    class ContainerType;
    class ImplPointerType;
    class PointerType;
    class SliceType;
    class ArrayType;
    class StructType;
    class FunctionType;

    enum class TypeKind {

        Ptrsize,
        UInt8,
        UInt16,
        UInt32,
        UInt64,

        Int,
        Int8,
        Int16,
        Int32,
        Int64,

        Char,

        Float32,
        Float64,

        Void,
        Bool,
        CStr,
        Pointer,
        Slice,
        Array,
        Struct,
        Enum,
        Interface,
        Function,
        Generic,

        Null,
        Error,
        // When an array is declared with no elements.
        EmptyArrayElm,
        // When an identifier is an import the type is set to this.
        Import,
        // When referencing an identifier that refers to one or more functions.
        FuncRef,
        // When referencing an identifier that refers to a struct.
        StructRef,
        // When referencing an identifier that refers to an enum.
        EnumRef,
        // When referencing an identifier that refers to an interface.
        InterfaceRef,

    };

    class Type {
    public:
        bool ContainsGenerics = false;
        
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
        bool IsQualifiedPointer() const;

        ContainerType* AsContainerType();
        PointerType* AsPointerTy();
        SliceType* AsSliceTy();
        ArrayType* AsArrayTy();
        StructType* AsStructType();
        FunctionType* AsFunctionType();
        
        Type* Unbox();
        Type* UnboxGeneric();

        const ContainerType* AsContainerType() const;
        const PointerType* AsPointerTy() const;
        const SliceType* AsSliceTy() const;
        const ArrayType* AsArrayTy() const;
        const StructType* AsStructType() const;
        const FunctionType* AsFunctionType() const;
        
        const Type* Unbox() const;
        const Type* UnboxGeneric() const;

        bool TypeNeedsDestruction() const;

        Type* GetPointerElementType(ArcoContext& Context) const;

        ulen GetTrivialTypeSizeInBytes() const;
        ulen GetSizeInBytes(llvm::Module& LLModule) const;

        static Type* GetIntTypeBasedOnByteSize(ulen Size, bool Signed, ArcoContext& Context);
        static Type* GetFloatTypeBasedOnByteSize(ulen Size, ArcoContext& Context);

        std::string ToString(bool                               ShowFullGenericTy = true,
                             const llvm::SmallVector<Type*, 8>* PartiallyBoundTys = nullptr,
                             bool                               ReplaceGenTyWithBoundTy = false) const;

        inline void SetUniqueId(u32 Id) {
            UniqueId = Id;
        }

        inline u32 GetUniqueId() {
            return UniqueId;
        }

    private:
        bool GenericsEquals(const Type* Ty) const;

        bool HasTypeBound(const llvm::SmallVector<Type*, 8>& PartiallyBoundTys) const;

    protected:
        TypeKind Kind;
        u32      UniqueId = 0;
    };

    struct TypeInfo {
        Type* Ty;
        bool  ConstMemory;
        bool  AllowImplicitPtr = false;
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

    class SliceType : public ContainerType {
    public:

        static SliceType* Create(Type* ElmTy, ArcoContext& Context);

    private:
        SliceType()
            : ContainerType(TypeKind::Slice) {}
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
        static StructType* Create(InterfaceDecl* Interface, ArcoContext& Context);

        StructDecl*    GetStruct() const { return Struct; }
        EnumDecl*      GetEnum() const { return Enum; }
        InterfaceDecl* GetInterface() const { return Interface; }
        void AssignStruct(StructDecl* Struct) {
            this->Struct = Struct;
        }
        void AssignEnum(EnumDecl* Enum) {
            this->Enum = Enum;
            Kind = TypeKind::Enum;
        }
        void AssignInterface(InterfaceDecl* Interface) {
            this->Interface = Interface;
            Kind = TypeKind::Interface;
        }

        Identifier GetStructName() const { return StructName; }

        SourceLoc GetErrorLoc() const { return ErrorLoc; }

    private:
        StructType(TypeKind Kind)
            : Type(Kind) {}
        
        SourceLoc   ErrorLoc;
        Identifier  StructName;
        union {
            StructDecl*    Struct;
            EnumDecl*      Enum;
            InterfaceDecl* Interface;
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
            : Type(TypeKind::Function) {}

    };

    class GenericType : public Type {
    public:

        static GenericType* Create(Identifier Name, ulen Idx, ArcoContext& Context);

        const Identifier& GetName() const {
            return Name;
        }

        inline ulen GetIdx() const {
            return Idx;
        }

        inline void BindType(Type* Ty) {
            BoundTy = Ty;
        }

        inline void UnbindType() {
            BoundTy = nullptr;
        }

        inline const Type* GetBoundTy() const {
            return BoundTy;
        }

        inline Type* GetBoundTy() {
            return BoundTy;
        }

    private:
        Identifier Name;
        ulen       Idx = 0;
        Type*      BoundTy = nullptr;

        GenericType()
            : Type(TypeKind::Generic) {}
    };

}

#endif // ARCO_TYPES_H