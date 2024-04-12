#ifndef ARCO_TYPES_H
#define ARCO_TYPES_H

#include <string>
#include <llvm/ADT/SmallVector.h>

#include "Source.h"
#include "Identifier.h"

namespace llvm {
    class Module;
    class StructType;
}

namespace arco {

    class ArcoContext;
    struct Decl;
    struct FuncDecl;
    struct Expr;
    struct StructDecl;
    struct EnumDecl;
    struct IdentRef;
    struct InterfaceDecl;
    struct GenericBinding;
    class ContainerType;
    class ImplPointerType;
    class PointerType;
    class SliceType;
    class ArrayType;
    class StructType;
    class FunctionType;
    class GenericType;

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

        bool TypeHasStorage(bool FromPtr = false) const;
        
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

        std::string ToString(bool                               ShowFullGenericTy       = true,
                             const llvm::SmallVector<Type*, 8>* PartiallyBoundFuncTys   = nullptr,
                             const llvm::SmallVector<Type*, 8>* PartiallyBoundStructTys = nullptr,
                             bool                               ReplaceGenTyWithBoundTy = false) const;

        inline void SetUniqueId(u32 Id) {
            UniqueId = Id;
        }

        inline u32 GetUniqueId() {
            return UniqueId;
        }

        inline void SetQualType(Type* QualTy) {
            QualType = QualTy;
        }

        inline Type* GetQualType() {
            return QualType;
        }

    private:
        
        bool HasTypeBound(const llvm::SmallVector<Type*, 8>* PartiallyBoundFuncTys,
                          const llvm::SmallVector<Type*, 8>* PartiallyBoundStructTys) const;

    protected:
        Type* QualType;
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
        static ArrayType* Create(Type*        ElmTy,
                                 Expr*        LengthExpr,
                                 bool         AllowDynamic,
                                 ArcoContext& Context);

        ulen GetLength() const { return Length; }

        Expr* GetLengthExpr() const { return LengthExpr; }

        /// This is the product of all the lengths
        /// of the dimensions of the array.
        ulen GetTotalLinearLength() const;

        inline void AssignLength(ulen Length) {
            this->Length = Length;
        }

        inline bool AllowsForDynamic() const {
            return AllowDynamic;
        }

    private:
        ArrayType()
            : ContainerType(TypeKind::Array) {}

        Expr*     LengthExpr = nullptr;
        ulen      Length;
        bool      AllowDynamic;
    };

    class StructType : public Type {
    public:
        
        static StructType* Create(Identifier StructName, llvm::SmallVector<Type*, 8> BindTypes, SourceLoc ErrorLoc, ArcoContext& Context);
        static StructType* Create(StructDecl* Struct, llvm::SmallVector<Type*, 8> BindTypes, ArcoContext& Context);
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

        inline Identifier GetStructName() const { return StructName; }

        inline SourceLoc GetErrorLoc() const { return ErrorLoc; }

        inline const llvm::SmallVector<Type*, 8>& GetBindTypes() const {
            return BindTypes;
        }

        bool DoesFieldsHaveAssignment() const;
        bool DoesNeedsDestruction() const;
        bool DoesNeedsMove() const;
        bool DoesNeedsCopy() const;
        bool DoesMustForceRaise() const;

        llvm::StructType* LLStructType;
        GenericBinding*   FoundBinding;

    private:
        StructType(TypeKind Kind)
            : Type(Kind) {}
        
        SourceLoc   ErrorLoc;
        Identifier  StructName;

        llvm::SmallVector<Type*, 8> BindTypes;
        union {
            StructDecl*    Struct = nullptr;
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

        static GenericType* Create(Identifier Name,
                                   IdentRef* ConstraintRef,
                                   bool InvertConstraint,
                                   ulen Idx,
                                   SourceLoc ErrorLoc,
                                   ArcoContext& Context);

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

        inline IdentRef* GetConstraintRef() const {
            return ConstraintRef;
        }

        inline bool ShouldInvertConstraint() const {
            return InvertConstraint;
        }

        inline Decl* GetBoundToDecl() const {
            return BoundToDecl;
        }

        inline void SetBoundToDecl(Decl* D) {
            BoundToDecl = D;
        }

        SourceLoc GetErrorLoc() const {
            return ErrorLoc;
        }

    private:
        Identifier Name;
        IdentRef*  ConstraintRef;
        bool       InvertConstraint;
        ulen       Idx = 0;
        Type*      BoundTy = nullptr;
        Decl*      BoundToDecl;
        SourceLoc  ErrorLoc;

        GenericType()
            : Type(TypeKind::Generic) {}
    };

}

namespace llvm {
    raw_ostream& operator<<(raw_ostream& OS, const arco::Type* Ty);
}

#endif // ARCO_TYPES_H