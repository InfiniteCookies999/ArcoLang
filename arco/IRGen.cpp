#include "IRGen.h"

#include "Context.h"
#include "EmitDebugInfo.h"
#include "TypeBinding.h"

#include <unordered_set>

//===-------------------------------===//
// Helper Functions
//===-------------------------------===//

llvm::IntegerType* arco::GetSystemIntType(llvm::LLVMContext& LLContext, llvm::Module& LLModule) {
    return llvm::IntegerType::getIntNTy(LLContext, LLModule.getDataLayout().getPointerSizeInBits());
}

struct LLValTypePrinter {
    LLValTypePrinter(const llvm::Value* arg)
        : Arg(arg) {}

    const llvm::Value* Arg;
    void PrintLLType(llvm::raw_ostream& OS, const llvm::Value* LLValue) const {
        LLValue->getType()->print(OS);
    }
};

struct LLTypePrinter {
    LLTypePrinter(const llvm::Type* arg)
        : Arg(arg) {}

    const llvm::Type* Arg;
    void PrintLLType(llvm::raw_ostream& OS, const llvm::Type* LLType) const {
        LLType->print(OS);
    }
};

llvm::raw_ostream& operator<<(llvm::raw_ostream& OS, const LLValTypePrinter& Printer) {
    Printer.PrintLLType(OS, Printer.Arg);
    return OS;
}

llvm::raw_ostream& operator<<(llvm::raw_ostream& OS, const LLTypePrinter& Printer) {
    Printer.PrintLLType(OS, Printer.Arg);
    return OS;
}


namespace arco {
static arco::FuncDecl* GetMappedInterfaceFunc(FuncDecl* InterfaceFunc, FuncsList& Funcs) {
    for (FuncDecl* Func : Funcs) {
        if (Func->MappedInterfaceFunc == InterfaceFunc) {
            return Func;
        }
    }
    assert(!"Unreachable: did not find mapped interface function");
    return nullptr;
}
}

bool arco::FuncUsesParamRetSlot(llvm::Module& LLModule, StructType* StructTy, ulen SizeInBytes) {
    return StructTy->GetStruct()->NeedsDestruction ||
        SizeInBytes > LLModule.getDataLayout().getPointerSize();
}

bool arco::FuncUsesParamRetSlot(ArcoContext& Context, StructType* StructTy) {
    ulen SizeInBytes = SizeOfTypeInBytes(Context.LLArcoModule, GenStructType(Context, StructTy->GetStruct()));
    return FuncUsesParamRetSlot(Context.LLArcoModule,
                                StructTy,
                                SizeInBytes);
}

ulen arco::SizeOfTypeInBytes(llvm::Module& LLModule, llvm::Type* LLType) {
    if (!LLType->isSized()) {
        return 0;
    }
    const llvm::DataLayout& LLDataLayout = LLModule.getDataLayout();
    llvm::TypeSize LLTypeSize = LLDataLayout.getTypeAllocSize(LLType);
    return LLTypeSize.getFixedSize();
}

ulen NextPow2(ulen V) {
    --V;
    V |= V >> 1;
    V |= V >> 2;
    V |= V >> 4;
    V |= V >> 8;
    V |= V >> 16;
    V++;
    return V;
};

llvm::Type* arco::GenType(ArcoContext& Context, Type* Ty) {
    llvm::LLVMContext& LLContext = Context.LLContext;
    switch (Ty->GetKind()) {
    case TypeKind::Char:
    case TypeKind::Int8:
    case TypeKind::UInt8:
        return llvm::Type::getInt8Ty(LLContext);
    case TypeKind::Int16:
    case TypeKind::UInt16:
        return llvm::Type::getInt16Ty(LLContext);
    case TypeKind::Int32:
    case TypeKind::UInt32:
        return llvm::Type::getInt32Ty(LLContext);
    case TypeKind::Int64:
    case TypeKind::UInt64:
        return llvm::Type::getInt64Ty(LLContext);
    case TypeKind::Int:
    case TypeKind::Ptrsize:
        return GetSystemIntType(LLContext, Context.LLArcoModule);
    case TypeKind::Bool:
        return llvm::Type::getInt1Ty(LLContext);
    case TypeKind::Void:
        return llvm::Type::getVoidTy(LLContext);
    case TypeKind::CStr:
        return llvm::Type::getInt8PtrTy(LLContext);
    case TypeKind::Float32:
        return llvm::Type::getFloatTy(LLContext);
    case TypeKind::Float64:
        return llvm::Type::getDoubleTy(LLContext);
    case TypeKind::Pointer: {
        PointerType* PtrTy = Ty->AsPointerTy();
        Type* ElmTy = PtrTy->GetElementType();
        if (ElmTy->Equals(Context.VoidType)) {
            return llvm::Type::getInt8PtrTy(LLContext);
        } else if (ElmTy->GetKind() == TypeKind::Interface) {
            InterfaceDecl* Interface = ElmTy->AsStructType()->GetInterface();
            if (Interface->NumFuncs == 1) {
                // Will just load the function immediately.
                llvm::Type* PtrTy = llvm::PointerType::get(GenArcoConvFuncType(Context, Interface->Funcs[0]), 0);
                return llvm::PointerType::get(PtrTy, 0);
            } else {
                // i8** point to index in vtable array.
                llvm::Type* PtrTy = llvm::PointerType::get(llvm::Type::getInt8PtrTy(LLContext), 0);
                return llvm::PointerType::get(PtrTy, 0);
            }
        } else {
            return llvm::PointerType::get(GenType(Context, PtrTy->GetElementType()), 0);
        }
    }
    case TypeKind::Array: {
        ArrayType* ArrayTy = Ty->AsArrayTy();
        return llvm::ArrayType::get(GenType(Context, ArrayTy->GetElementType()), ArrayTy->GetLength());
    }
    case TypeKind::Function: {
        FunctionType* FuncTy = Ty->AsFunctionType();
        llvm::SmallVector<llvm::Type*, 4> LLParamTypes;
        

        bool UsesParamRetSlot = false;
        Type* RetTy = FuncTy->RetTyInfo.Ty;
        llvm::Type* LLRetType;
        llvm::StructType* LLStructType;
        if (RetTy->GetKind() == TypeKind::Struct) {
            StructType* StructTy = RetTy->AsStructType();
            LLStructType = GenStructType(Context, StructTy->GetStruct());
            ulen SizeInBytes = SizeOfTypeInBytes(Context.LLArcoModule, LLStructType);
            UsesParamRetSlot = FuncUsesParamRetSlot(Context.LLArcoModule, RetTy->AsStructType(), SizeInBytes);
            if (UsesParamRetSlot) {
                LLRetType = llvm::Type::getVoidTy(LLContext);
            } else {
                LLRetType = llvm::Type::getIntNTy(LLContext, NextPow2(SizeInBytes) * 8); // *8 because bits
            }
        } else {
            LLRetType = GenType(Context, RetTy);
        }
        
        // TODO: This relies on the calling convention so there will need
        // to be a way to attach calling convention information to the type
        // so that the compiler knows how to properly pass parameters.
        //
        // For now it just conforms to arco's way of handling parameters.
        if (UsesParamRetSlot) {
            LLParamTypes.push_back(llvm::PointerType::get(LLStructType, 0));
        }

        for (TypeInfo ParamTyInfo : FuncTy->ParamTypes) {
            Type* Ty = ParamTyInfo.Ty;
            if (Ty->GetKind() == TypeKind::Array) {
                // Arrays are decayed when passed.
                LLParamTypes.push_back(
                    llvm::PointerType::get(GenType(Context, Ty->AsArrayTy()->GetElementType()), 0)
                );
            } else {
                LLParamTypes.push_back(GenType(Context, Ty));
            }
        }

        return llvm::PointerType::get(llvm::FunctionType::get(LLRetType, LLParamTypes, false), 0);
    }
    case TypeKind::Struct:
        return GenStructType(Context, Ty->AsStructType()->GetStruct());
    case TypeKind::Slice: {
        SliceType* SliceTy = Ty->AsSliceTy();
        auto Itr = Context.LLSliceTypes.find(SliceTy->GetUniqueId());
        if (Itr != Context.LLSliceTypes.end()) {
            return Itr->second;
        }
        llvm::StructType* LLSliceType = llvm::StructType::create(Context.LLContext);
        Context.LLSliceTypes.insert({ SliceTy->GetUniqueId(), LLSliceType });

        llvm::SmallVector<llvm::Type*> LLStructFieldTypes = {
            GetSystemIntType(LLContext, Context.LLArcoModule),
            llvm::PointerType::get(GenType(Context, SliceTy->GetElementType()), 0)
        };
        LLSliceType->setBody(LLStructFieldTypes);
        LLSliceType->setName("__slice");

        return LLSliceType;
    }
    case TypeKind::Interface:
        return llvm::Type::getVoidTy(LLContext);
    default:
        assert(!"Failed to implement case for GenType()");
        return nullptr;
    }
}

llvm::StructType* arco::GenStructType(ArcoContext& Context, StructDecl* Struct) {
    assert(!Struct->Is(AstKind::ENUM_DECL) && "Wrong gen type for enum");

    if (Struct->LLStructTy) {
        return Struct->LLStructTy;
    }
    llvm::StructType* LLStructTy = llvm::StructType::create(Context.LLContext);
    Struct->LLStructTy = LLStructTy; // Set early to prevent endless recursive

    llvm::SmallVector<llvm::Type*> LLStructFieldTypes;
    ulen NumFields = 0;
    if (!Struct->Interfaces.empty()) {
        NumFields += Struct->Interfaces.size();
    }
    // Count the fields which are not compile time generated.
    for (VarDecl* Field : Struct->Fields) {
        ++NumFields;
    }
    LLStructFieldTypes.resize(NumFields);
    for (ulen i = 0; i < Struct->Interfaces.size(); i++) {
        InterfaceDecl* Interface = Struct->Interfaces[i];
        if (Interface->NumFuncs == 1) {
            FuncDecl* InterfaceFunc = Interface->Funcs[0];
            LLStructFieldTypes[i] = llvm::PointerType::get(GenArcoConvFuncType(Context, InterfaceFunc), 0);
        } else {
            LLStructFieldTypes[i] = llvm::Type::getInt8PtrTy(Context.LLContext);
        }
    }

    if (Struct->Fields.empty()) {
        LLStructFieldTypes.push_back(llvm::Type::getInt8Ty(Context.LLContext));
    } else {
        for (VarDecl* Field : Struct->Fields) {
            LLStructFieldTypes[Field->LLFieldIdx] = GenType(Context, Field->Ty);
        }
    }
    LLStructTy->setBody(LLStructFieldTypes);
    LLStructTy->setName(Struct->Name.Text.str());

    if (!Struct->Interfaces.empty()) {
        const llvm::StructLayout* LLLayout = Context.LLArcoModule.getDataLayout().getStructLayout(LLStructTy);
        // Obtains offset past virtual data.
        Struct->VirtualOffset = LLLayout->getElementOffset(Struct->Interfaces.size());
    }

    return LLStructTy;
}

llvm::FunctionType* arco::GenArcoConvFuncType(ArcoContext& Context, FuncDecl* Func) {
    llvm::Module& LLModule = Context.LLArcoModule;
    llvm::LLVMContext& LLContext = Context.LLContext;

    llvm::Type* LLRetTy;
    if (Func->RetTy->GetKind() == TypeKind::Struct) {
        StructType* StructTy = Func->RetTy->AsStructType();

        ulen SizeInBytes = SizeOfTypeInBytes(LLModule, GenStructType(Context, StructTy->GetStruct()));
        if (!FuncUsesParamRetSlot(LLModule, StructTy, SizeInBytes)) {
            // Return type is optimized to fit into an integer.
            Func->UsesOptimizedIntRet = true;
            LLRetTy = llvm::Type::getIntNTy(LLContext, NextPow2(SizeInBytes) * 8); // *8 because bits
        } else {
            // Copy elision case by passing return value as param.
            Func->UsesParamRetSlot = true;
            LLRetTy = llvm::Type::getVoidTy(LLContext);
        }
    } else {
        LLRetTy = Func == Context.MainEntryFunc ? llvm::Type::getInt32Ty(LLContext)
            : GenType(Context, Func->RetTy);
    }

    llvm::SmallVector<llvm::Type*, 4> LLParamTypes;
    ulen ImplicitParams = 0;

    if (Func->MappedInterfaceFunc || Func->Interface) {
        // In the case that the function is mapping to an interface function it actually needs
        // to recieve an i8* because the pointer that the interface passes won't be correct and
        // the implementation function (this function) will have to reajust the "this" pointer
        // to be correct.
        LLParamTypes.push_back(llvm::Type::getInt8PtrTy(LLContext));
        ++ImplicitParams;
    } else if (Func->Struct) {
        // Member functions recieve pointers to the struct they
        // are contained inside of.
        LLParamTypes.push_back(llvm::PointerType::get(GenStructType(Context, Func->Struct), 0));
        ++ImplicitParams;
    }
    if (Func->UsesParamRetSlot) {
        LLParamTypes.push_back(llvm::PointerType::get(GenType(Context, Func->RetTy), 0));
        ++ImplicitParams;
    }
    if (!Func->RaisedErrors.empty()) {
        // TODO: Cache
        llvm::Type* PtrTy = llvm::PointerType::get(GenArcoConvFuncType(Context, Context.StdErrorInterface->Funcs[0]), 0);
        LLParamTypes.push_back(llvm::PointerType::get(llvm::PointerType::get(PtrTy, 0), 0));

        for (const auto& RaisedError : Func->RaisedErrors) {
            llvm::PointerType* LLErrorPassTy = llvm::PointerType::get(GenStructType(Context, RaisedError.ErrorStruct), 0);
            LLParamTypes.push_back(LLErrorPassTy);
        }
        ImplicitParams += 1 + Func->RaisedErrors.size();
    }

    for (VarDecl* Param : Func->Params) {
        Type* Ty = Param->Ty;
        if (Ty->GetKind() == TypeKind::Array) {
            // Arrays are decayed when passed.
            LLParamTypes.push_back(
                llvm::PointerType::get(GenType(Context, Ty->AsArrayTy()->GetElementType()), 0)
            );
        } else {
            LLParamTypes.push_back(GenType(Context, Ty));
        }
    }

    return llvm::FunctionType::get(LLRetTy, LLParamTypes, false);
}

llvm::Twine arco::GenFuncLinkName(FuncDecl* Func, bool IsMainFunc) {
    llvm::Twine LLFuncName = !Func->NativeName.empty() ? Func->NativeName : Func->Name.Text;
    llvm::Twine LLFullFuncName = (IsMainFunc || (Func->Mods & ModKinds::NATIVE))
                               ? LLFuncName
                               : LLFuncName.concat(".arco");
    return LLFullFuncName;
}


#define PUSH_SCOPE()        \
Scope NewScope;             \
NewScope.Parent = LocScope; \
LocScope = &NewScope;

#define POP_SCOPE()                  \
DestroyLocScopeInitializedObjects(); \
LocScope = LocScope->Parent;

#define EMIT_DI(S) if (EmitDebugInfo) { S; }

arco::IRGenerator::IRGenerator(ArcoContext& Context)
    : Context(Context),
      LLContext(Context.LLContext),
      LLModule(Context.LLArcoModule),
      Builder(Context.LLContext),
      EmitDebugInfo(Context.EmitDebugInfo)
{
}

void arco::IRGenerator::GenFunc(FuncDecl* Func, GenericBind* Binding) {
    
    // -- DEBUG
    // llvm::outs() << "generating function: " << (Func->Struct ? Func->Struct->Name.Text.str() + "." : "") << Func->Name << '\n';

    if (Func->IsGeneric()) {
        BindTypes(Func, Binding);
    }

    GenFuncDecl(Func, Binding);
    GenFuncBody(Func, Binding);

    if (Func->IsGeneric()) {
        UnbindTypes(Func);
    }
}

void arco::IRGenerator::GenGlobalVar(VarDecl* Global) {

    GenGlobalVarDecl(Global);

    llvm::GlobalVariable* LLGVar =
        llvm::cast<llvm::GlobalVariable>(Global->LLAddress);
    auto [NoFurtherInit, InitValue] = GenGlobalVarInitializeValue(Global);

    LLGVar->setInitializer(InitValue);

    if (!NoFurtherInit) {
        Context.GlobalPostponedAssignments.push_back(Global);
    }
}

void arco::IRGenerator::GenImplicitDefaultConstructorBody(StructDecl* Struct) {
    LLFunc = Struct->LLDefaultConstructor;

    llvm::BasicBlock* LLEntryBlock = llvm::BasicBlock::Create(LLContext, "func.entry", LLFunc);
    Builder.SetInsertPoint(LLEntryBlock);

    LLThis = LLFunc->getArg(0);
    LLThis->setName("this");

    if (!Struct->Interfaces.empty()) {
        GenCallToInitVTableFunc(LLThis, Struct);
    }
    for (VarDecl* Field : Struct->Fields) {
        llvm::Value* LLFieldAddr = CreateStructGEP(LLThis, Field->LLFieldIdx);
        if (Field->Assignment) {
            GenAssignment(LLFieldAddr, Field->Ty, Field->Assignment, Field->HasConstAddress);
        } else if (!Field->LeaveUninitialized) {
            GenDefaultValue(Field->Ty, LLFieldAddr);
        }
    }

    Builder.CreateRetVoid();

}

void arco::IRGenerator::GenGlobalInitFuncBody() {
    llvm::BasicBlock* LLEntryBlock = llvm::BasicBlock::Create(LLContext, "entry.block", Context.LLInitGlobalFunc);

    LLFunc = Context.LLInitGlobalFunc;	
    Builder.SetInsertPoint(LLEntryBlock);

    // Iterator since it is modifiable during generation.
    auto Itr = Context.GlobalPostponedAssignments.begin();
    while (Itr != Context.GlobalPostponedAssignments.end()) {
        VarDecl* Global = *Itr;
        if (Global->Assignment) {
            GenAssignment(Global->LLAddress, Global->Ty, Global->Assignment, Global->HasConstAddress);
        } else {
            if (Global->Ty->GetKind() == TypeKind::Struct) {
                CallDefaultConstructor(Global->LLAddress, Global->Ty->AsStructType());
            } else if (Global->Ty->GetKind() == TypeKind::Array &&
                   Global->Ty->AsArrayTy()->GetBaseType()->GetKind() == TypeKind::Struct) {
                ArrayType* ArrayTy = Global->Ty->AsArrayTy();
                llvm::Value* LLArrStartPtr = MultiDimensionalArrayToPointerOnly(Global->LLAddress, ArrayTy);
                llvm::Value* LLTotalLinearLength = GetSystemUInt(ArrayTy->GetTotalLinearLength());
                StructArrayCallDefaultConstructors(ArrayTy->GetBaseType(), LLArrStartPtr, LLTotalLinearLength);
            }
        }
        ++Itr;
    }
    
    Builder.CreateRetVoid();
}

void arco::IRGenerator::GenGlobalDestroyFuncBody() {
    llvm::BasicBlock* LLEntryBlock = llvm::BasicBlock::Create(LLContext, "entry.block", Context.LLDestroyGlobalsFunc);
        
    LLFunc = Context.LLDestroyGlobalsFunc;
    Builder.SetInsertPoint(LLEntryBlock);

    for (VarDecl* Global : Context.GlobalsNeedingDestruction) {
        CallDestructors(Global->Ty, Global->LLAddress, nullptr);
    }

    Builder.CreateRetVoid();
}

void arco::IRGenerator::GenFuncDecl(FuncDecl* Func, GenericBind* Binding) {
    if (Func->IsGeneric()) {
        BindTypes(Func, Binding);
    }

    if (Func->GetLLFunction()) return;

    if (Func->Mods & ModKinds::NATIVE) {
        Identifier Name = Func->Name;
        if (!Func->NativeName.empty()) {
            Name = Identifier(Func->NativeName);
        }

        auto Itr = Context.LLVMIntrinsicsTable.find(Name);
        if (Itr != Context.LLVMIntrinsicsTable.end()) {
            Func->LLVMIntrinsicID = Itr->second;
            return;
        }
    }
    
    // TODO: Native functions will not need to return data structures
    //       in the same way.

    llvm::FunctionType* LLFuncType = GenArcoConvFuncType(Context, Func);

    llvm::Function* LLFunc = llvm::Function::Create(
        LLFuncType,
        llvm::Function::ExternalLinkage,
        GenFuncLinkName(Func, Func == Context.MainEntryFunc),
        LLModule
    );

    if (Func->Mods & ModKinds::NATIVE) {
#ifdef _WIN32
        if (!Func->CallingConv.IsNull()) {
            LLFunc->setCallingConv(Context.CallConventions[Func->CallingConv]);
        } else {
            LLFunc->setDLLStorageClass(llvm::GlobalValue::DLLImportStorageClass);
            LLFunc->setCallingConv(llvm::CallingConv::X86_StdCall);
        }
        if (Func->Mods & ModKinds::DLLIMPORT) {
            LLFunc->setDLLStorageClass(llvm::GlobalValue::DLLImportStorageClass);
        }
#endif
    } else {
        // Will resolve the symbol within the same compilation unit.
        LLFunc->setDSOLocal(true);
    }

    Func->SetLLFunction(LLFunc);

    ulen ImplicitParams = 0;
    if (Func->Struct) {
        ++ImplicitParams;
    }
    if (Func->UsesParamRetSlot) {
        ++ImplicitParams;
        GetElisionRetSlotAddr(Func)->setName("ret.addr");
    }
    if (!Func->RaisedErrors.empty()) {
        ImplicitParams += 1 + Func->RaisedErrors.size();
    }
    if (Func->Struct) {
        LLFunc->getArg(0)->addAttr(llvm::Attribute::NoUndef);
    }
    for (ulen i = 0; i < Func->Params.size(); i++) {
        LLFunc->getArg(i + ImplicitParams)->setName(llvm::Twine(Func->Params[i]->Name.Text).concat(".param"));
        //LLFunc->getArg(i + ImplicitParams)->addAttr(llvm::Attribute::NoUndef);
        //LLFunc->getArg(i + ImplicitParams)->setName(Func->Params[i]->Name.Text);
    }

}

void arco::IRGenerator::GenFuncBody(FuncDecl* Func, GenericBind* Binding) {
    if (Func->Mods & ModKinds::NATIVE) return;

    CFunc  = Func;
    LLFunc = Func->GetLLFunction();
    
    // Entry block for the function.
    llvm::BasicBlock* LLEntryBlock = llvm::BasicBlock::Create(LLContext, "func.entry", LLFunc);
    Builder.SetInsertPoint(LLEntryBlock);

    EMIT_DI(GetDIEmitter(Func)->EmitFunc(Func));

    if (Func->NumReturns > 1) {
        LLFuncEndBB = llvm::BasicBlock::Create(LLContext, "func.return");
        if (Func == Context.MainEntryFunc) {
            LLRetAddr = Builder.CreateAlloca(GenType(Context.Int32Type), nullptr, "ret.val");
        } else if (Func->RetTy->GetKind() != TypeKind::Void &&
                   !Func->UsesParamRetSlot) {
            LLRetAddr = Builder.CreateAlloca(GenType(Func->RetTy), nullptr, "ret.val");
        }
    } else if (Func->NumReturns == 1 && !Func->RaisedErrors.empty()) {
        // The really is still more than one return value need to create a return address.
        LLFuncEndBB = llvm::BasicBlock::Create(LLContext, "func.return");
        if (Func->RetTy->GetKind() != TypeKind::Void &&
            !Func->UsesParamRetSlot) {
            LLRetAddr = Builder.CreateAlloca(GenType(Func->RetTy), nullptr, "ret.val");
        }
    }

    PUSH_SCOPE(); // Push function scope!
    // Allocating space for the variables
    //
    for (VarDecl* Var : Func->AllocVars) {
        if (Var->IsComptime()) continue;

        if (Func->UsesParamRetSlot && Func->NumReturns == 1 && Var->IsLocalRetValue) {
            // RVO case in which although the variable was declared as inside
            // of the function since it is returned by the function and the
            // function uses a parameter for a return slot the variable may use
            // the address of the return slot instead.
            Var->LLAddress = GetElisionRetSlotAddr(CFunc);
        } else {
            llvm::Type* LLTy;
            if (Var->IsParam() && Var->Ty->GetKind() == TypeKind::Array) {
                ArrayType* ArrayTy = Var->Ty->AsArrayTy();
                LLTy = llvm::PointerType::get(GenType(ArrayTy->GetElementType()), 0);
            } else {
                LLTy = GenType(Var->Ty);
            }

            llvm::Value* LLAlloca = Builder.CreateAlloca(LLTy);
            LLAlloca->setName(Var->Name.Text);
            Var->LLAddress = LLAlloca;
        }
    }

    // Storing the incoming parameters
    //
    ulen LLParamIndex = 0;
    if (Func->Struct) {
        // Member function pointer.
        LLThis = LLFunc->getArg(LLParamIndex++);
        llvm::Type* LLThisFinalTy;
        if (Func->MappedInterfaceFunc) {
            // Must adjust the "this" pointer since the interfaces are not guaranteed to pass the correct address.
            StructDecl* Struct = Func->Struct;
            llvm::StructType* LLStructTy = GenStructType(Func->Struct);
            LLThisFinalTy = llvm::PointerType::get(LLStructTy, 0);
            ulen InterfaceOffset = 0;
            InterfaceDecl* Interface = Func->MappedInterfaceFunc->Interface;
            for (InterfaceDecl* InheritedInterface : Struct->Interfaces) {
                if (InheritedInterface == Interface) {
                    break;
                } else {
                    ++InterfaceOffset;
                }
            }

            if (InterfaceOffset != 0) {
                // Need to subtract from the passed address to get back to the struct's base address.

                // TODO: Should we be relying on the layout information of the struct instead?
                const llvm::StructLayout* LLLayout = LLModule.getDataLayout().getStructLayout(LLStructTy);

                long long LLInterfaceOffset = (long long)LLLayout->getElementOffset(InterfaceOffset);
                LLThis = CreateInBoundsGEP(LLThis, { GetLLInt64(-LLInterfaceOffset) });
            }
            LLThis = Builder.CreateBitCast(LLThis, llvm::PointerType::get(LLStructTy, 0));
        } else {
            LLThisFinalTy = LLFunc->getArg(0)->getType();
        }

        llvm::Value* LLThisAddr = Builder.CreateAlloca(LLThisFinalTy, nullptr, "this.addr");
        Builder.CreateStore(LLThis, LLThisAddr);
        LLThis = CreateLoad(LLThisAddr);
        LLThis->setName("this");
        EMIT_DI(GetDIEmitter(Func)->EmitThisVar(LLThisAddr, Func, Builder));
    }
    if (Func->UsesParamRetSlot) {
        ++LLParamIndex;
    }
    if (!Func->RaisedErrors.empty()) {
        LLParamIndex += 1 + Func->RaisedErrors.size();
    }

    for (VarDecl* Param : Func->Params) {
        Builder.CreateStore(LLFunc->getArg(LLParamIndex++), Param->LLAddress);
        AddObjectToDestroyOpt(Param->Ty, Param->LLAddress);

        // TODO: When passing structures it does not end up knowing how to read the memory for some reason.
        // I wasted a bunch of time trying to fix the problem but the ll code generated by clang that I was
        // using to compare against ended up having the exact same problem. Oddly enough this problem goes
        // away if you store the parameter into a variable.
        //EMIT_DI(GetDIEmitter(Func)->EmitParam(Func, Param, Builder));
        EMIT_DI(GetDIEmitter(Func)->EmitLocalVar(Param, Builder));
    }

    if (Func->IsConstructor) {
        GenConstructorBodyFieldAssignments(Func, Func->Struct);
    }

    if (Func == Context.MainEntryFunc) {
        Context.LLInitGlobalFunc = GenGlobalInitFuncDecl();
        Builder.CreateCall(Context.LLInitGlobalFunc);
    }

    // Generating the statements of the function.
    for (AstNode* Stmt : Func->Scope.Stmts) {
        GenNode(Stmt);
    }

    // Before branching and destroying the objects which are
    // always destroyed need to still cleanup the function
    // scope's objects.
    if (LLFuncEndBB) {
        POP_SCOPE();
    }
    
    // Branching to LLFuncEndBB if its needed.
    if (LLFuncEndBB && Builder.GetInsertBlock()->empty()) {
        // If the current block is empty we can just use
        // the current block as the ending block instead.
        LLFuncEndBB->replaceAllUsesWith(Builder.GetInsertBlock());
        delete LLFuncEndBB;
    } else if (LLFuncEndBB) {
        LLFunc->getBasicBlockList().push_back(LLFuncEndBB); // Because the parent was not originally set
        GenBranchIfNotTerm(LLFuncEndBB);
        Builder.SetInsertPoint(LLFuncEndBB);
    }

    if (Func == Context.MainEntryFunc) {
        Context.LLDestroyGlobalsFunc = GenDestroyGlobalsFuncDecl();
        if (Func->NumReturns == 1 && EncounteredReturn) {
            // User defined return so must insert before the
            // return instruction.
            Builder.SetInsertPoint(&Builder.GetInsertBlock()->back());
            Builder.CreateCall(Context.LLDestroyGlobalsFunc);
        }
    }

    // If this function is a destructor then we still need to destroy the fields.
    if (Func->IsDestructor) {
        for (VarDecl* Field : Func->Struct->Fields) {
            if (Field->Ty->TypeNeedsDestruction()) {
                CallDestructors(Field->Ty, CreateStructGEP(LLThis, Field->LLFieldIdx), nullptr);
            }
        }
    }

    llvm::Instruction* LLRet = nullptr;
    if (LLFuncEndBB) {
        
        CallDestructors(AlwaysInitializedDestroyedObjects);
        // Call global destructors after destroying local values.
        if (Func == Context.MainEntryFunc) {
            Builder.CreateCall(Context.LLDestroyGlobalsFunc);
        }
        if (Func->UsesOptimizedIntRet) {
            LLRet = Builder.CreateRet(GenReturnValueForOptimizedStructAsInt(LLRetAddr));
        } else if (LLRetAddr) {
            LLRet = Builder.CreateRet(CreateLoad(LLRetAddr));
        } else {
            LLRet = Builder.CreateRetVoid();
        }
    } else if (Func->NumReturns == 1 && Func->RetTy == Context.VoidType) {
        if (!EncounteredReturn) {

            CallDestructors(AlwaysInitializedDestroyedObjects);
            // Call global destructors after destroying local values.
            if (Func == Context.MainEntryFunc) {
                Builder.CreateCall(Context.LLDestroyGlobalsFunc);
            }
            // Implicit void return.
            if (Func == Context.MainEntryFunc) {
                LLRet = Builder.CreateRet(GetLLInt32(0));
            } else {
                LLRet = Builder.CreateRetVoid();
            }
        }
    }

    if (EmitDebugInfo) {
        if (LLRet)
            GetDIEmitter(Func)->EmitDebugLocation(LLRet, Func->Scope.EndLoc);
        GetDIEmitter(Func)->EmitFuncEnd(Func);
    }
}

llvm::Function* arco::IRGenerator::GenGlobalInitFuncDecl() {
    llvm::FunctionType* LLFuncType =
        llvm::FunctionType::get(llvm::Type::getVoidTy(LLContext), false);
    llvm::Function* LLInitFunc =
        llvm::Function::Create(
            LLFuncType,
            llvm::Function::ExternalLinkage,
            "__arco.init.globals",
            LLModule
        );
    return LLInitFunc;
}

llvm::Function* arco::IRGenerator::GenDestroyGlobalsFuncDecl() {
    llvm::FunctionType* LLFuncType =
        llvm::FunctionType::get(llvm::Type::getVoidTy(LLContext), false);
    llvm::Function* LLDestroyFunc =
        llvm::Function::Create(
            LLFuncType,
            llvm::Function::ExternalLinkage,
            "__arco.destroy.globals",
            LLModule
        );
    return LLDestroyFunc;
}

llvm::Value* arco::IRGenerator::GenNode(AstNode* Node) {
    switch (Node->Kind) {
    case AstKind::VAR_DECL:
        return GenVarDecl(static_cast<VarDecl*>(Node));
    case AstKind::RETURN:
        return GenReturn(static_cast<ReturnStmt*>(Node));
    case AstKind::CONTINUE:
    case AstKind::BREAK:
        return GenLoopControl(static_cast<LoopControlStmt*>(Node));
    case AstKind::PREDICATE_LOOP:
        return GenPredicateLoop(static_cast<PredicateLoopStmt*>(Node));
    case AstKind::RANGE_LOOP:
        return GenRangeLoop(static_cast<RangeLoopStmt*>(Node));
    case AstKind::ITERATOR_LOOP:
        return GenIteratorLoop(static_cast<IteratorLoopStmt*>(Node));
    case AstKind::DELETE:
        return GenDelete(static_cast<DeleteStmt*>(Node));
    case AstKind::RAISE:
        return GenRaise(static_cast<RaiseStmt*>(Node));
    case AstKind::IF:
        return GenIf(static_cast<IfStmt*>(Node));
    case AstKind::NESTED_SCOPE:
        return GenNestedScope(static_cast<NestedScopeStmt*>(Node));
    case AstKind::BINARY_OP:
        return GenBinaryOp(static_cast<BinaryOp*>(Node));
    case AstKind::UNARY_OP:
        return GenUnaryOp(static_cast<UnaryOp*>(Node));
    case AstKind::NUMBER_LITERAL:
        return GenNumberLiteral(static_cast<NumberLiteral*>(Node));
    case AstKind::STRING_LITERAL:
        return GenStringLiteral(static_cast<StringLiteral*>(Node));
    case AstKind::NULLPTR:
        return llvm::Constant::getNullValue(GenType(static_cast<NullPtr*>(Node)->CastTy));
    case AstKind::BOOL_LITERAL: {
        BoolLiteral* B = static_cast<BoolLiteral*>(Node);
        if (B->TOF) return llvm::ConstantInt::getTrue(LLContext);
        else        return llvm::ConstantInt::getFalse(LLContext);
    }
    case AstKind::IDENT_REF:
        return GenIdentRef(static_cast<IdentRef*>(Node));
    case AstKind::FIELD_ACCESSOR:
        return GenFieldAccessor(static_cast<FieldAccessor*>(Node));
    case AstKind::THIS_REF:
        return LLThis;
    case AstKind::FUNC_CALL:
        return GenFuncCall(static_cast<FuncCall*>(Node), nullptr);
    case AstKind::ARRAY:
        return GenArray(static_cast<Array*>(Node), nullptr, false);
    case AstKind::ARRAY_ACCESS:
        return GenArrayAccess(static_cast<ArrayAccess*>(Node));
    case AstKind::TYPE_CAST:
        return GenTypeCast(static_cast<TypeCast*>(Node));
    case AstKind::TYPE_BITCAST:
        return GenTypeBitCast(static_cast<TypeBitCast*>(Node));
    case AstKind::STRUCT_INITIALIZER:
        return GenStructInitializer(static_cast<StructInitializer*>(Node), nullptr);
    case AstKind::HEAP_ALLOC:
        return GenHeapAlloc(static_cast<HeapAlloc*>(Node));
    case AstKind::SIZEOF:
        return GetSystemInt(
            SizeOfTypeInBytesNonVirtualInclusive(static_cast<SizeOf*>(Node)->TypeToGetSizeOf));
    case AstKind::TYPEOF:
        return GenTypeOf(static_cast<TypeOf*>(Node));
    case AstKind::MOVEOBJ:
        return GenNode(static_cast<MoveObj*>(Node)->Value);
    case AstKind::TERNARY:
        return GenTernary(static_cast<Ternary*>(Node), nullptr, false);
    case AstKind::VAR_DECL_LIST:
        return GenVarDeclList(static_cast<VarDeclList*>(Node));
    case AstKind::TRY_ERROR:
        return GenTryError(static_cast<TryError*>(Node), nullptr);
    default:
        assert(!"Unimplemented GenNode() case!");
        return nullptr;
    }
}

void arco::IRGenerator::GenGlobalVarDecl(VarDecl* Global) {
    if (Global->LLAddress) return; // Do not generate it twice

    std::string Name;
    if (Global->Mods & ModKinds::NATIVE) {
        if (!Global->NativeName.empty()) {
            Name = Global->NativeName.str();
        } else {
            Name = Global->Name.Text.str();
        }
    } else {
        Name = "__global." + Global->Name.Text.str();
        Name += "." + std::to_string(Context.NumGeneratedGlobalVars++);
    }

    llvm::GlobalVariable* LLGVar = GenLLVMGlobalVariable(Name, GenType(Global->Ty));
    Global->LLAddress = LLGVar;

    if (Global->Ty->TypeNeedsDestruction()) {
        Context.GlobalsNeedingDestruction.push_back(Global);
    }

    if (Global->Mods & ModKinds::NATIVE) {
#ifdef _WIN32
        LLGVar->setDLLStorageClass(llvm::GlobalValue::DLLImportStorageClass);
#endif
    } else {
        LLGVar->setDSOLocal(true);
    }

    if (Global->Ty->GetKind() == TypeKind::Array &&
        Global->HasConstAddress) {
        LLGVar->setConstant(true);
    }

    EMIT_DI(GetDIEmitter(Global)->EmitGlobalVar(Global, Builder));
}

llvm::Value* arco::IRGenerator::GenRValue(Expr* E) {
    llvm::Value* LLValue = GenNode(E);
    
    switch (E->Kind) {
    case AstKind::IDENT_REF:
    case AstKind::ARRAY_ACCESS:
    case AstKind::FIELD_ACCESSOR: {
        
        if (E->Is(AstKind::FIELD_ACCESSOR)) {
            FieldAccessor* FieldAcc = static_cast<FieldAccessor*>(E);
            if (FieldAcc->IsArrayLength || FieldAcc->EnumValue) {
                // Array lengths are not memory so no reason to load.
                // Enums refer to constant indexes.
                break;
            }
        }

        if (E->Is(AstKind::IDENT_REF) || E->Is(AstKind::FIELD_ACCESSOR)) {
            IdentRef* IRef = static_cast<IdentRef*>(E);
            if (IRef->RefKind == IdentRef::RK::Var && IRef->Var->IsComptime()) {
                // Do not load compile time variables.
                break;
            }
        }

        // Want to make sure not to load an array
        // since arrays should always be taken as
        // l-values.
        if (E->Ty->GetKind() != TypeKind::Array) {
            LLValue = CreateLoad(LLValue);
        }
        break;
    }
    case AstKind::UNARY_OP: {
        if (static_cast<UnaryOp*>(E)->Op == '*') {
            LLValue = CreateLoad(LLValue);
        }
        break;
    }
    case AstKind::STRUCT_INITIALIZER: {
        // Since an unseen allocation must have been generated to
        // create the allocation of the struct the struct is currently
        // a l-value.
        LLValue = CreateLoad(LLValue);
        break;
    }
    }
    
    if (E->CastTy) {
        LLValue = GenCast(E->CastTy, E->Ty, LLValue);
        if (E->CastTy->GetKind() == TypeKind::Slice ||
            E->CastTy->GetKind() == TypeKind::Struct /* cast to Any */) {
            // We did not load because it was an array but now we need
            // to load.
            LLValue = CreateLoad(LLValue);
        }
    }
    return LLValue;
}

//===-------------------------------===//
// Statements
//===-------------------------------===//

llvm::Value* arco::IRGenerator::GenVarDecl(VarDecl* Var) {
    if (Var->IsComptime()) return GenComptimeValue(Var);

    // If using RVO then there is no need to actually
    // add the object to be destroyed because the caller
    // will recieve a version of that object and manage
    // the object's memory.
    //
    // NOTE: When there are destructors functions do not use optimized integer returning
    //       but instead fall back on parameter return slots.
    bool LocalRet = CFunc->NumReturns == 1 && CFunc->UsesParamRetSlot && Var->IsLocalRetValue;
    if (LocalRet) {
        LocalReturnVar = Var;
    }

    EMIT_DI(GetDIEmitter(Var)->EmitLocalVar(Var, Builder));

    if (Var->IsErrorDecl) {
        FuncCall* Call = static_cast<FuncCall*>(Var->Assignment);
        llvm::SmallVector<llvm::Value*, 4> LLErrorAddrs = GenErrorAddrs(Call->CalledFunc, Var->LLAddress);
        GenFuncCall(Call, nullptr, LLErrorAddrs);
    } else if (Var->Assignment) {
        GenAssignment(Var->LLAddress, Var->Ty, Var->Assignment, Var->HasConstAddress);
    } else if (!Var->LeaveUninitialized) {
        GenDefaultValue(Var->Ty, Var->LLAddress);
    }

    if (!LocalRet) {
        bool ImplicitErrorRet = false;
        if (Var->Assignment && Var->Ty->GetKind() == TypeKind::Struct) {
            if (Var->Assignment->Is(AstKind::FUNC_CALL)) {
                FuncCall* Call = static_cast<FuncCall*>(Var->Assignment);
                if (Call->CalledFunc && !Call->CalledFunc->RaisedErrors.empty()) {
                    ImplicitErrorRet = true;
                }
            } else if (Var->Assignment->Is(AstKind::STRUCT_INITIALIZER)) {
                StructInitializer* StructInit = static_cast<StructInitializer*>(Var->Assignment);
                if (StructInit->CalledConstructor && !StructInit->CalledConstructor->RaisedErrors.empty()) {
                    ImplicitErrorRet = true;
                }
            }
        }

        if (ImplicitErrorRet) {
            // The variable is being conditionally initialized need to only destroy
            // the object if there is no error.

            llvm::Value* LLErrorInterfaceAddr = GetErrorRetAddr(0);
            llvm::Value* LLCond = Builder.CreateIsNull(CreateLoad(LLErrorInterfaceAddr));
            llvm::Value* LLCondAddr = CreateUnseenAlloca(llvm::Type::getInt1Ty(LLContext), "destroy.cond", true);
            Builder.CreateStore(LLCond, LLCondAddr);

            AddObjectToDestroyOpt(Var->Ty, Var->LLAddress, LLCondAddr);
            
        } else {
            AddObjectToDestroyOpt(Var->Ty, Var->LLAddress);
        }
    }

    // Emit the location for the generated assignment.
    if (EmitDebugInfo) {
        llvm::BasicBlock* LLBB = Builder.GetInsertBlock();

        if (LLBB->empty()) {
            // This can happen when dealing with raised errors because they generate
            // an if implement statement and we are at the end block of that if.
            // 
            // TODO: Fix this!
        } else {
            llvm::Instruction& LLAssignmentInst = LLBB->back();
            GetDIEmitter(CFunc)->EmitDebugLocation(&LLAssignmentInst, Var->Loc);
        }
    }
    return Var->LLAddress;
}

llvm::Value* arco::IRGenerator::GenReturn(ReturnStmt* Ret) {
    EncounteredReturn = true;

    if (!LLFuncEndBB) {
        // Only a single return so the instruction has full control over
        // returning and must create the return instructions.
    
        llvm::Value* LLRetValue = nullptr;
        
        if (CFunc->RetTy->GetKind() == TypeKind::Struct) {
            if (CFunc->UsesOptimizedIntRet) {
                LLRetValue = GenReturnValueForOptimizedStructAsInt(GenNode(Ret->Value));
            }
            else if (Ret->Value->Is(AstKind::IDENT_REF) && static_cast<IdentRef*>(Ret->Value)->Var->IsLocalRetValue) {
                // Do not need to move the object because we are returning a locally defined
                // value which already points to the return address.
                // 
                // Ex.
                //     'fn func() A {
                //         a A = A{ 44, 22 };
                //         a.g = 33;
                //         return a;
                //     }'
                //
                // Where sizeof(A) >= size of architecture's pointer size.
            }
            // Checking for cases in which no no existing LValue exists and the value needs
            // to be passed to prevent irrelevent copies.
            //
            // TODO: If we checked to make sure there are no lvalues in the ternary operator
            //       we could also get away with it here!
            else if (Ret->Value->Is(AstKind::STRUCT_INITIALIZER) ||
                     Ret->Value->Is(AstKind::FUNC_CALL)) {
                GenReturnByStoreToElisionRetSlot(Ret->Value, GetElisionRetSlotAddr(CFunc));
            }
            // Else the object needs to be copied/moved into the elision return address.
            else {
                llvm::Value* LLToAddr = GetElisionRetSlotAddr(CFunc);
                llvm::Value* LLFromAddr = GenNode(Ret->Value);

                StructDecl* Struct = CFunc->RetTy->AsStructType()->GetStruct();
                CopyOrMoveStructObject(LLToAddr, LLFromAddr, Struct);
            }
        } else if (!Ret->Value && CFunc == Context.MainEntryFunc) {
            // Default to returning zero for the main function.
            LLRetValue = GetLLInt32(0);
        } else if (Ret->Value) {
            LLRetValue = GenRValue(Ret->Value);
        }

        // Destroy all objects that have been generated up to
        // this point.
        CallDestructors(AlwaysInitializedDestroyedObjects);
        DestroyCurrentlyInitializedObjects();
    
        if (LLRetValue) {
            if (CFunc == Context.MainEntryFunc &&
                Ret->Value                     &&
                Ret->Value->Ty != Context.Int32Type) {
                LLRetValue = GenCast(Context.Int32Type, Ret->Value->Ty, LLRetValue);
            }
            Builder.CreateRet(LLRetValue);
        } else {
            Builder.CreateRetVoid();
        }

        EMIT_DI(EmitDebugLocation(Ret));

    } else {

        // Multiple returns exists. All returns jump to the LLFuncEndBB.
        // Storing the return value into LLRetAddr and letting LLFuncEndBB
        // handle the code for returning the value.

        if (CFunc->RetTy->GetKind() == TypeKind::Struct) {

            // Even if the function uses optimized integer returning still need
            // to copy to the return address. If it does use optimized integer
            // returning the bitcasting will then occur at the LLFuncEndBB block.

            llvm::Value* LLToAddr = CFunc->UsesParamRetSlot
                                          ? GetElisionRetSlotAddr(CFunc)
                                          : LLRetAddr;
            
            if (CFunc->UsesOptimizedIntRet) {
                // TODO: This can actually be optimized further by checking if the thing being
                // returned is a struct initializer then instead of creating the struct
                // just generate it's integer value and store it directly into the return slot.
            
                llvm::Value* LLRetValue = GenNode(Ret->Value);
                
                // Bitcasting the return slot to the an integer address to store the optimized integer
                // return value.
                llvm::Type* LLRetTy = LLFunc->getReturnType();
                LLToAddr = Builder.CreateBitCast(LLToAddr, llvm::PointerType::get(LLRetTy, 0));
                
                Builder.CreateStore(GenReturnValueForOptimizedStructAsInt(LLRetValue), LLToAddr);
            }
            else if (Ret->Value->Is(AstKind::IDENT_REF) &&
                static_cast<IdentRef*>(Ret->Value)->Var->LLAddress == LLToAddr) {
                // The value is already in the return address.
                // This can happen because there was a locally defined variable that
                // was created but also an error was raised so there is multiple control
                // paths to the end of the function. Then the variable already points to
                // the return address.
                //
                // Ex.
                //
                //    fn foo(cond bool) A raises MyError {
                //
                //          a A;
                //          if cond {
                //              raise MyError{ };
                //          }
                //
                //          return a;
                //      }
            }
            // Checking for cases in which no no existing LValue exists and the value needs
            // to be passed to prevent irrelevent copies.
            // 
            // TODO: If we checked to make sure there are no lvalues in the ternary operator
            //       we could also get away with it here!
            else if (Ret->Value->Is(AstKind::STRUCT_INITIALIZER) ||
                     Ret->Value->Is(AstKind::FUNC_CALL)) {
                GenReturnByStoreToElisionRetSlot(Ret->Value, LLToAddr);
            }
            // Else the object needs to be copied/moved into the elision return address.
            else {
                llvm::Value* LLFromAddr = GenNode(Ret->Value);

                StructDecl* Struct = CFunc->RetTy->AsStructType()->GetStruct();
                CopyOrMoveStructObject(LLToAddr, LLFromAddr, Struct);
            }
        } else if (!Ret->Value && CFunc == Context.MainEntryFunc) {
            Builder.CreateStore(GetLLInt32(0), LLRetAddr);
        } else if (Ret->Value) {
            Builder.CreateStore(GenRValue(Ret->Value), LLRetAddr);
        }


        DestroyCurrentlyInitializedObjects();
        // DestroyAlwaysInitializedObjects is handled by the end block.
        // DestroyErrorControledObjects    is handled by the end block.

        Builder.CreateBr(LLFuncEndBB);
    }

    return nullptr;
}

llvm::Value* arco::IRGenerator::GenLoopControl(LoopControlStmt* LoopControl) {

    Scope* ScopeItr = LocScope;
    while (ScopeItr) {
        CallDestructors(ScopeItr->ObjectsNeedingDestroyed);

        if (ScopeItr->IsLoopScope) {
            break;
        }
        ScopeItr = ScopeItr->Parent;
    }

    if (LoopControl->Kind == AstKind::BREAK) {
        llvm::BasicBlock* LoopExit = LoopBreakStack[LoopBreakStack.size() - 1 - (LoopControl->LoopCount-1)];
        Builder.CreateBr(LoopExit);
    } else {
        llvm::BasicBlock* LoopRestart = LoopContinueStack[LoopBreakStack.size() - 1 - (LoopControl->LoopCount-1)];
        Builder.CreateBr(LoopRestart);
    }
    EMIT_DI(EmitDebugLocation(LoopControl));
    return nullptr;
}

llvm::Value* arco::IRGenerator::GenPredicateLoop(PredicateLoopStmt* Loop) {

    if (Loop->Cond && Loop->Cond->Is(AstKind::RANGE)) {
        return GenRangeExprLoop(static_cast<Range*>(Loop->Cond), Loop->Scope, nullptr);
    }

    llvm::BasicBlock* LLEndBB  = llvm::BasicBlock::Create(LLContext, "loop.end", LLFunc);
    llvm::BasicBlock* LLBodyBB = llvm::BasicBlock::Create(LLContext, "loop.body", LLFunc);
    llvm::BasicBlock* LLCondBB = llvm::BasicBlock::Create(LLContext, "loop.cond", LLFunc);
    
    LoopBreakStack.push_back(LLEndBB);
    LoopContinueStack.push_back(LLCondBB);

    // Generating the condition block
    EMIT_DI(GetDIEmitter()->EmitScopeStart(Loop->Scope.StartLoc));
    PUSH_SCOPE();
    LocScope->IsLoopScope = true;
    GenLoopCondJump(LLCondBB, LLBodyBB, LLEndBB, Loop->Cond);	
    EMIT_DI(EmitDebugLocation(Loop));

    GenBlock(LLBodyBB, Loop->Scope.Stmts);
    POP_SCOPE();

    LoopBreakStack.pop_back();
    LoopContinueStack.pop_back();

    // Unconditionally branch back to the condition block
    GenBranchIfNotTerm(LLCondBB);

    // Finally continuing forward into a new block after the loop
    GenBranchIfNotTerm(LLEndBB);
    Builder.SetInsertPoint(LLEndBB);

    EMIT_DI(GetDIEmitter()->EmitScopeEnd());
    
    return nullptr;
}

llvm::Value* arco::IRGenerator::GenRangeExprLoop(Range* Rg, LexScope& LScope, VarDecl* CaptureVar) {

    llvm::BasicBlock* LLEndBB      = llvm::BasicBlock::Create(LLContext, "loop.end", LLFunc);
    llvm::BasicBlock* LLBodyBB     = llvm::BasicBlock::Create(LLContext, "loop.body", LLFunc);
    llvm::BasicBlock* LLIncBB      = llvm::BasicBlock::Create(LLContext, "loop.inc", LLFunc);
    llvm::BasicBlock* LLCondBB     = llvm::BasicBlock::Create(LLContext, "loop.cond", LLFunc);
    llvm::BasicBlock* LLContinueBB = LLIncBB;

    LoopBreakStack.push_back(LLEndBB);
    LoopContinueStack.push_back(LLContinueBB);

    llvm::Value* LLIndex;
    if (CaptureVar) {
        LLIndex = CaptureVar->LLAddress;
    } else {
        LLIndex = CreateUnseenAlloca(GenType(Rg->Ty), "tmp.loop.index");
    }
    Builder.CreateStore(GenRValue(Rg->LHS), LLIndex);

    EMIT_DI(GetDIEmitter()->EmitScopeStart(LScope.StartLoc));
    PUSH_SCOPE();
    LocScope->IsLoopScope = true;
    
    // Generating the condition block
    Builder.CreateBr(LLCondBB);
    Builder.SetInsertPoint(LLCondBB);

    llvm::Value* LLIndexValue = CreateLoad(LLIndex);
    llvm::Value* LLRHS, *LLCond;
    if (Rg->Op == TokenKind::DOT_DOT_EQ) {
        //..=
        LLRHS = Builder.CreateAdd(GenRValue(Rg->RHS), GetOneValue(Rg->Ty), "one.more");
    } else {
        // ..<
        LLRHS = GenRValue(Rg->RHS);
    }
    // Use less than because RHS might start out less in some cases and we do
    // not want to infinite loop.
    if (Rg->Ty->IsSigned()) {
        LLCond = Builder.CreateICmpSLT(LLIndexValue, LLRHS);
    } else {
        LLCond = Builder.CreateICmpULT(LLIndexValue, LLRHS);
    }

    Builder.CreateCondBr(LLCond, LLBodyBB, LLEndBB);
    EMIT_DI(EmitDebugLocation(Rg));

    GenBlock(LLBodyBB, LScope.Stmts);
    POP_SCOPE();

    LoopBreakStack.pop_back();
    LoopContinueStack.pop_back();

    // Unconditionally branch back to the condition or inc. block
    // to restart the loop.
    GenBranchIfNotTerm(LLContinueBB);

    // Creating the code for the inc. block if needed
    if (LLIncBB) {

        Builder.SetInsertPoint(LLIncBB);
        
        // Increment the index.
        llvm::Value* LLIndexValue = CreateLoad(LLIndex);
        LLIndexValue = Builder.CreateAdd(LLIndexValue, GetOneValue(Rg->Ty), "inc");
        Builder.CreateStore(LLIndexValue, LLIndex);

        // Jumping directly into the loop condition
        Builder.CreateBr(LLCondBB); // No need to check for terminal since expressions cannot jump.
    }

    GenBranchIfNotTerm(LLEndBB);
    Builder.SetInsertPoint(LLEndBB);

    EMIT_DI(GetDIEmitter()->EmitScopeEnd());

    return nullptr;
}

llvm::Value* arco::IRGenerator::GenRangeLoop(RangeLoopStmt* Loop) {

    llvm::BasicBlock* LLEndBB  = llvm::BasicBlock::Create(LLContext, "loop.end", LLFunc);
    llvm::BasicBlock* LLBodyBB = llvm::BasicBlock::Create(LLContext, "loop.body", LLFunc);
    llvm::BasicBlock* LLIncBB  = !Loop->Incs.empty()
                                    ? llvm::BasicBlock::Create(LLContext, "loop.inc", LLFunc)
                                    : nullptr;
    llvm::BasicBlock* LLCondBB     = llvm::BasicBlock::Create(LLContext, "loop.cond", LLFunc);
    llvm::BasicBlock* LLContinueBB = LLIncBB ? LLIncBB : LLCondBB;

    LoopBreakStack.push_back(LLEndBB);
    LoopContinueStack.push_back(LLContinueBB);

    // Do not want to place these objects in the generation scope
    // because then they would be destroyed for every iteration of
    // the loop.
    for (AstNode* InitNode : Loop->InitNodes) {
        GenNode(InitNode);
    }

    EMIT_DI(GetDIEmitter()->EmitScopeStart(Loop->Scope.StartLoc));
    PUSH_SCOPE();
    LocScope->IsLoopScope = true;
    // Generating the condition block
    GenLoopCondJump(LLCondBB, LLBodyBB, LLEndBB, Loop->Cond);
    EMIT_DI(EmitDebugLocation(Loop));

    GenBlock(LLBodyBB, Loop->Scope.Stmts);
    POP_SCOPE();

    LoopBreakStack.pop_back();
    LoopContinueStack.pop_back();

    // Unconditionally branch back to the condition or inc. block
    // to restart the loop.
    GenBranchIfNotTerm(LLContinueBB);
    EMIT_DI(EmitDebugLocation(Loop->Scope.EndLoc));

    // Creating the code for the inc. block if needed
    if (LLIncBB) {

        Builder.SetInsertPoint(LLIncBB);

        for (Expr* Inc : Loop->Incs) {
            GenNode(Inc);
        }

        // Jumping directly into the loop condition
        Builder.CreateBr(LLCondBB); // No need to check for terminal since expressions cannot jump.
    }

    GenBranchIfNotTerm(LLEndBB);
    Builder.SetInsertPoint(LLEndBB);

    EMIT_DI(GetDIEmitter()->EmitScopeEnd());

    return nullptr;
}

llvm::Value* arco::IRGenerator::GenIteratorLoop(IteratorLoopStmt* Loop) {

    if (Loop->IterOnExpr->Is(AstKind::RANGE)) {
        return GenRangeExprLoop(static_cast<Range*>(Loop->IterOnExpr), Loop->Scope, Loop->VarVal);
    }

    llvm::BasicBlock* LLEndBB  = llvm::BasicBlock::Create(LLContext, "loop.end", LLFunc);
    llvm::BasicBlock* LLBodyBB = llvm::BasicBlock::Create(LLContext, "loop.body", LLFunc);
    llvm::BasicBlock* LLIncBB  = llvm::BasicBlock::Create(LLContext, "loop.inc", LLFunc);
    llvm::BasicBlock* LLCondBB = llvm::BasicBlock::Create(LLContext, "loop.cond", LLFunc);

    LoopBreakStack.push_back(LLEndBB);
    LoopContinueStack.push_back(LLIncBB);

    ContainerType* ContainerTy = Loop->IterOnExpr->Ty->AsContainerType();

    llvm::Value* LLArrItrPtrAddr = CreateUnseenAlloca(
        llvm::PointerType::get(GenType(ContainerTy->GetElementType()), 0), "arr.itr.ptr");

    llvm::Value* LLIterOnExpr = GenNode(Loop->IterOnExpr);
    llvm::Value* LLPtrToArrStart;
    llvm::Value* LLLength;
    if (ContainerTy->GetKind() == TypeKind::Array) {
        LLLength = GetSystemUInt(ContainerTy->AsArrayTy()->GetLength());
        LLPtrToArrStart = ArrayToPointer(LLIterOnExpr);
    } else {
        LLLength = CreateLoad(CreateStructGEP(LLIterOnExpr, 0));
        LLPtrToArrStart = CreateLoad(CreateStructGEP(LLIterOnExpr, 1));
    }
    
    llvm::Value* LLPtrToArrEnd = CreateInBoundsGEP(LLPtrToArrStart, { LLLength });
    LLPtrToArrEnd->setName("arr.itr.end");
    Builder.CreateStore(LLPtrToArrStart, LLArrItrPtrAddr);

    // Jumping directly into the loop condition
    Builder.CreateBr(LLCondBB);
    Builder.SetInsertPoint(LLCondBB);

    // Keep going until end of array
    llvm::Value* LLCond = Builder.CreateICmpNE(CreateLoad(LLArrItrPtrAddr), LLPtrToArrEnd);
    Builder.CreateCondBr(LLCond, LLBodyBB, LLEndBB);
    EMIT_DI(EmitDebugLocation(Loop));
    EMIT_DI(GetDIEmitter()->EmitScopeStart(Loop->Scope.StartLoc));

    GenBranchIfNotTerm(LLBodyBB);
    Builder.SetInsertPoint(LLBodyBB);

    // TODO: Optimize, can't this just have the variable's address be what indexes instead of needing
    // a seperate pointer to point to the array and iterator when working with pointer types?

    // TODO: Optimize this for storing arrays so that it doesn't copy then entire array
    //       every iteration if iterating on arrays.
    // Storing into the variable
    llvm::Value* LLArrPtrValue = CreateLoad(LLArrItrPtrAddr);
    LLArrPtrValue = LoadIteratorLoopValueIfNeeded(LLArrPtrValue, Loop->VarVal->Ty, ContainerTy->GetElementType());
    // TODO: Doesn't this need to cast since SemAnalysis uses IsAssignableTo?
    
    Builder.CreateStore(LLArrPtrValue, Loop->VarVal->LLAddress);

    PUSH_SCOPE();
    LocScope->IsLoopScope = true;
    GenBlock(nullptr, Loop->Scope.Stmts);
    POP_SCOPE();

    LoopBreakStack.pop_back();
    LoopContinueStack.pop_back();

    // Jump back to the continue block to restart the loop
    GenBranchIfNotTerm(LLIncBB);

    // Incrementing the array pointer
    Builder.SetInsertPoint(LLIncBB);
    EMIT_DI(EmitDebugLocation(Loop->Scope.EndLoc));

    llvm::Value* LLArrItrPtr = CreateLoad(LLArrItrPtrAddr);
    llvm::Value* LLNextPtr = CreateInBoundsGEP(LLArrItrPtr, { GetSystemUInt(1) });
    Builder.CreateStore(LLNextPtr, LLArrItrPtrAddr);

    // Jumping directly into the loop condition
    Builder.CreateBr(LLCondBB);

    // Finally continuing forward into a new block after the loop
    GenBranchIfNotTerm(LLEndBB);
    Builder.SetInsertPoint(LLEndBB);

    EMIT_DI(GetDIEmitter()->EmitScopeEnd());

    return nullptr;
}

llvm::Value* arco::IRGenerator::LoadIteratorLoopValueIfNeeded(llvm::Value* LLValuePtr, Type* ToTy, Type* FromTy) {
    bool NonPtrAssignment = false;
    if (ToTy->GetKind() == TypeKind::Pointer) {
        PointerType* VarPtrTy = ToTy->AsPointerTy();

        // a int*[4] = [ 4, 52, 2 ];
        // loop i int* : a { .. }

        if (VarPtrTy->Equals(FromTy)) {
            NonPtrAssignment = true;
        } // otherwise effectively retreiving the index by reference.
    } else {
        NonPtrAssignment = true;
    }
    if (NonPtrAssignment) {
        LLValuePtr = CreateLoad(LLValuePtr);

        if (!ToTy->Equals(FromTy)) {
            // Well it is not generated by a rvalue so we have to cast here.
            LLValuePtr = GenCast(ToTy, FromTy, LLValuePtr);
        }
    }
    return LLValuePtr;
}

llvm::Value* arco::IRGenerator::GenDelete(DeleteStmt* Delete) {
    llvm::Value* LLValue = GenRValue(Delete->Value);

    llvm::Value* LLFree = llvm::CallInst::CreateFree(LLValue, Builder.GetInsertBlock());
    Builder.Insert(LLFree);
    EMIT_DI(EmitDebugLocation(Delete));
    return nullptr;
}

llvm::Value* arco::IRGenerator::GenIf(IfStmt* If) {
    llvm::BasicBlock* LLThenBB = llvm::BasicBlock::Create(LLContext, "if.then", LLFunc);
    llvm::BasicBlock* LLEndBB  = llvm::BasicBlock::Create(LLContext, "if.end", LLFunc);
    llvm::BasicBlock* LLElseBB = LLEndBB;
    if (If->Else) {
        LLElseBB = llvm::BasicBlock::Create(LLContext, "if.else", LLFunc);
    }

    EMIT_DI(GetDIEmitter()->EmitScopeStart(CFunc->Scope.StartLoc));
    PUSH_SCOPE();
    GenBranchOnCond(If->Cond, LLThenBB, LLElseBB);
    EMIT_DI(EmitDebugLocation(If));

    GenBlock(LLThenBB, If->Scope.Stmts);
    POP_SCOPE();

    // Jump out of the body of the if statement
    GenBranchIfNotTerm(LLEndBB);
    EMIT_DI(GetDIEmitter()->EmitScopeEnd());

    // Generating the else statement if it exist
    if (AstNode* Else = If->Else) {
        Builder.SetInsertPoint(LLElseBB);
        GenNode(Else); // Pushes and pops its own scope.
    }

    // Finally continuing forward into a new block after the if
    GenBranchIfNotTerm(LLEndBB);
    Builder.SetInsertPoint(LLEndBB);

    return nullptr;
}

llvm::Value* arco::IRGenerator::GenNestedScope(NestedScopeStmt* NestedScope) {
    EMIT_DI(GetDIEmitter()->EmitScopeStart(CFunc->Loc));
    PUSH_SCOPE();
    GenBlock(nullptr, NestedScope->Scope.Stmts);
    POP_SCOPE();
    EMIT_DI(GetDIEmitter()->EmitScopeEnd());
    return nullptr;
}

llvm::Value* arco::IRGenerator::GenRaise(RaiseStmt* Raise) {
    EncounteredReturn = true;

    // TODO: Deal with nonsense involving data structures that do not have
    // default constructores.

    llvm::Value* LLStructErrorAddr    = GetErrorRetAddr(1 + Raise->RaisedIdx);
    llvm::Value* LLErrorInterfaceAddr = GetErrorRetAddr(0);
    GenStructInitializer(Raise->StructInit, LLStructErrorAddr);
    
    PointerType* ErrorInterfacePtrTy = Context.ErrorInterfacePtrType;
    StructDecl* Struct = Raise->StructInit->Ty->AsStructType()->GetStruct();
    
    llvm::Value* LLErrorInterface =
        GenCastToInterface(ErrorInterfacePtrTy, Struct, LLStructErrorAddr, LLErrorInterfaceAddr->getType()->getPointerElementType());
    Builder.CreateStore(LLErrorInterface, LLErrorInterfaceAddr);

    EMIT_DI(EmitDebugLocation(Raise));

    GenRaiseReturnZeroedValue();
    
    return nullptr;
}

void arco::IRGenerator::GenRaiseReturnZeroedValue() {
    
    if (FieldInitializingIdx != -1 && FieldInitializingIdx >  0) {
        // Oh god we are initializing a constructor we have to roll back the values
        // that were initialized.
        StructDecl* Struct = CFunc->Struct;
        for (ulen i = 0; i <= FieldInitializingIdx - 1; i++) {
            VarDecl* Field = Struct->Fields[i];
            if (Field->Ty->TypeNeedsDestruction()) {
                CallDestructors(Field->Ty, CreateStructGEP(LLThis, i), nullptr);
            }
        }
    }

    if (!LLFuncEndBB) {
        // There is no return statement at all so the raised error may
        // just return.
        llvm::Value* LLRetValue = nullptr;
        if (CFunc->RetTy->GetKind() == TypeKind::Struct) {
            if (CFunc->UsesOptimizedIntRet) {
                ulen SizeInBytes = SizeOfTypeInBytes(GenType(CFunc->RetTy));
                LLRetValue = llvm::ConstantInt::get(
                    llvm::IntegerType::get(LLContext, SizeInBytes * 8), 0);
                Builder.CreateRet(LLRetValue);
            }
            // else the struct is being passed in as a parameter so
            // there is no reason to return anything.
        } else if (CFunc == Context.MainEntryFunc) {
            // Default to returning zero for the main function.
            LLRetValue = GetLLInt32(0);
        } else if (CFunc->RetTy->GetKind() != TypeKind::Void) {
            LLRetValue = GenZeroedValue(CFunc->RetTy);
        }

        // Destroy all objects that have been generated up to
        // this point.
        CallDestructors(AlwaysInitializedDestroyedObjects);
        DestroyCurrentlyInitializedObjects();

        if (LLRetValue) {
            Builder.CreateRet(LLRetValue);
        } else {
            Builder.CreateRetVoid();
        }

    } else {
        // No need to set the return address there is an error so it can be garbage.

        if (CFunc->NumReturns == 1 && CFunc->UsesParamRetSlot && LocalReturnVar) {
            // A locally returned variable has been initialized but since the caller
            // does not destroy the object due to the error produced the variable
            // has to be destroyed before returning.
            if (LocalReturnVar->Ty->TypeNeedsDestruction()) {
                CallDestructors(LocalReturnVar->Ty, LocalReturnVar->LLAddress, nullptr);
            }
        }

        DestroyCurrentlyInitializedObjects();
        // DestroyAlwaysInitializedObjects is handled by the end block.

        Builder.CreateBr(LLFuncEndBB);
    }
}

//===-------------------------------===//
// Expressions
//===-------------------------------===//

llvm::Value* arco::IRGenerator::GenBinaryOp(BinaryOp* BinOp) {
    switch (BinOp->Op) {
    case '=': {
        llvm::Value* LLAddress = GenNode(BinOp->LHS);
        GenAssignment(LLAddress, BinOp->LHS->Ty, BinOp->RHS, BinOp->RHS->HasConstAddress, true);
        EMIT_DI(EmitDebugLocation(BinOp));
        return LLAddress;
    }
    //
    // Arithmetic
    //
    case '+': {
        llvm::Value* LLLHS = GenRValue(BinOp->LHS);
        llvm::Value* LLRHS = GenRValue(BinOp->RHS);
        if (BinOp->Ty->IsPointer()) {
            // Pointer arithmetic

            llvm::Value* PtrAddr, *Add;
            bool LArray = BinOp->LHS->Ty->GetKind() == TypeKind::Array;
            bool RArray = BinOp->RHS->Ty->GetKind() == TypeKind::Array;

            if (LArray || RArray) {
                PtrAddr = LArray ? ArrayToPointer(LLLHS) : LLRHS;
                Add     = LArray ? LLRHS : LLLHS;
            } else {
                PtrAddr = BinOp->LHS->Ty->IsPointer() ? LLLHS : LLRHS;
                Add     = BinOp->LHS->Ty->IsPointer() ? LLRHS : LLLHS;
            }
 
            return CreateInBoundsGEP(PtrAddr, { Add });
        } else {
            return GenAdd(LLLHS, LLRHS, BinOp->Ty);
        }
    }
    case '-': {
        llvm::Value* LLLHS = GenRValue(BinOp->LHS);
        llvm::Value* LLRHS = GenRValue(BinOp->RHS);

        if (BinOp->Ty->IsPointer()) {

            llvm::Value* PtrAddr = LLLHS;
            if (BinOp->LHS->Ty->GetKind() == TypeKind::Array) {
                PtrAddr = ArrayToPointer(PtrAddr);
            }

            llvm::Value* Sub = Builder.CreateNeg(LLRHS);
            return CreateInBoundsGEP(PtrAddr, { Sub });
        } else {
            return GenSub(LLLHS, LLRHS, BinOp->Ty);
        }
    }
    case '*': {
        llvm::Value* LLLHS = GenRValue(BinOp->LHS);
        llvm::Value* LLRHS = GenRValue(BinOp->RHS);

        return GenMul(LLLHS, LLRHS, BinOp->Ty);
    }
    case '/': {
        llvm::Value* LLLHS = GenRValue(BinOp->LHS);
        llvm::Value* LLRHS = GenRValue(BinOp->RHS);

        return GenDiv(LLLHS, LLRHS, BinOp->Ty);
    }
    case '%': {
        llvm::Value* LLLHS = GenRValue(BinOp->LHS);
        llvm::Value* LLRHS = GenRValue(BinOp->RHS);

        if (BinOp->Ty->IsSigned()) {
            return Builder.CreateSRem(LLLHS, LLRHS);
        }
        return Builder.CreateURem(LLLHS, LLRHS);
    }
    case TokenKind::PLUS_EQ: { // +=
        llvm::Value* LLLHS = GenNode(BinOp->LHS);
        llvm::Value* LLRHS = GenRValue(BinOp->RHS);

        if (BinOp->Ty->IsPointer()) {
            // Pointer arithmetic
            llvm::Value* V = CreateInBoundsGEP(CreateLoad(LLLHS), { LLRHS });
            Builder.CreateStore(V, LLLHS);
            EMIT_DI(EmitDebugLocation(BinOp));
            return V;
        } else {
            llvm::Value* LLLHSRV = CreateLoad(LLLHS);
            llvm::Value* V = BinOp->Ty->IsInt() ? Builder.CreateAdd(LLLHSRV, LLRHS)
                                                : Builder.CreateFAdd(LLLHSRV, LLRHS);
            Builder.CreateStore(V, LLLHS);
            EMIT_DI(EmitDebugLocation(BinOp));
            return V;
        }
    }
    case TokenKind::MINUS_EQ: { // -=
        llvm::Value* LLLHS = GenNode(BinOp->LHS);
        llvm::Value* LLRHS = GenRValue(BinOp->RHS);

        if (BinOp->Ty->IsPointer()) {
            llvm::Value* V = CreateInBoundsGEP(CreateLoad(LLLHS), { Builder.CreateNeg(LLRHS) });
            Builder.CreateStore(V, LLLHS);
            EMIT_DI(EmitDebugLocation(BinOp));
            return V;
        } else {
            llvm::Value* LLLHSRV = CreateLoad(LLLHS);
            llvm::Value* V = BinOp->Ty->IsInt() ? Builder.CreateSub(LLLHSRV, LLRHS)
                                                : Builder.CreateFSub(LLLHSRV, LLRHS);
            Builder.CreateStore(V, LLLHS);
            EMIT_DI(EmitDebugLocation(BinOp));
            return V;
        }
    }
    case TokenKind::STAR_EQ: { // *=
        llvm::Value* LLLHS = GenNode(BinOp->LHS);
        llvm::Value* LLRHS = GenRValue(BinOp->RHS);
        llvm::Value* LLLHSRV = CreateLoad(LLLHS);
        llvm::Value* V = BinOp->Ty->IsInt() ? Builder.CreateMul(LLLHSRV, LLRHS)
                                            : Builder.CreateFMul(LLLHSRV, LLRHS);
        Builder.CreateStore(V, LLLHS);
        EMIT_DI(EmitDebugLocation(BinOp));
        return V;
    }
    case TokenKind::SLASH_EQ: { // /=
        llvm::Value* LLLHS = GenNode(BinOp->LHS);
        llvm::Value* LLRHS = GenRValue(BinOp->RHS);
        llvm::Value* LLLHSRV = CreateLoad(LLLHS);
        llvm::Value* V;
        if (BinOp->Ty->IsInt()) {
            V = BinOp->Ty->IsSigned() ? Builder.CreateSDiv(LLLHSRV, LLRHS)
                                      : Builder.CreateUDiv(LLLHSRV, LLRHS);
        } else {
            V = Builder.CreateFDiv(LLLHSRV, LLRHS);
        }
        Builder.CreateStore(V, LLLHS);
        EMIT_DI(EmitDebugLocation(BinOp));
        return V;
    }
    case TokenKind::MOD_EQ: { // %=
        llvm::Value* LLLHS = GenNode(BinOp->LHS);
        llvm::Value* LLRHS = GenRValue(BinOp->RHS);
        llvm::Value* LLLHSRV = CreateLoad(LLLHS);
        llvm::Value* V = BinOp->Ty->IsSigned() ? Builder.CreateSRem(LLLHSRV, LLRHS)
                                               : Builder.CreateURem(LLLHSRV, LLRHS);
        Builder.CreateStore(V, LLLHS);
        EMIT_DI(EmitDebugLocation(BinOp));
        return V;
    }
    //
    // Bitwise
    //
    case '&': {
        llvm::Value* LLLHS = GenRValue(BinOp->LHS);
        llvm::Value* LLRHS = GenRValue(BinOp->RHS);
        return Builder.CreateAnd(LLLHS, LLRHS);
    }
    case '^': {
        llvm::Value* LLLHS = GenRValue(BinOp->LHS);
        llvm::Value* LLRHS = GenRValue(BinOp->RHS);
        return Builder.CreateXor(LLLHS, LLRHS);
    }
    case '|': {
        llvm::Value* LLLHS = GenRValue(BinOp->LHS);
        llvm::Value* LLRHS = GenRValue(BinOp->RHS);
        llvm::Value* LLValue = Builder.CreateOr(LLLHS, LLRHS);
        return LLValue;
    }
    case TokenKind::LT_LT: { // <<
        llvm::Value* LLLHS = GenRValue(BinOp->LHS);
        llvm::Value* LLRHS = GenRValue(BinOp->RHS);
        llvm::Value* LLValue = Builder.CreateShl(LLLHS, LLRHS);
        return LLValue;
    }
    case TokenKind::GT_GT: { // >>
        llvm::Value* LLLHS = GenRValue(BinOp->LHS);
        llvm::Value* LLRHS = GenRValue(BinOp->RHS);
        if (BinOp->Ty->IsSigned()) {
            // Arithmetic shift so that the sign is considered
            // seperately from the bits.
            return Builder.CreateAShr(LLLHS, LLRHS);
        } else {
            return Builder.CreateLShr(LLLHS, LLRHS);
        }
    }
    case TokenKind::AMP_EQ: { // &=
        llvm::Value* LLLHS = GenNode(BinOp->LHS);
        llvm::Value* LLRHS = GenRValue(BinOp->RHS);
        llvm::Value* LLLHSRV = CreateLoad(LLLHS);
        llvm::Value* V = Builder.CreateAnd(LLLHSRV, LLRHS);
        Builder.CreateStore(V, LLLHS);
        EMIT_DI(EmitDebugLocation(BinOp));
        return V;
    }
    case TokenKind::CRT_EQ: { // ^=
        llvm::Value* LLLHS = GenNode(BinOp->LHS);
        llvm::Value* LLRHS = GenRValue(BinOp->RHS);
        llvm::Value* LLLHSRV = CreateLoad(LLLHS);
        llvm::Value* V = Builder.CreateXor(LLLHSRV, LLRHS);
        Builder.CreateStore(V, LLLHS);
        EMIT_DI(EmitDebugLocation(BinOp));
        return V;
    }
    case TokenKind::BAR_EQ: { // |=
        llvm::Value* LLLHS = GenNode(BinOp->LHS);
        llvm::Value* LLRHS = GenRValue(BinOp->RHS);
        llvm::Value* LLLHSRV = CreateLoad(LLLHS);
        llvm::Value* V = Builder.CreateOr(LLLHSRV, LLRHS);
        Builder.CreateStore(V, LLLHS);
        EMIT_DI(EmitDebugLocation(BinOp));
        return V;
    }
    case TokenKind::LT_LT_EQ: { // <<=
        llvm::Value* LLLHS = GenNode(BinOp->LHS);
        llvm::Value* LLRHS = GenRValue(BinOp->RHS);
        llvm::Value* LLLHSRV = CreateLoad(LLLHS);
        llvm::Value* V = Builder.CreateShl(LLLHSRV, LLRHS);
        Builder.CreateStore(V, LLLHS);
        EMIT_DI(EmitDebugLocation(BinOp));
        return V;
    }
    case TokenKind::GT_GT_EQ: { // >>=
        llvm::Value* LLLHS = GenNode(BinOp->LHS);
        llvm::Value* LLRHS = GenRValue(BinOp->RHS);
        llvm::Value* LLLHSRV = CreateLoad(LLLHS);
        
        llvm::Value* V;
        if (BinOp->Ty->IsSigned()) {
            // Arithmetic shift so that the sign is considered
            // seperately from the bits.
            V = Builder.CreateAShr(LLLHSRV, LLRHS);
        } else {
            V = Builder.CreateLShr(LLLHSRV, LLRHS);
        }
        Builder.CreateStore(V, LLLHS);
        EMIT_DI(EmitDebugLocation(BinOp));
        return V;
    }
    //
    // Conditionals
    //
    case TokenKind::EQ_EQ: {
        TypeKind LK = BinOp->LHS->Ty->GetKind();
        TypeKind RK = BinOp->RHS->Ty->GetKind();
        if (LK == TypeKind::Null && RK == TypeKind::Null) {
            return llvm::ConstantInt::getTrue(LLContext);
        } else if (LK == TypeKind::Float32 || LK == TypeKind::Float64 ||
                   RK == TypeKind::Float32 || RK == TypeKind::Float64) {
            llvm::Value* LLLHS = GenRValue(BinOp->LHS);
            llvm::Value* LLRHS = GenRValue(BinOp->RHS);
            return Builder.CreateFCmpUEQ(LLLHS, LLRHS);
        } else if (LK == TypeKind::StructRef || RK == TypeKind::StructRef) {
            InterfaceDecl* Interface = LK != TypeKind::StructRef
                                          ? BinOp->LHS->Ty->AsPointerTy()->GetElementType()->AsStructType()->GetInterface()
                                          : BinOp->RHS->Ty->AsPointerTy()->GetElementType()->AsStructType()->GetInterface();
            StructDecl* Struct = LK == TypeKind::StructRef
                                     ? static_cast<IdentRef*>(BinOp->LHS)->Struct
                                     : static_cast<IdentRef*>(BinOp->RHS)->Struct;
            llvm::Value* LLInterfacePtr = LK != TypeKind::StructRef
                                             ? GenRValue(BinOp->LHS)
                                             : GenRValue(BinOp->RHS);
            FuncDecl* FirstInterfaceFunc = Interface->Funcs[0];
            FuncDecl* MappedFunc = GetMappedInterfaceFunc(FirstInterfaceFunc, Struct->Funcs[FirstInterfaceFunc->Name]);
            GenFuncDecl(MappedFunc);
            llvm::Function* LLMappedFunc = MappedFunc->GetLLFunction();

            // NOTE: since the ptr into the vtable is what is set to null there becomes a problem where
            //       we try and load the pointer into the vtable to get the function pointer back out but
            //       it is null so it tries to load a null address and fails.
            // 
            //   To fix this an additional branch is added to check to see if the address into the vtable
            //   is null.
            // 
            // i8* (i8*)***  <= address of stack struct offset by vtable ptr
            // i8* (i8*)**   <= address of ptr into vtable
            // i8* (i8*)*    <= the actual function pointer the thing we care about comparing
            
            llvm::BasicBlock* LLStartBlock = Builder.GetInsertBlock();
            llvm::BasicBlock* LLLHSTrue = llvm::BasicBlock::Create(LLContext, "and.lhs.true", LLFunc);
            llvm::BasicBlock* LLThen    = llvm::BasicBlock::Create(LLContext, "if.then", LLFunc);
            llvm::BasicBlock* LLEndBB   = llvm::BasicBlock::Create(LLContext, "and.end", LLFunc);
            
            Builder.CreateCondBr(Builder.CreateIsNotNull(LLInterfacePtr), LLLHSTrue, LLEndBB);
            Builder.SetInsertPoint(LLLHSTrue);

            llvm::Value* LLLHSTruth;
            if (Interface->NumFuncs == 1) {
                // Quick pass where we just load the function pointer immediately.
                LLLHSTruth = Builder.CreateICmpEQ(LLMappedFunc, CreateLoad(LLInterfacePtr));
            } else {
                // Quick pass where we just load the function pointer immediately.
                LLLHSTruth = Builder.CreateICmpEQ(Builder.CreateBitCast(LLMappedFunc, llvm::Type::getInt8PtrTy(LLContext)),
                    CreateLoad(CreateLoad(LLInterfacePtr)));
            }
            
            Builder.CreateBr(LLEndBB);
            Builder.SetInsertPoint(LLEndBB);

            llvm::PHINode* LLResPHINode = llvm::PHINode::Create(
                llvm::Type::getInt1Ty(LLContext), 2, "cond.res", LLEndBB);
            LLResPHINode->addIncoming(llvm::ConstantInt::getFalse(LLContext), LLStartBlock);
            LLResPHINode->addIncoming(LLLHSTruth, LLLHSTrue);

            return LLResPHINode;
        } else {
            llvm::Value* LLLHS = GenRValue(BinOp->LHS);
            llvm::Value* LLRHS = GenRValue(BinOp->RHS);
            return Builder.CreateICmpEQ(LLLHS, LLRHS);
        }
    }
    case TokenKind::EXL_EQ: {
        TypeKind LK = BinOp->LHS->Ty->GetKind();
        TypeKind RK = BinOp->RHS->Ty->GetKind();
        if (LK == TypeKind::Null && RK == TypeKind::Null) {
            return llvm::ConstantInt::getFalse(LLContext);
        } else if (LK == TypeKind::Float32 || LK == TypeKind::Float64 ||
                   RK == TypeKind::Float32 || RK == TypeKind::Float64) {
            llvm::Value* LLLHS = GenRValue(BinOp->LHS);
            llvm::Value* LLRHS = GenRValue(BinOp->RHS);
            return Builder.CreateFCmpUNE(LLLHS, LLRHS);
        } else if (LK == TypeKind::StructRef || RK == TypeKind::StructRef) {
            InterfaceDecl* Interface = LK != TypeKind::StructRef
                                          ? BinOp->LHS->Ty->AsPointerTy()->GetElementType()->AsStructType()->GetInterface()
                                          : BinOp->RHS->Ty->AsPointerTy()->GetElementType()->AsStructType()->GetInterface();
            StructDecl* Struct = LK == TypeKind::StructRef
                                     ? static_cast<IdentRef*>(BinOp->LHS)->Struct
                                     : static_cast<IdentRef*>(BinOp->RHS)->Struct;
            llvm::Value* LLInterfacePtr = LK != TypeKind::StructRef
                                             ? GenRValue(BinOp->LHS)
                                             : GenRValue(BinOp->RHS);
            FuncDecl* FirstInterfaceFunc = Interface->Funcs[0];
            FuncDecl* MappedFunc = GetMappedInterfaceFunc(FirstInterfaceFunc, Struct->Funcs[FirstInterfaceFunc->Name]);
            GenFuncDecl(MappedFunc);
            llvm::Function* LLMappedFunc = MappedFunc->GetLLFunction();

            // Read comment under EQ_EQ for an explaination for the branching.

            llvm::BasicBlock* LLStartBlock = Builder.GetInsertBlock();
            llvm::BasicBlock* LLLHSFalse = llvm::BasicBlock::Create(LLContext, "or.lhs.false", LLFunc);
            llvm::BasicBlock* LLThen     = llvm::BasicBlock::Create(LLContext, "if.then", LLFunc);
            llvm::BasicBlock* LLEndBB    = llvm::BasicBlock::Create(LLContext, "and.end", LLFunc);
            
            Builder.CreateCondBr(Builder.CreateIsNull(LLInterfacePtr), LLEndBB, LLLHSFalse);
            Builder.SetInsertPoint(LLLHSFalse);

            llvm::Value* LLLHSTruth;
            if (Interface->NumFuncs == 1) {
                // Quick pass where we just load the function pointer immediately.
                LLLHSTruth = Builder.CreateICmpNE(LLMappedFunc, CreateLoad(LLInterfacePtr));
            } else {
                // Quick pass where we just load the function pointer immediately.
                LLLHSTruth = Builder.CreateICmpNE(Builder.CreateBitCast(LLMappedFunc, llvm::Type::getInt8PtrTy(LLContext)),
                                            CreateLoad(CreateLoad(LLInterfacePtr)));
            }

            Builder.CreateBr(LLEndBB);
            Builder.SetInsertPoint(LLEndBB);

            llvm::PHINode* LLResPHINode = llvm::PHINode::Create(
                llvm::Type::getInt1Ty(LLContext), 2, "cond.res", LLEndBB);
            LLResPHINode->addIncoming(llvm::ConstantInt::getTrue(LLContext), LLStartBlock);
            LLResPHINode->addIncoming(LLLHSTruth, LLLHSFalse);

            return LLResPHINode;
        } else {
            llvm::Value* LLLHS = GenRValue(BinOp->LHS);
            llvm::Value* LLRHS = GenRValue(BinOp->RHS);
            return Builder.CreateICmpNE(LLLHS, LLRHS);
        }
    }
    case '<': {
        TypeKind LK = BinOp->LHS->Ty->GetKind();
        TypeKind RK = BinOp->RHS->Ty->GetKind();
        if (LK == TypeKind::Null && RK == TypeKind::Null) {
            return llvm::ConstantInt::getFalse(LLContext);
        } else if (LK == TypeKind::Float32 || LK == TypeKind::Float64 ||
                   RK == TypeKind::Float32 || RK == TypeKind::Float64) {
            llvm::Value* LLLHS = GenRValue(BinOp->LHS);
            llvm::Value* LLRHS = GenRValue(BinOp->RHS);
            return Builder.CreateFCmpULT(LLLHS, LLRHS);
        } else {
            llvm::Value* LLLHS = GenRValue(BinOp->LHS);
            llvm::Value* LLRHS = GenRValue(BinOp->RHS);

            if (BinOp->ResultType->IsSigned()) {
                return Builder.CreateICmpSLT(LLLHS, LLRHS);
            } else {
                return Builder.CreateICmpULT(LLLHS, LLRHS);
            }
        }
    }
    case '>': {
        TypeKind LK = BinOp->LHS->Ty->GetKind();
        TypeKind RK = BinOp->RHS->Ty->GetKind();
        if (LK == TypeKind::Null && RK == TypeKind::Null) {
            return llvm::ConstantInt::getFalse(LLContext);
        } else if (LK == TypeKind::Float32 || LK == TypeKind::Float64 ||
                   RK == TypeKind::Float32 || RK == TypeKind::Float64) {
            llvm::Value* LLLHS = GenRValue(BinOp->LHS);
            llvm::Value* LLRHS = GenRValue(BinOp->RHS);
            return Builder.CreateFCmpUGT(LLLHS, LLRHS);
        } else {
            llvm::Value* LLLHS = GenRValue(BinOp->LHS);
            llvm::Value* LLRHS = GenRValue(BinOp->RHS);

            if (BinOp->ResultType->IsSigned()) {
                return Builder.CreateICmpSGT(LLLHS, LLRHS);
            } else {
                return Builder.CreateICmpUGT(LLLHS, LLRHS);
            }	
        }
    }
    case TokenKind::LT_EQ: {
        TypeKind LK = BinOp->LHS->Ty->GetKind();
        TypeKind RK = BinOp->RHS->Ty->GetKind();
        if (LK == TypeKind::Null && RK == TypeKind::Null) {
            return llvm::ConstantInt::getTrue(LLContext);
        } else if (LK == TypeKind::Float32 || LK == TypeKind::Float64 ||
                   RK == TypeKind::Float32 || RK == TypeKind::Float64) {
            llvm::Value* LLLHS = GenRValue(BinOp->LHS);
            llvm::Value* LLRHS = GenRValue(BinOp->RHS);
            return Builder.CreateFCmpULE(LLLHS, LLRHS);
        } else {
            llvm::Value* LLLHS = GenRValue(BinOp->LHS);
            llvm::Value* LLRHS = GenRValue(BinOp->RHS);

            if (BinOp->ResultType->IsSigned()) {
                return Builder.CreateICmpSLE(LLLHS, LLRHS);
            } else {
                return Builder.CreateICmpULE(LLLHS, LLRHS);
            }	
        }
    }
    case TokenKind::GT_EQ: {
        TypeKind LK = BinOp->LHS->Ty->GetKind();
        TypeKind RK = BinOp->RHS->Ty->GetKind();
        if (LK == TypeKind::Null && RK == TypeKind::Null) {
            return llvm::ConstantInt::getTrue(LLContext);
        } else if (LK == TypeKind::Float32 || LK == TypeKind::Float64 ||
                   RK == TypeKind::Float32 || RK == TypeKind::Float64) {
            llvm::Value* LLLHS = GenRValue(BinOp->LHS);
            llvm::Value* LLRHS = GenRValue(BinOp->RHS);
            return Builder.CreateFCmpUGE(LLLHS, LLRHS);
        } else {
            llvm::Value* LLLHS = GenRValue(BinOp->LHS);
            llvm::Value* LLRHS = GenRValue(BinOp->RHS);

            if (BinOp->ResultType->IsSigned()) {
                return Builder.CreateICmpSGE(LLLHS, LLRHS);
            } else {
                return Builder.CreateICmpUGE(LLLHS, LLRHS);
            }	
        }
    }
    case TokenKind::AMP_AMP: { // &&
        if (BinOp->IsFoldable) {
            llvm::ConstantInt* LLLHS = llvm::cast<llvm::ConstantInt>(GenCond(BinOp->LHS));
            llvm::ConstantInt* LLRHS = llvm::cast<llvm::ConstantInt>(GenCond(BinOp->RHS));
            if (LLLHS->isOne() && LLRHS->isOne()) {
                return llvm::ConstantInt::getTrue(LLContext);
            } else {
                return llvm::ConstantInt::getFalse(LLContext);
            }
        }

        // See: https://github.com/llvm/llvm-project/blob/839ac62c5085d895d3165bc5024db623a7a78813/clang/lib/CodeGen/CGExprScalar.cpp
        // VisitBinLAnd

        // ... We gen the nodes lower on the tree first
        //     then after returning back up from the
        //     children node emission we calculate the very
        //     last conidition which gets fed to the PHI node.
        //
        //     It only reaches this last condition block if it
        //     suceeded all children LHS of '&&' operator.

        //   P1 && P2 && P3
        //
        //
        //           &&
        //          /  \
        //         &&   P3  <- Only evaluted in the end after children.
        //        /  \
        //       P1   P2

        llvm::BasicBlock* LLEndBB     = llvm::BasicBlock::Create(LLContext, "and.end", LLFunc);
        llvm::BasicBlock* LLLHSTrueBB = llvm::BasicBlock::Create(LLContext, "and.lhs.true", LLFunc);
        
        // Generate children
        GenBranchOnCond(BinOp->LHS, LLLHSTrueBB, LLEndBB);

        // All children blocks result in false if they
        // arrive from those blocks.
        llvm::PHINode* LLResPHINode = llvm::PHINode::Create(
            llvm::Type::getInt1Ty(LLContext), 2 /* At least 2 but can add more */,
            "cond.res", LLEndBB);
        for (llvm::pred_iterator PI = llvm::pred_begin(LLEndBB),
                                 PE = llvm::pred_end(LLEndBB);
            PI != PE; ++PI) {
            LLResPHINode->addIncoming(llvm::ConstantInt::getFalse(LLContext), *PI);
        }

        // Now dealing with the final RHS.
        // Basically if we made it here there is
        // only one condition left to check!
        Builder.SetInsertPoint(LLLHSTrueBB);
        llvm::Value* LLRHSCondV = GenCond(BinOp->RHS);
        // Need to re-obtain the last block since the condiition might have
        // added more blocks.
        LLLHSTrueBB = Builder.GetInsertBlock();
        
        Builder.CreateBr(LLEndBB);
        Builder.SetInsertPoint(LLEndBB);
        LLResPHINode->addIncoming(LLRHSCondV, LLLHSTrueBB);

        return LLResPHINode;
    }
    case TokenKind::BAR_BAR: { // ||
        if (BinOp->IsFoldable) {
            llvm::ConstantInt* LLLHS = llvm::cast<llvm::ConstantInt>(GenCond(BinOp->LHS));
            llvm::ConstantInt* LLRHS = llvm::cast<llvm::ConstantInt>(GenCond(BinOp->RHS));
            if (LLLHS->isOne() || LLRHS->isOne()) {
                return llvm::ConstantInt::getTrue(LLContext);
            } else {
                return llvm::ConstantInt::getFalse(LLContext);
            }
        }

        llvm::BasicBlock* LLEndBB      = llvm::BasicBlock::Create(LLContext, "or.end", LLFunc);
        llvm::BasicBlock* LLLHSFalseBB = llvm::BasicBlock::Create(LLContext, "or.lhs.false", LLFunc);

        // Generate children
        GenBranchOnCond(BinOp->LHS, LLEndBB, LLLHSFalseBB);

        llvm::PHINode* LLResPHINode = llvm::PHINode::Create(
            llvm::Type::getInt1Ty(LLContext), 2 /* At least 2 but can add more */,
            "cond.res", LLEndBB);
        for (llvm::pred_iterator PI = llvm::pred_begin(LLEndBB),
                                 PE = llvm::pred_end(LLEndBB);
            PI != PE; ++PI) {
            LLResPHINode->addIncoming(llvm::ConstantInt::getTrue(LLContext), *PI);
        }

        Builder.SetInsertPoint(LLLHSFalseBB);
        llvm::Value* LLRHSCondV = GenCond(BinOp->RHS);
        // Need to re-obtain the last block since the condiition might have
        // added more blocks.
        LLLHSFalseBB = Builder.GetInsertBlock();
        
        Builder.CreateBr(LLEndBB);
        Builder.SetInsertPoint(LLEndBB);
        LLResPHINode->addIncoming(LLRHSCondV, LLLHSFalseBB);

        return LLResPHINode;
    }
    default:
        assert(!"Failed to implement GenBinaryOp() case!");
        return nullptr;
    }
}

llvm::Value* arco::IRGenerator::GenUnaryOp(UnaryOp* UniOp) {
    
    switch (UniOp->Op) {
    case TokenKind::PLUS_PLUS: case TokenKind::POST_PLUS_PLUS: {
        llvm::Value* LLVal  = GenNode(UniOp->Value);
        llvm::Value* LLRVal = CreateLoad(LLVal);
        llvm::Value* IncRes;
        if (UniOp->Ty->IsPointer()) {
            // Pointer arithemtic
            IncRes = CreateInBoundsGEP(LLRVal, { GetLLInt64(1) });
        } else {
            IncRes = Builder.CreateAdd(LLRVal, GetOneValue(UniOp->Value->Ty), "inc");
        }
        Builder.CreateStore(IncRes, LLVal);
        EMIT_DI(EmitDebugLocation(UniOp));
        return UniOp->Op == TokenKind::PLUS_PLUS ? IncRes : LLRVal;
    }
    case TokenKind::MINUS_MINUS: case TokenKind::POST_MINUS_MINUS: {
        llvm::Value* LLVal  = GenNode(UniOp->Value);
        llvm::Value* LLRVal = CreateLoad(LLVal);
        llvm::Value* IncRes;
        if (UniOp->Ty->IsPointer()) {
            // Pointer arithemtic
            IncRes = CreateInBoundsGEP(LLRVal, { GetLLInt64(-1) });
        } else {
            IncRes = Builder.CreateSub(LLRVal, GetOneValue(UniOp->Value->Ty), "inc");
        }
        Builder.CreateStore(IncRes, LLVal);
        EMIT_DI(EmitDebugLocation(UniOp));
        return UniOp->Op == TokenKind::MINUS_MINUS ? IncRes : LLRVal;
    }
    case '&': {
        // When GenRValue is called it makes sure
        // not to shave off the pointer value for
        // this operator. Because of that all this
        // needs to do is return the l-value.
        return GenNode(UniOp->Value);
    }
    case '-': {
        llvm::Value* LLValue = GenRValue(UniOp->Value);
        if (UniOp->Ty->IsInt()) {
            return Builder.CreateNeg(LLValue);
        } else {
            return Builder.CreateFNeg(LLValue);
        }
    }
    case '+':
        return GenRValue(UniOp->Value);
    case '*': {
        llvm::Value* LLValue = GenNode(UniOp->Value);
        // Need to verify that the the value is actually an l-value.
        //
        // It is possible that it is not in cases in which its a pointer
        // as a result of some operation. Examples:
        // 
        // Example 1:  '*(a + 4)'    (where 'a' is a pointer)
        // Example 2:  'int a = *f()'
        // 
        // TODO: There is probably a cleaner way of doing this.
        //       Some type of IsLValue() function?
        if (UniOp->Value->Is(AstKind::IDENT_REF) || UniOp->Value->Is(AstKind::FIELD_ACCESSOR) ||
            UniOp->Value->Is(AstKind::ARRAY_ACCESS) ||
            (UniOp->Value->Is(AstKind::UNARY_OP) || static_cast<UnaryOp*>(UniOp->Value)->Op == '*')) {
            LLValue = CreateLoad(LLValue);
        }
        return LLValue;
    }
    case '!': {
        if (UniOp->Value->Ty->IsPointer()) {
            return Builder.CreateIsNull(GenRValue(UniOp->Value));
        } else {
            return Builder.CreateNot(GenRValue(UniOp->Value));
        }
    }
    case '~':
        return Builder.CreateNot(GenRValue(UniOp->Value));
    default:
        assert(!"Failed to implement GenUnaryOp() case!");
        return nullptr;
    }
}

llvm::Value* arco::IRGenerator::GenNumberLiteral(NumberLiteral* Number) {
    switch (Number->Ty->GetKind()) {
    case TypeKind::Char:
    case TypeKind::Int8:           return GetLLInt8(Number->SignedIntValue);
    case TypeKind::Int16:          return GetLLInt16(Number->SignedIntValue);
    case TypeKind::Int32:          return GetLLInt32(Number->SignedIntValue);
    case TypeKind::Int64:          return GetLLInt64(Number->SignedIntValue);
    case TypeKind::UInt8:   return GetLLUInt8(Number->UnsignedIntValue);
    case TypeKind::UInt16:  return GetLLUInt16(Number->UnsignedIntValue);
    case TypeKind::UInt32:  return GetLLUInt32(Number->UnsignedIntValue);
    case TypeKind::UInt64:  return GetLLUInt64(Number->UnsignedIntValue);
    case TypeKind::Float32:
        return llvm::ConstantFP::get(LLContext, llvm::APFloat(Number->Float32Value));
    case TypeKind::Float64:
        return llvm::ConstantFP::get(LLContext, llvm::APFloat(Number->Float64Value));
    case TypeKind::Int:
        return GetSystemInt(Number->SignedIntValue);
    case TypeKind::Ptrsize:
        return GetSystemUInt(Number->UnsignedIntValue);
    default:
        assert(!"Unimplemented GenNumberLiteral() case");
        return nullptr;
    }
}

llvm::Value* arco::IRGenerator::GenStringLiteral(StringLiteral* String) {
    return GenStringLiteral(String->Characters.c_str(), String->Characters.size());
}

llvm::Value* arco::IRGenerator::GenStringLiteral(const char* String, ulen Length) {
    // TODO: Should intern the strings.

    llvm::SmallVector<llvm::Constant*, 4> LLElements;
    LLElements.reserve(Length + 1);
    for (ulen i = 0; i < Length; i++) {
        LLElements.push_back(GetLLUInt8(String[i]));
    }
    // Null termination
    LLElements.push_back(GetLLUInt8('\0'));

    llvm::ArrayType* LLArrType =
        llvm::ArrayType::get(llvm::Type::getInt8Ty(LLContext), LLElements.size());
    llvm::Constant* LLConstArray = llvm::ConstantArray::get(LLArrType, LLElements);

    llvm::GlobalVariable* LLGArray = GenConstGlobalArray(LLConstArray);

    return DecayArray(LLGArray);
}

llvm::Value* arco::IRGenerator::GenIdentRef(IdentRef* IRef) {
    if (IRef->RefKind == IdentRef::RK::Funcs) {
        FuncDecl* Func = (*IRef->Funcs)[0];
        GenFuncDecl(Func);
        return Func->GetLLFunction();
    }
    
    VarDecl* Var = IRef->Var;
    if (Var->IsComptime()) {
        return GenComptimeValue(Var);
    }
    if (Var->IsGlobal) {
        GenGlobalVarDecl(Var);
    }
    
    if (Var->IsField()) {
        return CreateStructGEP(LLThis, Var->LLFieldIdx);
    } else {
        return Var->LLAddress;
    }
}

llvm::Value* arco::IRGenerator::GenFieldAccessor(FieldAccessor* FieldAcc) {
    if (FieldAcc->IsArrayLength) {
        return GetSystemInt(
                FieldAcc->Site->Ty->AsArrayTy()->GetLength());
    } else if (FieldAcc->IsSliceLength) {
        return CreateStructGEP(GenNode(FieldAcc->Site), 0);
    }

    if (FieldAcc->EnumValue) {
        // Use static cast because AsStructType strips away enum information.
        EnumDecl* Enum = static_cast<StructType*>(FieldAcc->Ty)->GetEnum();
        ulen EnumIndex = FieldAcc->EnumValue->Index;
        Type* IndexType = Enum->IndexingInOrder ? Enum->ValuesType : Context.IntType;
        return llvm::ConstantInt::get(GenType(IndexType), EnumIndex, false);
    }

    if (FieldAcc->RefKind == IdentRef::RK::Var) {
        if (FieldAcc->Var->IsGlobal) {
            return GenIdentRef(FieldAcc);
        }
        if (FieldAcc->Var->IsComptime()) {
            return GenComptimeValue(FieldAcc->Var);
        }
    }
    
    Expr* Site = FieldAcc->Site;
    llvm::Value* LLSite;	
    if (Site->Is(AstKind::FUNC_CALL) && Site->Ty->GetKind() == TypeKind::Struct) {
        FuncCall* Call = static_cast<FuncCall*>(Site);
        LLSite = CreateUnseenAlloca(GenType(Call->Ty), "tmp.obj");
        AddObjectToDestroyOpt(Call->Ty, LLSite);
        GenStoreStructRetFromCall(Call, LLSite);
    } else {
        LLSite = GenNode(Site);
    }

    // Automatically dereference pointers!
    if (Site->Ty->GetKind() == TypeKind::Pointer &&
        Site->IsNot(AstKind::THIS_REF) && /* Reference is already loaded. */
        Site->IsNot(AstKind::FUNC_CALL) && /* no address to load */
        Site->IsNot(AstKind::TYPE_CAST)/* type cast calls GenRValue */) {
        LLSite = CreateLoad(LLSite);
        LLSite->setName("ptr.deref");
    }

    if (FieldAcc->RefKind == IdentRef::RK::Funcs) {
        // Calling a member function. Ex.  'a.b()'
        return LLSite;
    } else {
        return CreateStructGEP(LLSite, FieldAcc->Var->LLFieldIdx);
    }
}

llvm::Constant* arco::IRGenerator::GenComptimeValue(VarDecl* Var) {
    if (!Var->LLComptimeVal) {
        Var->LLComptimeVal = llvm::cast<llvm::Constant>(GenRValue(Var->Assignment));	
    }
    return Var->LLComptimeVal;
}

llvm::Value* arco::IRGenerator::GenFuncCall(FuncCall* Call,
                                            llvm::Value* LLAddr,
                                            const ErrorAddrList& LLErrorAddrs) {
    FuncDecl* CalledFunc = Call->CalledFunc;
    if (CalledFunc) {
        return GenFuncCallGeneral(
            Call,
            CalledFunc,
            Call->Args,
            Call->NamedArgs,
            LLAddr,
            Call->VarArgsPassAlong,
            LLErrorAddrs
        );
    }

    // Making a call on a variable instead.
    ulen NumArgs = Call->Args.size();

    bool UsesParameterRetSlot = false;
    if (Call->Ty->GetKind() == TypeKind::Struct) {
        StructType* StructTy = Call->Ty->AsStructType();
        UsesParameterRetSlot = FuncUsesParamRetSlot(Context, Call->Ty->AsStructType());
    }

    llvm::SmallVector<llvm::Value*> LLArgs;
    LLArgs.resize(Call->Args.size() + (UsesParameterRetSlot ? 1 : 0));
    ulen ArgIdx = 0;
    
    if (UsesParameterRetSlot) {
        if (!LLAddr) {
            // Strange, but the user has decided to ignore
            // the return value so a temporary object needs
            // to be created.
            LLAddr = CreateUnseenAlloca(GenType(Call->Ty), "ignored.ret");
        }

        LLArgs[ArgIdx++] = LLAddr;
    }
    
    for (ulen i = 0; i < Call->Args.size(); i++) {
        LLArgs[ArgIdx++] = GenCallArg(Call->Args[i].E, false);
    }
    
    llvm::FunctionType* LLFuncTy = llvm::cast<llvm::FunctionType>(
        GenType(Call->Site->Ty)->getPointerElementType());
    llvm::Value* LLCallee = GenRValue(Call->Site);

    llvm::Value* LLRetValue = Builder.CreateCall(LLFuncTy, LLCallee, LLArgs);
    EMIT_DI(EmitDebugLocation(Call));

    if (LLRetValue->getType() != llvm::Type::getVoidTy(LLContext)) {
        LLRetValue->setName("ret.val");
    }
    return LLRetValue;
}

llvm::Value* arco::IRGenerator::GenFuncCallGeneral(Expr* CallNode,
                                                   FuncDecl* CalledFunc,
                                                   llvm::SmallVector<NonNamedValue>& Args,
                                                   llvm::SmallVector<NamedValue>& NamedArgs,
                                                   llvm::Value* LLAddr,
                                                   bool VarArgsPassAlong,
                                                   const ErrorAddrList& LLErrorAddrs) {
    GenericBind* Binding;
    if (!CalledFunc->Interface) {
        if (CalledFunc->IsGeneric()) {
            FuncCall* Call = static_cast<FuncCall*>(CallNode);
            Binding = Call->Binding;
            GenFuncDecl(CalledFunc, Binding);
        } else {
            GenFuncDecl(CalledFunc);
        }
    }
    
    if (CalledFunc->LLVMIntrinsicID) {
        return GenLLVMIntrinsicCall(CallNode->Loc, CalledFunc, Args, NamedArgs);
    }

    ulen NumArgs = CalledFunc->Params.size();
    if (CalledFunc->Struct || CalledFunc->Interface) {
        ++NumArgs;
    }
    if (CalledFunc->UsesParamRetSlot) {
        ++NumArgs;
    }
    NumArgs += LLErrorAddrs.size();
    bool ImplicitlyReturnsErrors = LLErrorAddrs.empty() && !CalledFunc->RaisedErrors.empty();
    if (ImplicitlyReturnsErrors) {
        NumArgs += CalledFunc->RaisedErrors.size() + 1;
    }

    llvm::SmallVector<llvm::Value*, 2> LLArgs;
    LLArgs.resize(NumArgs);

    ulen ArgOffset = 0;
    llvm::Value* LLInterfaceAddr;
    bool TemporaryObjAddr = false;
    if (CalledFunc->Struct) {
        if (CalledFunc->IsConstructor) {
            if (!LLAddr) {
                LLAddr = CreateUnseenAlloca(GenType(CallNode->Ty), "tmp.struct");
                TemporaryObjAddr = true;
            }
            
            LLArgs[ArgOffset++] = LLAddr;
        } else {
            
            FuncCall* Call = static_cast<FuncCall*>(CallNode);
            llvm::Value* LLThisPass;
            if (Call->Site->Is(AstKind::FIELD_ACCESSOR)) {
                // TODO: does this.memfunc() work?
                LLThisPass = GenNode(Call->Site);
            } else {
                // Calling one member function from another.
                LLThisPass = LLThis;
            
                
            }
            if (CalledFunc->MappedInterfaceFunc) {
                // Calling a virtual function so need to adjust the base
                // address which will then readjust it back at the called
                // function.
                
                StructDecl* Struct = CalledFunc->Struct;
                llvm::StructType* LLStructTy = GenStructType(Struct);
                ulen InterfaceOffset = 0;
                InterfaceDecl* Interface = CalledFunc->MappedInterfaceFunc->Interface;
                for (InterfaceDecl* InheritedInterface : Struct->Interfaces) {
                    if (InheritedInterface == Interface) {
                        break;
                    } else {
                        ++InterfaceOffset;
                    }
                }

                // It expects the passed value to be i8* when mapping virtual functions.
                LLThisPass = Builder.CreateBitCast(LLThisPass, llvm::Type::getInt8PtrTy(LLContext));
                if (InterfaceOffset != 0) {
                    // Need to subtract from the passed address to get back to the struct's base address.

                    // TODO: Should we be relying on the layout information of the struct instead?
                    const llvm::StructLayout* LLLayout = LLModule.getDataLayout().getStructLayout(LLStructTy);

                    long long LLInterfaceOffset = (long long)LLLayout->getElementOffset(InterfaceOffset);
                    LLThisPass = CreateInBoundsGEP(LLThisPass, { GetLLInt64(LLInterfaceOffset) });

                }
            }
            LLArgs[ArgOffset++] = LLThisPass;
        }
    } else if (CalledFunc->Interface) {
        FuncCall* Call = static_cast<FuncCall*>(CallNode);
        FieldAccessor* FieldAcc = static_cast<FieldAccessor*>(Call->Site);
        LLInterfaceAddr = CreateLoad(GenNode(FieldAcc->Site));
        LLArgs[ArgOffset++] = Builder.CreateBitCast(LLInterfaceAddr, llvm::Type::getInt8PtrTy(LLContext, 0));
    }
    if (CalledFunc->UsesParamRetSlot) {
        if (!LLAddr) {
            // Strange, but the user has decided to ignore
            // the return value so a temporary object needs
            // to be created.
            LLAddr = CreateUnseenAlloca(GenType(CallNode->Ty), "ignored.ret");
            TemporaryObjAddr = true;
        }

        LLArgs[ArgOffset++] = LLAddr;
    }
    if (!LLErrorAddrs.empty()) {
        for (ulen i = 0; i < LLErrorAddrs.size(); i++) {
            LLArgs[ArgOffset++] = LLErrorAddrs[i];
        }
    }
    if (ImplicitlyReturnsErrors) {
        // This function raises the errors that the called function raises and they are
        // not being captured by this function, so we may simply pass some of ours errors
        // along into the function.
        LLArgs[ArgOffset++] = GetErrorRetAddr(0);
        for (const auto& RaisedError : CalledFunc->RaisedErrors) {
            // GetRetError
            ulen OurErrorIdx = 0;
            for (const auto& OurRaisedError : CFunc->RaisedErrors) {
                if (OurRaisedError.ErrorStruct == RaisedError.ErrorStruct) {
                    break;
                }
                ++OurErrorIdx;
            }
            LLArgs[ArgOffset++] = GetErrorRetAddr(1 + OurErrorIdx);
        }
    }

    if (!CalledFunc->IsVariadic) {
        for (ulen i = 0; i < Args.size(); i++) {
            VarDecl* Param = CalledFunc->Params[i];
            LLArgs[ArgOffset + i] = GenCallArg(Args[i].E, Param->ImplicitPtr);
        }
        for (NamedValue& NamedArg : NamedArgs) {
            VarDecl* Param = NamedArg.VarRef;
            LLArgs[ArgOffset + Param->ParamIdx] = GenCallArg(NamedArg.AssignValue, Param->ImplicitPtr);
        }
    } else {
        ulen i = 0;
        for (; i < CalledFunc->Params.size() - 1; i++) {
            VarDecl* Param = CalledFunc->Params[i];
            LLArgs[ArgOffset++] = GenCallArg(Args[i].E, Param->ImplicitPtr);
        }
        if (VarArgsPassAlong) {
            LLArgs[ArgOffset++] = GenRValue(Args[i].E); // TODO: implicit pointer here?
        } else {
            ulen NumVarArgs = Args.size() - (CalledFunc->Params.size() - 1);
            VarDecl* LastParam = CalledFunc->Params[CalledFunc->Params.size() - 1];
            bool ImplicitPtr = LastParam->ImplicitPtr;

            // Creating the array that the slice points to.
            // TODO: could optimize this in case the arguments are constant...

            Type* ElmType = LastParam->Ty->AsSliceTy()->GetElementType();
            ArrayType* ArrayTy = ArrayType::Create(ElmType, NumVarArgs, Context);
            llvm::Value* LLArray = CreateUnseenAlloca(GenType(ArrayTy), "tmp.varargs.arr");
            ulen ArrayIdx = 0;
            for (; i < Args.size(); i++, ArrayIdx++) {
                llvm::Value* LLElmAddr = GetArrayIndexAddress(LLArray, GetSystemInt(ArrayIdx));
                Builder.CreateStore(GenCallArg(Args[i].E, ImplicitPtr), LLElmAddr);
            }
        
            llvm::Value* LLVarArgs = CreateUnseenAlloca(GenType(LastParam->Ty), "tmp.varargs");
            llvm::Value* LLSliceLengthAddr = CreateStructGEP(LLVarArgs, 0);
            Builder.CreateStore(GetSystemInt(NumVarArgs), LLSliceLengthAddr);
            llvm::Value* LLSliceArrPtrAddr = CreateStructGEP(LLVarArgs, 1);
            Builder.CreateStore(DecayArray(LLArray), LLSliceArrPtrAddr);
        
            LLArgs[ArgOffset++] = CreateLoad(LLVarArgs);
        }
    }

    if (CalledFunc->NumDefaultArgs) {
        // TODO: Optimization: It is probably better overall to just have all the arguments
        // placed into one vector and to just ieratate over that vector, if the slot is nullptr
        // then we know it's a default argument otherwise we generate code. That way we do not
        // have to lookup and check if a named argument was passed or not.
        if (NamedArgs.empty()) {
            for (ulen i = 0; i < CalledFunc->Params.size() - Args.size(); i++) {
                VarDecl* Param = CalledFunc->Params[Args.size() + i];
                LLArgs[ArgOffset + Param->ParamIdx] = GenCallArg(Param->Assignment, false);
            }
        } else {
            for (ulen i = 0; i < CalledFunc->Params.size() - Args.size(); i++) {
                VarDecl* Param = CalledFunc->Params[Args.size() + i];
                auto Itr = std::find_if(NamedArgs.begin(), NamedArgs.end(),
                    [Param](const NamedValue& A) {
                        return A.VarRef == Param;
                    });
                if (Itr == NamedArgs.end()) {
                    LLArgs[ArgOffset + Param->ParamIdx] = GenCallArg(Param->Assignment, false);
                }
            }
        }    
    }
    
    llvm::Value* LLRetValue;
    if (!CalledFunc->Interface) {

        llvm::Function* LLCalledFunc;
        GenericBind* PrevBinding;
        if (CalledFunc->IsGeneric()) {
            PrevBinding = CalledFunc->CurBinding;
            CalledFunc->CurBinding = Binding;
        }
        LLCalledFunc = CalledFunc->GetLLFunction();
        if (CalledFunc->IsGeneric()) {
            CalledFunc->CurBinding = PrevBinding;
        }

        // -- DEBUG
        //llvm::outs() << "Calling function with name: " << CalledFunc->Name << "\n";
        //llvm::outs() << "Types passed to function:\n";
        //for (ulen i = 0; i < LLArgs.size(); i++) {
        //	llvm::outs() << LLValTypePrinter(LLArgs[i]) << "\n";
        //}
        //llvm::outs() << "Types expected by function:\n";
        //for (ulen i = 0; i < LLCalledFunc->arg_size(); i++) {
        //	llvm::outs() << LLValTypePrinter(LLCalledFunc->getArg(i)) << "\n";
        //}
        //llvm::outs() << "\n";

        LLRetValue = Builder.CreateCall(LLCalledFunc, LLArgs);
        EMIT_DI(EmitDebugLocation(CallNode));

    } else {
        InterfaceDecl* Interface = CalledFunc->Interface;

        if (Interface->NumFuncs == 1) {
            llvm::Value* LLMappedInterfaceFunc = CreateLoad(LLInterfaceAddr);
            llvm::FunctionType* LLFuncTy = llvm::cast<llvm::FunctionType>(LLMappedInterfaceFunc->getType()->getPointerElementType());
            LLRetValue = Builder.CreateCall(LLFuncTy, LLMappedInterfaceFunc, LLArgs);
        } else {
            llvm::Value* LLOffsetIntoVTable = CreateLoad(LLInterfaceAddr);
            if (CalledFunc->InterfaceIdx != 0) {
                // Must offset into the array from the already given offset to select
                // the correct function.
                // TODO: Hope this is right because this honestly is a bit spooky having to use an existing
                // address of an array to then obtain the next address since we do not have the address of the
                // array itself.
                LLOffsetIntoVTable = CreateInBoundsGEP(LLOffsetIntoVTable, { GetLLUInt64(CalledFunc->InterfaceIdx) });
            }

            llvm::Value* LLFuncPtr = CreateLoad(LLOffsetIntoVTable);
            // Have to bitcast to the function type.
            llvm::FunctionType* LLFuncTy = GenArcoConvFuncType(Context, CalledFunc);
            LLFuncPtr = Builder.CreateBitCast(LLFuncPtr, llvm::PointerType::get(LLFuncTy, 0));
            LLRetValue = Builder.CreateCall(LLFuncTy, LLFuncPtr, LLArgs);
            EMIT_DI(EmitDebugLocation(CallNode));
        }
    }

    if (LLRetValue->getType() != llvm::Type::getVoidTy(LLContext)) {
        LLRetValue->setName("ret.val");
    }
    
    if (ImplicitlyReturnsErrors) {
        // Implicit if statement to check for err.

        llvm::BasicBlock* LLThenBB = llvm::BasicBlock::Create(LLContext, "ife.then", LLFunc);
        llvm::BasicBlock* LLEndBB  = llvm::BasicBlock::Create(LLContext, "ife.end", LLFunc);
        
        llvm::Value* LLErrorInterfaceAddr = GetErrorRetAddr(0);
        llvm::Value* LLCond = Builder.CreateIsNull(CreateLoad(LLErrorInterfaceAddr));
        
        if (TemporaryObjAddr) {
            llvm::Value* LLCondAddr = CreateUnseenAlloca(llvm::Type::getInt1Ty(LLContext), "destroy.cond", true);
            Builder.CreateStore(LLCond, LLCondAddr);
            AddObjectToDestroyOpt(CallNode->Ty, LLAddr, LLCondAddr);
        }
        
        Builder.CreateCondBr(LLCond, LLEndBB, LLThenBB);

        Builder.SetInsertPoint(LLThenBB);
        GenRaiseReturnZeroedValue();
        GenBranchIfNotTerm(LLEndBB);
        Builder.SetInsertPoint(LLEndBB);
    } else if (TemporaryObjAddr) {
        AddObjectToDestroyOpt(CallNode->Ty, LLAddr);
    }
    
    if (CalledFunc->IsConstructor) {
        // Return the address if we had to create one.
        return LLAddr;
    } else {
        return LLRetValue;
    }
}

llvm::Value* arco::IRGenerator::GenCallArg(Expr* Arg, bool ImplictPtr) {
    if (ImplictPtr) {
        if (Arg->CastTy) {
            Arg->CastTy = nullptr; // TODO: hacky fix to make sure it does not cast.
            if (Arg->Is(AstKind::FUNC_CALL) && Arg->Ty->GetKind() == TypeKind::Struct) {
                // Doesn't have an address so need to create one.
                llvm::Value* LLArg = CreateUnseenAlloca(GenType(Arg->Ty), "arg.tmp");
                // TODO: Do we need to move the object here?
                GenStoreStructRetFromCall(static_cast<FuncCall*>(Arg), LLArg);
                
                // It is passed as a pointer so the current function has ownership over the memory.
                AddObjectToDestroyOpt(Arg->Ty, LLArg);

                // No need to load because we want the struct's address.
                return LLArg;
            } else {
                return GenNode(Arg);
            }
        } else {
            // Great it is already the pointer.
            return GenRValue(Arg);
        }
    }

    if (Context.StdAnyStruct) {
        if (Arg->Ty->Equals(Context.AnyType)) {
            // Any type is the argument so the destination type
            // must be Any type.
            return GenRValue(Arg);
        }
        if (Arg->CastTy && Arg->CastTy->GetKind() == TypeKind::Struct) {
            if (Arg->Is(AstKind::FUNC_CALL) && Arg->Ty->GetKind() == TypeKind::Struct) {
                llvm::Value* LLArg = CreateUnseenAlloca(GenType(Arg->Ty), "arg.tmp");
                GenStoreStructRetFromCall(static_cast<FuncCall*>(Arg), LLArg);

                // Any does not have ownership.
                AddObjectToDestroyOpt(Arg->Ty, LLArg);

                return CreateLoad(GenCast(Arg->CastTy, Arg->Ty, LLArg));
            } else {
                // Cast to any so nothing below applies.
                return GenRValue(Arg);
            }
        }
    }

    // TODO: once constant struct's are supported and they can be part of enums
    // this will need to check the parameter type instead.
    llvm::Value* LLArg = nullptr;
    if (Arg->Is(AstKind::FUNC_CALL) && Arg->Ty->GetKind() == TypeKind::Struct) {
        // The argument is a call that returns a struct.
        // There is no reason to destroy the memory since
        // the called function will just take ownership
        // over the memory.

        LLArg = CreateUnseenAlloca(GenType(Arg->Ty), "arg.tmp");
        GenStoreStructRetFromCall(static_cast<FuncCall*>(Arg), LLArg);
        
        // TODO: If there ends up being further optimizations such that parameters
        // take into account similar constraints to return values where structs
        // get passed as integers/pointers depending on memory size then this will
        // not need to be loaded.
        LLArg = CreateLoad(LLArg);

    } else if (Arg->Is(AstKind::STRUCT_INITIALIZER)) {
        // No need to destroy the object because it is going to be passed to a function
        // which takes ownership over the memory.
        StructDecl* Struct = Arg->Ty->AsStructType()->GetStruct();
        LLArg = CreateUnseenAlloca(GenStructType(Struct), "arg.tmp");
        GenStructInitializer(static_cast<StructInitializer*>(Arg), LLArg);
        LLArg = CreateLoad(LLArg);
    } else if (Arg->Ty->GetKind() == TypeKind::Struct) {
        StructDecl* Struct = Arg->Ty->AsStructType()->GetStruct();
        LLArg = CreateUnseenAlloca(GenStructType(Struct), "arg.tmp");
        if (Arg->Is(AstKind::MOVEOBJ)) {
            CopyOrMoveStructObject(LLArg, GenNode(static_cast<MoveObj*>(Arg)->Value), Struct);
        } else {
            // Ex.
            //   fn foo(a A) {}
            //
            //  ...
            //   a A;
            //   foo(a)

            llvm::Value* LLCopyAddr = GenNode(Arg);
            CopyStructObject(LLArg, LLCopyAddr, Struct);
        }
        LLArg = CreateLoad(LLArg);
    } else {
        LLArg = GenRValue(Arg);

        if (Arg->Ty->GetKind() == TypeKind::Array) {
            // Arrays are passed as pointers. Cannot simply decay though
            // because the argument might be an already decayed array.
            LLArg = ArrayToPointer(LLArg);
        }
    }
    return LLArg;
}

llvm::Value* arco::IRGenerator::GenArray(Array* Arr, llvm::Value* LLAddr, bool IsConstDest) {

    ArrayType* DestTy = GetGenArrayDestType(Arr);
    bool DestIsPointer = Arr->CastTy && Arr->CastTy->IsPointer();

    if (!LLAddr && !DestIsPointer) {
        LLAddr = CreateUnseenAlloca(GenType(DestTy), "tmp.array");
        AddObjectToDestroyOpt(Arr->Ty, LLAddr);
    }

    if (Arr->IsFoldable) {

        if (DestIsPointer) {
            if (IsConstDest) {
                // No need to do any complicated memcopying the pointer
                // can just point to constant global array.

                // TODO: Should the dso local value change?
                return GenConstGlobalArray(GenConstArray(Arr, DestTy));
            }

            // Need a temporary array that can be memcopied into
            // and then pointed at by the pointer.
            LLAddr = CreateUnseenAlloca(GenType(DestTy), "tmp.array");
            AddObjectToDestroyOpt(Arr->Ty, LLAddr);
        }


        // For the sake of efficiency we memcpy the array over
        // into the destination.

        llvm::Value* LLGArray = GenConstGlobalArray(GenConstArray(Arr, DestTy));

        ulen TotalLinearLength = DestTy->GetTotalLinearLength();

        llvm::Type* LLDestTy = GenType(DestTy->GetBaseType());
        llvm::Align LLAlignment = GetAlignment(LLDestTy);
        Builder.CreateMemCpy(
            LLAddr, LLAlignment,
            LLGArray, LLAlignment,
            TotalLinearLength * SizeOfTypeInBytes(LLDestTy)
        );

    } else {

        if (DestIsPointer) {
            // if the destination is a pointer but also that pointer is global
            // then the array needs to be a global array rather than
            // a unseen alloca.
            if (LLAddr && llvm::isa<llvm::GlobalValue>(LLAddr)) {
                LLAddr = GenLLVMGlobalVariable(std::string("__global.array.") + std::to_string(Context.NumGeneratedGlobalVars), GenType(DestTy));
                llvm::GlobalVariable* LLGVar = llvm::cast<llvm::GlobalVariable>(LLAddr);
                LLGVar->setInitializer(GenZeroedValue(DestTy));
            } else {
                // Creating a temporary array that the pointer can point to.
                LLAddr = CreateUnseenAlloca(GenType(DestTy), "tmp.array");
                AddObjectToDestroyOpt(Arr->Ty, LLAddr);
            }
        }

        FillArrayViaGEP(Arr, LLAddr, DestTy);
    }

    return LLAddr;
}

arco::ArrayType* arco::IRGenerator::GetGenArrayDestType(Array* Arr) {
    ArrayType* DestTy = Arr->Ty->AsArrayTy();
    if (Arr->CastTy) {
        TypeKind CastKind = Arr->CastTy->GetKind();
        if (CastKind == TypeKind::Array) {
            DestTy = Arr->CastTy->AsArrayTy();
        } else if (CastKind == TypeKind::Pointer) {
            DestTy = ArrayType::Create(
                Arr->CastTy->AsPointerTy()->GetElementType(),
                DestTy->GetLength(),
                Context);
        } else if (CastKind == TypeKind::CStr) {
            DestTy = ArrayType::Create(
                Context.CharType,
                DestTy->GetLength(),
                Context);
        } else if (CastKind == TypeKind::Slice ||
                   CastKind == TypeKind::Struct /* Cast to Any type */) {
            return DestTy;
        } else {
            assert(!"Unreachable");
        }
    }
    return DestTy;
}

llvm::Constant* arco::IRGenerator::GenConstArray(Array* Arr, ArrayType* DestTy) {

    bool ElmsAreArrs = DestTy->GetElementType()
                             ->GetKind() == TypeKind::Array;

    llvm::SmallVector<llvm::Constant*, 4> LLElements;
    for (ulen i = 0; i < DestTy->GetLength(); i++) {
        llvm::Value* LLElmValue;
        if (i < Arr->Elements.size()) {
            Expr* Elm = Arr->Elements[i];
            if (ElmsAreArrs) {
                LLElmValue = GenConstArray(
                    static_cast<Array*>(Elm),
                    DestTy->GetElementType()->AsArrayTy()
                );
            } else {
                LLElmValue = GenRValue(Elm);
                // May need to perform another cast to ensure the element cast
                // to the type of the array.
                Type* ElmTy = Elm->CastTy ? Elm->CastTy : Elm->Ty;
                if (!ElmTy->Equals(DestTy->GetElementType())) {
                    LLElmValue = GenCast(DestTy->GetElementType(), ElmTy, LLElmValue);
                }
            }
        } else {
            LLElmValue = GenConstValue(DestTy->GetElementType());
        }
        LLElements.push_back(llvm::cast<llvm::Constant>(LLElmValue));
    }

    llvm::ArrayType* LLArrType =
        llvm::ArrayType::get(GenType(DestTy->GetElementType()), DestTy->GetLength());
    return llvm::ConstantArray::get(LLArrType, LLElements);
}

void arco::IRGenerator::FillArrayViaGEP(Array* Arr, llvm::Value* LLAddr, ArrayType* DestTy) {
    
    Type* ElmTy = DestTy->GetElementType();
    bool ElmsAreArrs = ElmTy->GetKind() == TypeKind::Array;

    bool AddrIsPtr = LLAddr->getType()->isPointerTy() && !LLAddr->getType()->getPointerElementType()->isArrayTy();
    
    for (ulen i = 0; i < DestTy->GetLength(); i++) {
        
        // Because the address might come from a heap allocation it is possible
        // it refers to a pointer therefore it becomes necessary to check if it
        // is a pointer first.
        llvm::Value* LLIndex = GetSystemUInt(i);
        llvm::Value* LLAddrAtIndex;
        if (AddrIsPtr) {
            LLAddrAtIndex = CreateInBoundsGEP(LLAddr, { LLIndex });
        } else {
            LLAddrAtIndex = GetArrayIndexAddress(LLAddr, LLIndex);
        }
        
        if (i < Arr->Elements.size()) {
            Expr* Elm = Arr->Elements[i];
            if (ElmsAreArrs) {
                FillArrayViaGEP(
                    static_cast<Array*>(Arr),
                    LLAddrAtIndex,
                    ElmTy->AsArrayTy()
                );
            } else {
                // TODO: HasConstAddress = false?
                GenAssignment(LLAddrAtIndex, ElmTy, Elm, false);
            }
        } else {
            GenDefaultValue(ElmTy, LLAddrAtIndex);
        }
    }
}

llvm::Value* arco::IRGenerator::GenArrayAccess(ArrayAccess* Access) {

    llvm::Value* LLSite  = GenNode(Access->Site);
    llvm::Value* LLIndex = GenRValue(Access->Index);

    if (Access->Site->Ty->GetKind() == TypeKind::Slice) {
        LLSite = CreateStructGEP(LLSite, 1);
        LLSite = CreateLoad(LLSite);
        llvm::Value* LLAccess = CreateInBoundsGEP(LLSite, LLIndex);
        LLAccess->setName("slice.access");
        return LLAccess;
    } else if (Access->Site->Is(AstKind::TYPE_CAST)) { // TODO: Wouldn't this also apply for foo()[4] cases?
        // Deal with annoying fact that type casting call GenRValue for
        // pointers meaning CreateLoad will fudge up the GenNode call.
        llvm::Value* LLAccess = CreateInBoundsGEP(LLSite, LLIndex);
        LLAccess->setName("ptr.access");
        return LLAccess;
    }

    llvm::Type* LLSiteType = LLSite->getType()->getPointerElementType();
    llvm::Value* LLAccess;
    if (LLSiteType->isPointerTy()) {
        LLSite = CreateLoad(LLSite);
        LLAccess = CreateInBoundsGEP(LLSite, LLIndex);
    } else {
        LLAccess = GetArrayIndexAddress(LLSite, LLIndex);
    }

    if (Access->Site->Ty->GetKind() == TypeKind::Array) {
        LLAccess->setName("arr.access");
    } else {
        LLAccess->setName("ptr.access");
    }

    return LLAccess;
}

llvm::Value* arco::IRGenerator::GenTypeCast(TypeCast* Cast) {
    return GenCast(Cast->Ty, Cast->Value->Ty, GenRValue(Cast->Value));
}

llvm::Value* arco::IRGenerator::GenTypeBitCast(TypeBitCast* Cast) {
   
    llvm::Value* LLValue = GenRValue(Cast->Value);
    Type* FromType = Cast->Value->Ty;
    // Enum values may be floats and which are not indexable so need to load.
    //
    if (FromType->GetRealKind() == TypeKind::Enum) {
        
        // KEEP static cast here because AsStructTy will strip away enum information.
        EnumDecl* Enum = static_cast<StructType*>(FromType)->GetEnum();
        FromType = Enum->ValuesType;
        if (!Enum->IndexingInOrder) {
            // Not indexing in order so we have to get the values out of a global array.
            llvm::Value* LLGlobalEnumArray = GenGlobalEnumArray(Enum);
            llvm::Value* LLIndexAddress = GetArrayIndexAddress(LLGlobalEnumArray, LLValue);
            LLValue = CreateLoad(LLIndexAddress);
        }
    }

    return Builder.CreateBitCast(LLValue, GenType(Cast->Ty));
}

llvm::Value* arco::IRGenerator::GenStructInitializer(StructInitializer* StructInit,
                                                     llvm::Value* LLAddr,
                                                     const ErrorAddrList& LLErrorAddrs) {
    StructType* StructTy = StructInit->Ty->AsStructType();
    StructDecl* Struct = StructTy->GetStruct();

    if (StructInit->CalledConstructor) {
        return GenFuncCallGeneral(
            StructInit,
            StructInit->CalledConstructor,
            StructInit->Args,
            StructInit->NamedArgs,
            LLAddr,
            StructInit->VarArgsPassAlong,
            LLErrorAddrs
        );
    }
    
    // This must go after GenFuncCallGeneral because if the constructor raises an
    // error then the GenFuncCallGeneral code does special work when LLAddr is nullptr
    // to make sure to only clean up the object if it is actually initialized.
    if (!LLAddr) {
        LLAddr = CreateUnseenAlloca(GenStructType(StructTy), "tmp.structinit");
        AddObjectToDestroyOpt(StructTy, LLAddr);
    }

    GenStructInitArgs(LLAddr, Struct, StructInit->Args, StructInit->NamedArgs);

    return LLAddr;
}

void arco::IRGenerator::GenStructInitArgs(llvm::Value* LLAddr,
                                          StructDecl* Struct,
                                          llvm::SmallVector<NonNamedValue>& Args,
                                          llvm::SmallVector<NamedValue>& NamedArgs) {

    // Initializing the VTable and storing pointers to it.
    
    // TODO: Language optimization: This is probably pointless if the
    //       structures only has like a single interface.
    if (!Struct->Interfaces.empty()) {
        GenCallToInitVTableFunc(LLAddr, Struct);
    }

    ulen i = 0;
    for (i = 0; i < Args.size(); i++) {
        NonNamedValue Value = Args[i];
        VarDecl* Field = Struct->Fields[i];
        
        llvm::Value* LLFieldAddr = CreateStructGEP(LLAddr, Field->LLFieldIdx);
        GenAssignment(LLFieldAddr, Field->Ty, Value.E, Field->HasConstAddress);
    }
    for (NamedValue& Arg : NamedArgs) {
        VarDecl* Field = Arg.VarRef;

        llvm::Value* LLFieldAddr = CreateStructGEP(LLAddr, Field->LLFieldIdx);
        GenAssignment(LLFieldAddr, Field->Ty, Arg.AssignValue, Field->HasConstAddress);
    }
    if (NamedArgs.empty()) {
        for (; i < Struct->Fields.size(); i++) {
            VarDecl* Field = Struct->Fields[i];

            llvm::Value* LLFieldAddr = CreateStructGEP(LLAddr, Field->LLFieldIdx);
            if (Field->Assignment) {
                GenAssignment(LLFieldAddr, Field->Ty, Field->Assignment, Field->HasConstAddress);
            } else if (!Field->LeaveUninitialized) {
                GenDefaultValue(Field->Ty, LLFieldAddr);
            }
        }
    } else {
        // Fill in arguments past the non-named arguments which are not
        // covered by the named arguments.
        for (; i < Struct->Fields.size(); i++) {
            VarDecl* Field = Struct->Fields[i];
            
            auto Itr = std::find_if(NamedArgs.begin(), NamedArgs.end(),
                [Field](const NamedValue& A) {
                    return A.VarRef == Field;
                });
            if (Itr == NamedArgs.end()) {
                llvm::Value* LLFieldAddr = CreateStructGEP(LLAddr, Field->LLFieldIdx);
                if (Field->Assignment) {
                    GenAssignment(LLFieldAddr, Field->Ty, Field->Assignment, Field->HasConstAddress);
                } else if (!Field->LeaveUninitialized) {
                    GenDefaultValue(Field->Ty, LLFieldAddr);
                }
            }
        }
    }
}

llvm::Value* arco::IRGenerator::GenHeapAlloc(HeapAlloc* Alloc, const ErrorAddrList& LLErrorAddrs) {
    Type* TypeToAlloc = Alloc->TypeToAlloc;
    if (TypeToAlloc->GetKind() == TypeKind::Array) {
        ArrayType* ArrayTy = TypeToAlloc->AsArrayTy();

        // Calculating how much memory needs to be allocated for the array.
        llvm::Value* LLTotalLinearLength = GenRValue(ArrayTy->GetLengthExpr());
        while (ArrayTy->GetElementType()->GetKind() == TypeKind::Array) {
            ArrayTy = ArrayTy->GetElementType()->AsArrayTy();
            LLTotalLinearLength = 
                Builder.CreateMul(LLTotalLinearLength, GenRValue(ArrayTy->GetLengthExpr()));
        }

        Type* BaseTy = ArrayTy->GetElementType();
        llvm::Value* LLArrStartPtr = GenMalloc(GenType(BaseTy), LLTotalLinearLength);
        
        if (!Alloc->Values.empty()) {
            GenAssignment(LLArrStartPtr, BaseTy, Alloc->Values[0].E, false);
        } else if (BaseTy->GetKind() == TypeKind::Struct) {
            // Need to initialize fields so calling the default constructor.
            StructArrayCallDefaultConstructors(BaseTy, LLArrStartPtr, LLTotalLinearLength);
        }

        return LLArrStartPtr;
    } else {
        llvm::Value* LLMalloc = GenMalloc(GenType(TypeToAlloc), nullptr);
        if (TypeToAlloc->GetKind() == TypeKind::Struct) {
            StructType* StructTy = TypeToAlloc->AsStructType();
            StructDecl* Struct = StructTy->GetStruct();
            if (Alloc->CalledConstructor) {
                GenFuncCallGeneral(Alloc,
                                  Alloc->CalledConstructor,
                                  Alloc->Values,
                                  Alloc->NamedValues,
                                  LLMalloc, Alloc->VarArgsPassAlong,
                                  LLErrorAddrs);
            } else {
                if (!Alloc->Values.empty()) {
                    GenStructInitArgs(LLMalloc, Struct, Alloc->Values, Alloc->NamedValues);
                } else {
                    // Need to initialize fields so calling the default constructor.
                    CallDefaultConstructor(LLMalloc, TypeToAlloc->AsStructType());
                }
            }
        } else {
            if (!Alloc->Values.empty()) {
                GenAssignment(LLMalloc, TypeToAlloc, Alloc->Values[0].E, false);
            }
        }
        return LLMalloc;
    }
}

llvm::Value* arco::IRGenerator::GenTypeOf(TypeOf* TOf) {
    return GenTypeOfGlobal(TOf->TypeToGetTypeOf);
}

llvm::GlobalVariable* arco::IRGenerator::GenTypeOfGlobal(Type* GetTy) {
    auto Itr = Context.LLTypeInfoMap.find(GetTy->GetUniqueId());
    if (Itr != Context.LLTypeInfoMap.end()) {
        return Itr->second;
    } else {
        llvm::StructType* LLTypeType = GenStructType(Context.StdTypeStruct);
        std::string LLGlobalTypeInfoName = "__global.typeinfo." + std::to_string(Context.NumGeneratedGlobalVars++);
        llvm::GlobalVariable* LLGlobal = GenLLVMGlobalVariable(LLGlobalTypeInfoName, LLTypeType);
        // Set before calling GenTypeOfType to not end up with duplicates.
        Context.LLTypeInfoMap.insert({ GetTy->GetUniqueId(), LLGlobal });
        LLGlobal->setInitializer(GenTypeOfType(GetTy));

        LLGlobal->setConstant(true);
        LLGlobal->setDSOLocal(true);
        return LLGlobal;
    }
}

llvm::Constant* arco::IRGenerator::GenTypeOfType(Type* GetTy) {
    
    llvm::StructType* LLTypeType          = GenStructType(Context.StdTypeStruct);
    llvm::StructType* LLArrayStructType   = GenStructType(Context.StdArrayTypeStruct);
    llvm::StructType* LLStructStructType  = GenStructType(Context.StdStructTypeStruct);
    llvm::StructType* LLEnumStructType    = GenStructType(Context.StdEnumTypeStruct);

    // We use the GetRealKind() so as to not loose enum information.
    TypeKind Kind = GetTy->GetRealKind();

    llvm::Constant* LLTypeId = GetSystemInt(static_cast<i64>(Kind));
    llvm::Constant* LLPointerInfo, *LLArrayInfo, *LLStructInfo, *LLEnumInfo;
    if (Kind == TypeKind::Pointer || Kind == TypeKind::CStr) {
        LLPointerInfo = GenTypeOfGlobal(GetTy->GetPointerElementType(Context));
    } else if (Kind == TypeKind::Slice) {
        LLPointerInfo = GenTypeOfGlobal(GetTy->AsSliceTy()->GetElementType());
    } else {
        LLPointerInfo = llvm::Constant::getNullValue(llvm::PointerType::get(LLTypeType, 0));
    }
    if (Kind == TypeKind::Array) {
        LLArrayInfo = GenTypeOfArrayTypeGlobal(GetTy->AsArrayTy());
    } else {
        LLArrayInfo = llvm::Constant::getNullValue(llvm::PointerType::get(LLArrayStructType, 0));
    }
    if (Kind == TypeKind::Struct) {
        LLStructInfo = GenTypeOfStructTypeGlobal(GetTy->AsStructType());
    } else {
        LLStructInfo = llvm::Constant::getNullValue(llvm::PointerType::get(LLStructStructType, 0));
    }
    if (Kind == TypeKind::Enum) {
        LLEnumInfo = GenTypeOfEnumTypeGlobal(static_cast<StructType*>(GetTy)->GetEnum());
    } else {
        LLEnumInfo = llvm::Constant::getNullValue(llvm::PointerType::get(LLEnumStructType, 0));
    }

    ulen SizeInBytes;
    if (GetTy->GetKind() == TypeKind::Function) {
        // functions types are unsized but we treat them as pointers.
        SizeInBytes = LLModule.getDataLayout().getPointerSize();
    } else {
        llvm::Type* LLGetType = GenType(GetTy);
        if (LLGetType->isSized()) {
            SizeInBytes = SizeOfTypeInBytes(LLGetType);
        } else {
            SizeInBytes = 0;
        }
    }

    llvm::SmallVector<llvm::Constant*> LLElements = {
        LLTypeId,
        GetSystemInt(SizeInBytes),
        LLPointerInfo,
        LLArrayInfo,
        LLStructInfo,
        LLEnumInfo
    };

    return llvm::ConstantStruct::get(LLTypeType, LLElements);
}

llvm::GlobalVariable* arco::IRGenerator::GenTypeOfArrayTypeGlobal(ArrayType* ArrayTy) {
    llvm::SmallVector<llvm::Constant*, 2> LLElements = {
        GenTypeOfGlobal(ArrayTy->GetElementType()),
        GetSystemInt(ArrayTy->GetLength())
    };
    llvm::StructType* LLArrayStructType = GenStructType(Context.StdArrayTypeStruct);
    std::string LLGlobalTypeInfoName = "__global.typeinfo.arr." + std::to_string(Context.NumGeneratedGlobalVars++);
    llvm::GlobalVariable* LLGlobal = GenLLVMGlobalVariable(LLGlobalTypeInfoName, LLArrayStructType);
    LLGlobal->setInitializer(llvm::ConstantStruct::get(LLArrayStructType, LLElements));
    return LLGlobal;
}

llvm::GlobalVariable* arco::IRGenerator::GenTypeOfStructTypeGlobal(StructType* StructTy) {
    
    StructDecl* Struct = StructTy->GetStruct();

    // Creating a field array.
    llvm::SmallVector<llvm::Constant*> LLFields;
    LLFields.resize(Struct->Fields.size());
    llvm::StructType* LLFieldStructType = GenStructType(Context.StdFieldTypeStruct);

    llvm::StructType* LLStructType = GenStructType(StructTy);
    const llvm::StructLayout* LLStructLayout = LLModule.getDataLayout().getStructLayout(LLStructType);
    for (VarDecl* Field : Struct->Fields) {
        
        Identifier FieldName = Field->Name;
        ulen FieldOffset = LLStructLayout->getElementOffset(Field->LLFieldIdx);
        llvm::SmallVector<llvm::Constant*, 2> LLFieldElements = {
            llvm::cast<llvm::Constant>(GenStringLiteral(FieldName.Text.data(), FieldName.Text.size())),
            GenTypeOfGlobal(Field->Ty),
            GetSystemInt(FieldOffset)
        };

        // NOTE: Not using LLFieldIdx here because typeof does not include the field information of the
        //       interfaces.
        LLFields[Field->FieldIdx] = llvm::ConstantStruct::get(LLFieldStructType, LLFieldElements);
    }
    std::string LLGlobalFieldArrayName = "__global.typeinfo.field.arr." + std::to_string(Context.NumGeneratedGlobalVars++);
    llvm::ArrayType* LLFieldArrayTy = llvm::ArrayType::get(LLFieldStructType, LLFields.size());
    llvm::GlobalVariable* LLFieldsArrayGlobal = GenLLVMGlobalVariable(LLGlobalFieldArrayName, LLFieldArrayTy);
    LLFieldsArrayGlobal->setInitializer(llvm::ConstantArray::get(LLFieldArrayTy, LLFields));

    Identifier StructName = Struct->Name;
    llvm::SmallVector<llvm::Constant*> LLElements = {
        llvm::cast<llvm::Constant>(GenStringLiteral(StructName.Text.data(), StructName.Text.size())),
        GetSystemInt(Struct->Fields.size()),
        llvm::cast<llvm::Constant>(DecayArray(LLFieldsArrayGlobal))
    };
    llvm::StructType* LLStructStructType = GenStructType(Context.StdStructTypeStruct);
    std::string LLGlobalTypeInfoName = "__global.typeinfo.struct." + std::to_string(Context.NumGeneratedGlobalVars++);
    llvm::GlobalVariable* LLGlobal = GenLLVMGlobalVariable(LLGlobalTypeInfoName, LLStructStructType);
    LLGlobal->setInitializer(llvm::ConstantStruct::get(LLStructStructType, LLElements));
    return LLGlobal;
}

llvm::GlobalVariable* arco::IRGenerator::GenTypeOfEnumTypeGlobal(EnumDecl* Enum) {

    // Generating the names array.
    llvm::SmallVector<llvm::Constant*> LLNames;
    LLNames.reserve(Enum->Values.size());
    for (EnumDecl::EnumValue& Value : Enum->Values) {
        Identifier& Name = Value.Name;
        llvm::Constant* LLConstName = llvm::cast<llvm::Constant>(GenStringLiteral(Name.Text.data(), Name.Text.size()));
        LLNames.push_back(LLConstName);
    }
    std::string LLGlobalEnumNameArrayName = "__global.typeinfo.enum.names.arr." + std::to_string(Context.NumGeneratedGlobalVars++);
    llvm::ArrayType* LLEnumNameArrayTy = llvm::ArrayType::get(GenType(Context.CStrType), LLNames.size());
    llvm::GlobalVariable* LLEnumNameArrayGlobal = GenLLVMGlobalVariable(LLGlobalEnumNameArrayName, LLEnumNameArrayTy);
    LLEnumNameArrayGlobal->setInitializer(llvm::ConstantArray::get(LLEnumNameArrayTy, LLNames));

    Type* IndexType = Enum->IndexingInOrder ? Enum->ValuesType : Context.IntType;
    llvm::SmallVector<llvm::Constant*> LLElements = {
         GenTypeOfGlobal(Enum->ValuesType),
         GenTypeOfGlobal(IndexType),
         GetSystemInt(Enum->Values.size()),
         llvm::cast<llvm::Constant>(DecayArray(LLEnumNameArrayGlobal))
    };

    llvm::StructType* LLEnumStructType = GenStructType(Context.StdEnumTypeStruct);
    std::string LLGlobalTypeInfoName = "__global.typeinfo.enum." + std::to_string(Context.NumGeneratedGlobalVars++);
    llvm::GlobalVariable* LLGlobal = GenLLVMGlobalVariable(LLGlobalTypeInfoName, LLEnumStructType);
    LLGlobal->setInitializer(llvm::ConstantStruct::get(LLEnumStructType, LLElements));
    return LLGlobal;
}

llvm::Value* arco::IRGenerator::GenTernary(Ternary* Tern, llvm::Value* LLAddr, bool DestroyIfNeeded) {
    
    if (Tern->Ty->GetKind() == TypeKind::Struct) {
        llvm::BasicBlock* LLThenBB = llvm::BasicBlock::Create(LLContext, "tif.then", LLFunc);
        llvm::BasicBlock* LLEndBB  = llvm::BasicBlock::Create(LLContext, "tif.end", LLFunc);
        llvm::BasicBlock* LLElseBB = llvm::BasicBlock::Create(LLContext, "tif.else", LLFunc);
        
        if (!LLAddr) {
            LLAddr = CreateUnseenAlloca(GenType(Tern->Ty), "tern.result");
        }

        GenBranchOnCond(Tern->Cond, LLThenBB, LLElseBB);
        
        // Then block
        Builder.SetInsertPoint(LLThenBB);
        GenAssignment(LLAddr, Tern->Ty, Tern->LHS, Tern->HasConstAddress, DestroyIfNeeded);
        GenBranchIfNotTerm(LLEndBB);

        // Else block
        Builder.SetInsertPoint(LLElseBB);
        GenAssignment(LLAddr, Tern->Ty, Tern->RHS, Tern->HasConstAddress, DestroyIfNeeded);
        GenBranchIfNotTerm(LLEndBB);


        // Finally continuing forward into a new block after the ternary.
        GenBranchIfNotTerm(LLEndBB);
        Builder.SetInsertPoint(LLEndBB);

        return LLAddr;
    } else if (Tern->Ty->GetKind() == TypeKind::Array) {
        // TODO
    } else {
        return Builder.CreateSelect(GenCond(Tern->Cond), GenRValue(Tern->LHS), GenRValue(Tern->RHS));
    }
}

llvm::Value* arco::IRGenerator::GenVarDeclList(VarDeclList* List) {
    
    if (List->DecomposesError) {
        // Read comment at GenVarDecl for explaination.
        VarDecl* ReturnVar = List->Decls[0];
        VarDecl* ErrorVar  = List->Decls[1];

        FuncDecl* CalledFunc;
        if (ReturnVar->Assignment->Is(AstKind::FUNC_CALL)) {
            FuncCall* Call = static_cast<FuncCall*>(ReturnVar->Assignment);
            CalledFunc = Call->CalledFunc;
        } else if (ReturnVar->Assignment->Is(AstKind::STRUCT_INITIALIZER)) {
            StructInitializer* StructInit = static_cast<StructInitializer*>(ReturnVar->Assignment);
            CalledFunc = StructInit->CalledConstructor;
        } else {
            HeapAlloc* Alloc = static_cast<HeapAlloc*>(ReturnVar->Assignment);
            CalledFunc = Alloc->CalledConstructor;
        }
        
        llvm::SmallVector<llvm::Value*, 4> LLErrorAddrs = GenErrorAddrs(CalledFunc, ErrorVar->LLAddress);

        GenAssignment(ReturnVar->LLAddress,
                      ReturnVar->Ty,
                      ReturnVar->Assignment,
                      ReturnVar->HasConstAddress,
                      false,
                      LLErrorAddrs
                      );

        if (!(CFunc->NumReturns == 1 && CFunc->UsesParamRetSlot && ReturnVar->IsLocalRetValue)) {
            llvm::Value* LLCond = Builder.CreateIsNull(CreateLoad(ErrorVar->LLAddress));
            llvm::Value* LLCondAddr = CreateUnseenAlloca(llvm::Type::getInt1Ty(LLContext), "destroy.cond", true);
            Builder.CreateStore(LLCond, LLCondAddr);

            AddObjectToDestroyOpt(ReturnVar->Ty, ReturnVar->LLAddress, LLCondAddr);
            
        }
    } else {
        for (VarDecl* Var : List->Decls) {
            GenVarDecl(Var);
        }
    }

    // TODO: Eventually needs to return values for predicate if statements.
    return nullptr;
}

arco::IRGenerator::ErrorAddrList arco::IRGenerator::GenErrorAddrs(FuncDecl* CalledFunc, llvm::Value* LLErrorInterfaceAddr) {
    
    const llvm::SmallVector<FuncDecl::RaisedError>& RaisedErrors = CalledFunc->RaisedErrors;

    ErrorAddrList LLErrorAddrs;
    LLErrorAddrs.reserve(1 + RaisedErrors.size());
    LLErrorAddrs.push_back(LLErrorInterfaceAddr);
    for (const auto& RaisedError : RaisedErrors) {
        StructDecl* ErrorStruct = RaisedError.ErrorStruct;
        llvm::Type* LLRaisedErrorStructTy = GenStructType(ErrorStruct);
        llvm::Value* LLErrorStructAddr = CreateUnseenAlloca(LLRaisedErrorStructTy, "error.addr");

        StructType* StructTy = StructType::Create(ErrorStruct, Context);
        GenDefaultValue(StructTy, LLErrorStructAddr);
        AddObjectToDestroyOpt(StructTy, LLErrorStructAddr);
        LLErrorAddrs.push_back(LLErrorStructAddr);
    }

    // Error is null by default.
    llvm::Value* LLNull = llvm::Constant::getNullValue(LLErrorInterfaceAddr->getType()->getPointerElementType());
    Builder.CreateStore(LLNull, LLErrorInterfaceAddr);
    return LLErrorAddrs;
}

llvm::Value* arco::IRGenerator::GenTryError(TryError* Try, llvm::Value* LLAddr) {
    FuncCall* Call = static_cast<FuncCall*>(Try->Value);
    
    llvm::Value* LLInterfaceAddr = CreateUnseenAlloca(GenType(Context.ErrorInterfacePtrType), "try.err");
    
    llvm::Value* LLRetValue = GenFuncCall(Call, LLAddr, GenErrorAddrs(Call->CalledFunc, LLInterfaceAddr));

    llvm::BasicBlock* LLThenBB = llvm::BasicBlock::Create(LLContext, "if.err", LLFunc);
    llvm::BasicBlock* LLEndBB  = llvm::BasicBlock::Create(LLContext, "if.end", LLFunc);

    llvm::Value* LLCond = Builder.CreateIsNotNull(CreateLoad(LLInterfaceAddr));
    Builder.CreateCondBr(LLCond, LLThenBB, LLEndBB);

    Builder.SetInsertPoint(LLThenBB);
    GenFuncDecl(Context.StdErrorPanicFunc);
    Builder.CreateCall(Context.StdErrorPanicFunc->GetLLFunction(), { CreateLoad(LLInterfaceAddr) });
    
    GenBranchIfNotTerm(LLEndBB);
    Builder.SetInsertPoint(LLEndBB);

    return LLRetValue;
}

llvm::Value* arco::IRGenerator::GenAdd(llvm::Value* LLLHS, llvm::Value* LLRHS, Type* Ty) {
    if (Ty->IsInt()) {
        return Builder.CreateAdd(LLLHS, LLRHS);
    }
    return Builder.CreateFAdd(LLLHS, LLRHS);
}

llvm::Value* arco::IRGenerator::GenSub(llvm::Value* LLLHS, llvm::Value* LLRHS, Type* Ty) {
    if (Ty->IsInt()) {
        return Builder.CreateSub(LLLHS, LLRHS);
    }
    return Builder.CreateFSub(LLLHS, LLRHS);
}

llvm::Value* arco::IRGenerator::GenMul(llvm::Value* LLLHS, llvm::Value* LLRHS, Type* Ty) {
    if (Ty->IsInt()) {
        return Builder.CreateMul(LLLHS, LLRHS);
    }
    return Builder.CreateFMul(LLLHS, LLRHS);
}

llvm::Value* arco::IRGenerator::GenDiv(llvm::Value* LLLHS, llvm::Value* LLRHS, Type* Ty) {
    if (Ty->IsInt()) {
        if (Ty->IsSigned()) {
            return Builder.CreateSDiv(LLLHS, LLRHS);
        } else {
            return Builder.CreateUDiv(LLLHS, LLRHS);
        }
    }
    return Builder.CreateFDiv(LLLHS, LLRHS);
}

void arco::IRGenerator::GenLoopCondJump(llvm::BasicBlock* LLCondBB,
                                        llvm::BasicBlock* LLBodyBB,
                                        llvm::BasicBlock* LLEndBB,
                                        Expr* Cond) {
    // Jumping directly into the loop condition
    Builder.CreateBr(LLCondBB);
    if (EmitDebugInfo) {
        if (Cond) {
            // TODO: Emit location of loop instead.
            EMIT_DI(EmitDebugLocation(Cond));
        }
    }
    Builder.SetInsertPoint(LLCondBB);

    llvm::Value* LLCond = Cond ? GenCond(Cond) : llvm::ConstantInt::getTrue(LLContext);
    Builder.CreateCondBr(LLCond, LLBodyBB, LLEndBB);
}

llvm::Value* arco::IRGenerator::GenCond(Expr* Cond) {
    llvm::Value* LLValue = GenRValue(Cond);
    if (Cond->Ty->IsPointer()) {
        return Builder.CreateIsNotNull(LLValue);
    } else {
        return LLValue;
    }
}

void arco::IRGenerator::GenBlock(llvm::BasicBlock* LLBB, ScopeStmts& Stmts) {
    if (LLBB) { // May be nullptr to allow the insertion of statements before generating the statement list.
        // Unconditionally jump into the next block.
        GenBranchIfNotTerm(LLBB);
        Builder.SetInsertPoint(LLBB);
    }
    for (AstNode* Stmt : Stmts) {
        GenNode(Stmt);
    }
}

llvm::Value* arco::IRGenerator::GenCast(Type* ToType, Type* FromType, llvm::Value* LLValue) {

    if (FromType->GetRealKind() == TypeKind::Enum) {
        if (ToType->GetKind() == TypeKind::Struct) {
            // If generating to Any then we actually need to perserve the enum information
            // rather than stripping it away.
            llvm::Value* LLAny = CreateUnseenAlloca(GenStructType(Context.StdAnyStruct), "tmp.any");
            GenToAny(LLAny, LLValue, FromType);
            return LLAny;
        }

        // KEEP static cast here because AsStructTy will strip away enum information.
        EnumDecl* Enum = static_cast<StructType*>(FromType)->GetEnum();
        FromType = Enum->ValuesType;
        if (!Enum->IndexingInOrder) {
            // Not indexing in order so we have to get the values out of a global array.
            llvm::Value* LLGlobalEnumArray = GenGlobalEnumArray(Enum);
            llvm::Value* LLIndexAddress = GetArrayIndexAddress(LLGlobalEnumArray, LLValue);
            LLValue = CreateLoad(LLIndexAddress);
        }
    }

    if (ToType->Equals(FromType)) {
        // Pointless cast.
        return LLValue;
    }

    llvm::Type* LLCastType = GenType(ToType);
    switch (ToType->GetKind()) {
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
        //  --- TO Integers ---
        if (FromType->IsInt()) {
            // Int to Int
            ulen ToSize   = ToType->GetSizeInBytes(LLModule);
            ulen FromSize = FromType->GetSizeInBytes(LLModule);

            if (ToSize < FromSize) {
                // Signed and unsigned downcasting uses trunc
                return Builder.CreateTrunc(LLValue, LLCastType);
            } else if (ToSize > FromSize) {
                if (ToType->IsSigned()) {
                    // Signed upcasting
                    return Builder.CreateSExt(LLValue, LLCastType);
                } else {
                    // Unsigned upcasting
                    return Builder.CreateZExt(LLValue, LLCastType);
                }
            } else {
                // They are actually the same type from LLVM's POV just different
                // types in the language.
                return LLValue;
            }
        } else if (FromType->IsPointer()) {
            // Ptr to Int
            return Builder.CreatePtrToInt(LLValue, LLCastType);
        } else if (FromType->GetKind() == TypeKind::Bool) {
            // Bool to Int
            if (ToType->IsSigned()) {
                // Signed upcasting
                return Builder.CreateSExt(LLValue, LLCastType);
            } else {
                // Unsigned upcasting
                return Builder.CreateZExt(LLValue, LLCastType);
            }
        } else if (FromType->IsFloat()) {
            // Float to Int
            if (ToType->IsSigned()) {
                return Builder.CreateFPToSI(LLValue, LLCastType);
            } else {
                return Builder.CreateFPToUI(LLValue, LLCastType);
            }
        }
        goto missingCaseLab;
    case TypeKind::Float32:
    case TypeKind::Float64:
        //  --- TO Floats ---
        if (FromType->IsFloat()) {
            // Float to Float
            if (ToType->GetTrivialTypeSizeInBytes() > FromType->GetTrivialTypeSizeInBytes()) {
                // Upcasting float
                return Builder.CreateFPExt(LLValue, LLCastType);
            } else {
                // Downcasting float
                return Builder.CreateFPTrunc(LLValue, LLCastType);
            }
        } else if (FromType->IsInt()) {
            // Int to Float
            if (FromType->IsSigned()) {
                return Builder.CreateSIToFP(LLValue, LLCastType);
            } else {
                return Builder.CreateUIToFP(LLValue, LLCastType);
            }
        }
        goto missingCaseLab;
    case TypeKind::Pointer: {
        //  --- TO Pointers ---
        if (FromType->GetKind() == TypeKind::Null) {
            return LLValue; // Already handled during generation
        } else if (FromType->GetKind() == TypeKind::Array) {
            if (ToType->Equals(Context.VoidPtrType)) {

                llvm::Value* LLPtrValue = MultiDimensionalArrayToPointerOnly(LLValue, FromType->AsArrayTy());
                return Builder.CreateBitCast(LLPtrValue, llvm::Type::getInt8PtrTy(LLContext));
            } else {
                return ArrayToPointer(LLValue);
            }
        } else if (FromType->IsPointer()) {
            PointerType* PtrType = ToType->AsPointerTy();
            
            if (PtrType->GetElementType()->GetKind() == TypeKind::Interface) {
                StructDecl* Struct = FromType->AsPointerTy()->GetElementType()->AsStructType()->GetStruct();
                return GenCastToInterface(PtrType, Struct, LLValue, LLCastType);
            } else if (ToType->Equals(Context.VoidPtrType)) {
                Type* FromElmType = FromType->GetPointerElementType(Context);
                if (FromElmType->GetKind() == TypeKind::Struct) {
                    StructDecl* Struct = FromElmType->AsStructType()->GetStruct();
                    if (!Struct->Interfaces.empty()) {
                        // Obtain offset past the interface data so that things such as
                        // memset work correctly.
                        ulen NumInterfaceFuncs = 0;
                        for (InterfaceDecl* Interface : Struct->Interfaces) {
                            NumInterfaceFuncs += Interface->NumFuncs;
                        }

                        llvm::StructType* LLStructTy = GenStructType(Struct);
                        // -1 because CreateStructGEP is zero index based.
                        llvm::Value* LLOffsetAddr = CreateStructGEP(LLValue, NumInterfaceFuncs - 1);
                        return Builder.CreateBitCast(LLOffsetAddr, LLCastType);
                    }
                }

                return Builder.CreateBitCast(LLValue, LLCastType);
            } else {
                // Pointer to Pointer
                
                if (FromType->Equals(Context.ErrorInterfacePtrType)) {
                    if (PtrType->GetElementType()->GetKind() == TypeKind::Struct) {
                        StructType* StructTy = PtrType->GetElementType()->AsStructType();
                        StructDecl* Struct   = StructTy->GetStruct();
                        if (Struct->ImplementsInterface(Context.StdErrorInterface)) {
                            // interface to a pointer to a struct which implements the interface.
                            // We want to subtract off the amount of virtual interface data from the pointer
                            // so that the pointer points to the beginning of that data structure instead of
                            // at the specific interface data.
                            GenStructType(Struct);

                            // Have to cast to i8* first to properly offset in bytes.
                            llvm::Value* LLBitcasted = Builder.CreateBitCast(LLValue, llvm::Type::getInt8PtrTy(LLContext));
                            
                            llvm::Value* Sub = GetSystemInt(-Struct->VirtualOffset);
                            LLBitcasted = CreateInBoundsGEP(LLBitcasted, { Sub });
                            LLBitcasted = Builder.CreateBitCast(LLBitcasted, LLCastType);
                        }
                    }
                }

                return Builder.CreateBitCast(LLValue, LLCastType);
            }
        } else if (FromType->IsInt()) {
            // Int to Ptr
            return Builder.CreateIntToPtr(LLValue, LLCastType);
        }
        goto missingCaseLab;
    }
    case TypeKind::Slice: {
        llvm::Value* LLSlice = CreateUnseenAlloca(GenType(ToType), "tmp.slice");
        GenArrayToSlice(LLSlice, LLValue, ToType, FromType);
        return LLSlice;
    }
    case TypeKind::CStr: {
        if (FromType->GetKind() == TypeKind::Null) {
            return LLValue; // Already handled during generation
        } else if (FromType->GetKind() == TypeKind::Array) {
            return ArrayToPointer(LLValue);
        } else if (FromType->IsPointer()) {
            // Pointer to Pointer
            return Builder.CreateBitCast(LLValue, LLCastType);
        } else if (FromType->IsInt()) {
            // Int to Ptr
            return Builder.CreateIntToPtr(LLValue, LLCastType);
        }
        goto missingCaseLab;
    }
    case TypeKind::Struct: {
        // This happens when casting to the Any struct.
        llvm::Value* LLAny = CreateUnseenAlloca(GenStructType(Context.StdAnyStruct), "tmp.any");
        GenToAny(LLAny, LLValue, FromType);
        return LLAny;
    }
    default: {
missingCaseLab:
        llvm::outs() << FromType->ToString() << " => " << ToType->ToString() << "\n";
        assert(!"Missing cast case");
        return nullptr;
    }
    }
}

llvm::Value* arco::IRGenerator::GenCastToInterface(PointerType* InterfacePtrTy, StructDecl* Struct, llvm::Value* LLValue, llvm::Type* LLCastType) {
    InterfaceDecl* Interface = InterfacePtrTy->GetElementType()->AsStructType()->GetInterface();
    
    ulen InterfaceOffset = 0;
    for (InterfaceDecl* InheritedInterface : Struct->Interfaces) {
        if (InheritedInterface == Interface) {
            break;
        } else {
            ++InterfaceOffset;
        }
    }

    llvm::Value* LLVTableAddr = CreateStructGEP(LLValue, InterfaceOffset);
    return Builder.CreateBitCast(LLVTableAddr, LLCastType);
}

llvm::Value* arco::IRGenerator::CreateLoad(llvm::Value* LLAddr) {
    return Builder.CreateLoad(LLAddr->getType()->getPointerElementType(), LLAddr);
}

void arco::IRGenerator::GenArrayToSlice(llvm::Value* LLSlice, llvm::Value* LLArray, Type* SliceTy, Type* ArrayTy) {
    ulen Length = ArrayTy->AsArrayTy()->GetLength();

    llvm::Value* LLLengthFieldAddr = CreateStructGEP(LLSlice, 0);
    Builder.CreateStore(GetSystemInt(Length), LLLengthFieldAddr);
    llvm::Value* LLPtrFieldAddr = CreateStructGEP(LLSlice, 1);
    Builder.CreateStore(ArrayToPointer(LLArray), LLPtrFieldAddr);

}

void arco::IRGenerator::GenToAny(llvm::Value* LLAny, llvm::Value* LLValue, Type* ValueTy) {
    
    if (ValueTy->Equals(Context.AnyType)) {
        // TODO: performance?
        Builder.CreateStore(CreateLoad(LLValue), LLAny);
        return;
    }

    llvm::Value* LLTypeFieldAddr = CreateStructGEP(LLAny, 0);
    // Just point directly to the global variable no need to copy since
    // we cannot it is declared as constant.
    Builder.CreateStore(GenTypeOfGlobal(ValueTy), LLTypeFieldAddr);
    llvm::Value* LLValueFieldAddr = CreateStructGEP(LLAny, 1);
    
    if (LLValue->getType()->isArrayTy()) {
        LLValue = ArrayToPointer(LLValue);
    } else if (!LLValue->getType()->isPointerTy()) {
        // Must have been a literal so need to generate an address to point to.
        llvm::Value* LLValueAddr = CreateUnseenAlloca(LLValue->getType(), "tmp.any.val");
        // TODO: need to check for destruction here?
        
        Builder.CreateStore(LLValue, LLValueAddr);
        LLValue = LLValueAddr;
    } else {
        // Yes it's a pointer but is it the pointer type we want.
        // a := "hello";
        // b Any = a;   -- the pointer would refer to the address of a not the pointer of a.

        // TODO: Is there a better way to do this? This seems rather hacky.
        if (ValueTy->IsPointer() && ValueTy->GetKind() != TypeKind::Function) {
            if (LLValue->getType() != GenType(ValueTy)) {
                LLValue = CreateLoad(LLValue);
            }
        }
    }

    // bitcast to void*.
    LLValue = Builder.CreateBitCast(LLValue, llvm::PointerType::get(llvm::IntegerType::getInt8Ty(LLContext), 0));

    Builder.CreateStore(LLValue, LLValueFieldAddr);

}

llvm::Constant* arco::IRGenerator::GenConstValue(Type* Ty) {
    return GenZeroedValue(Ty);
}

llvm::Constant* arco::IRGenerator::GenZeroedValue(Type* Ty) {
    switch (Ty->GetKind()) {
    case TypeKind::Int8: case TypeKind::Char:
        return GetLLInt8(0);
    case TypeKind::UInt8:    return GetLLUInt8(0);
    case TypeKind::Int16:    return GetLLInt16(0);
    case TypeKind::UInt16:   return GetLLUInt16(0);
    case TypeKind::Int32:    return GetLLInt32(0);
    case TypeKind::UInt32:   return GetLLUInt32(0);
    case TypeKind::Int64:    return GetLLInt64(0);
    case TypeKind::UInt64:   return GetLLUInt64(0);
    case TypeKind::Int:      return GetSystemInt(0);
    case TypeKind::Ptrsize:  return GetSystemUInt(0);
    case TypeKind::Bool:
        return llvm::ConstantInt::getFalse(LLContext);
    case TypeKind::Float32:
        return llvm::ConstantFP::get(LLContext, llvm::APFloat((float)0.0F));
    case TypeKind::Float64:
        return llvm::ConstantFP::get(LLContext, llvm::APFloat((double)0.0));
    case TypeKind::Pointer:
    case TypeKind::CStr:
    case TypeKind::Function:
        return llvm::Constant::getNullValue(GenType(Ty));
    case TypeKind::Array:
    case TypeKind::Struct:
    case TypeKind::Slice:
        return llvm::ConstantAggregateZero::get(GenType(Ty));
    default:
        assert(!"Failed to implement GenZeroedValue() case!");
        return nullptr;
    }
}

llvm::Value* arco::IRGenerator::GenMalloc(llvm::Type* LLType, llvm::Value* LLArrayLength) {
    
    llvm::Value* LLMalloc = llvm::CallInst::CreateMalloc(
        Builder.GetInsertBlock(),                   // llvm::BasicBlock *InsertAtEnd
        llvm::Type::getInt64Ty(LLContext),          // llvm::Type* IntPtrTy
        LLType,                                     // llvm::Type* AllocTy
        GetSystemUInt(SizeOfTypeInBytes(LLType)),   // llvm::Value* AllocSize
        LLArrayLength,
        nullptr,
        ""
    );
    Builder.Insert(LLMalloc);
    
    return LLMalloc;
}

void arco::IRGenerator::StructArrayCallDefaultConstructors(Type* BaseTy,
                                                           llvm::Value* LLArrStartPtr,
                                                           llvm::Value* LLTotalLinearLength) {
    GenInternalArrayLoop(BaseTy, LLArrStartPtr, LLTotalLinearLength,
        [this](llvm::PHINode* LLElmAddr, Type* BaseTy) {
            CallDefaultConstructor(LLElmAddr, BaseTy->AsStructType());
        });
}

void arco::IRGenerator::GenInternalArrayLoop(Type* BaseTy,
                                             llvm::Value* LLArrStartPtr,
                                             llvm::Value* LLTotalLinearLength,
                                             const std::function<void(llvm::PHINode*, Type*)>& CodeGenCallback) {
    
    llvm::BasicBlock* BeforeLoopBB = Builder.GetInsertBlock();
    llvm::Value* LLEndOfArrPtr = CreateInBoundsGEP(LLArrStartPtr, { LLTotalLinearLength });

    llvm::BasicBlock* LoopBB    = llvm::BasicBlock::Create(LLContext, "array.loop", LLFunc);
    llvm::BasicBlock* LoopEndBB = llvm::BasicBlock::Create(LLContext, "array.end", LLFunc);

    Builder.CreateBr(LoopBB);
    Builder.SetInsertPoint(LoopBB);

    // Pointer used to traverse through the array
    llvm::PHINode* LLArrPtr = Builder.CreatePHI(llvm::PointerType::get(GenType(BaseTy), 0), 0, "obj.loop.ptr");

    // Incoming value to the start of the array from the incoming block
    LLArrPtr->addIncoming(LLArrStartPtr, BeforeLoopBB);

    CodeGenCallback(LLArrPtr, BaseTy);

    // Move to the next element in the array
    llvm::Value* LLNextElementPtr = CreateInBoundsGEP(LLArrPtr, { GetSystemUInt(1) });

    // Checking if all objects have been looped over
    llvm::Value* LLLoopEndCond = Builder.CreateICmpEQ(LLNextElementPtr, LLEndOfArrPtr);
    Builder.CreateCondBr(LLLoopEndCond, LoopEndBB, LoopBB);

    // The value must come from the block that 'LLNextCount' is created
    // in which would be whatever the current block is.
    llvm::BasicBlock* LLCurBlock = Builder.GetInsertBlock();
    LLArrPtr->addIncoming(LLNextElementPtr, LLCurBlock);

    // End of loop
    Builder.SetInsertPoint(LoopEndBB);

}

llvm::Value* arco::IRGenerator::GenGlobalEnumArray(EnumDecl* Enum) {
    if (Enum->LLGlobalArray) {
        return Enum->LLGlobalArray;
    }
    
    std::string LLEnumName = "__global.enum.array." + std::to_string(Context.NumGeneratedGlobalVars++);
    
    llvm::ArrayType* LLArrType =
        llvm::ArrayType::get(GenType(Enum->ValuesType), Enum->Values.size());
    llvm::GlobalVariable* LLGlobalArray =
        GenLLVMGlobalVariable(LLEnumName, LLArrType);
    
    //llvm::Value* LLArray;
    llvm::SmallVector<llvm::Constant*, 4> LLElements;
    LLElements.resize(Enum->Values.size());
    for (const EnumDecl::EnumValue& Value : Enum->Values) {
        LLElements[Value.Index] = llvm::cast<llvm::Constant>(GenRValue(Value.Assignment));
    }

    llvm::Constant* LLConstEnumArray = llvm::ConstantArray::get(LLArrType, LLElements);
    LLGlobalArray->setInitializer(LLConstEnumArray);

    LLGlobalArray->setConstant(true);
    LLGlobalArray->setDSOLocal(true);
    
    Enum->LLGlobalArray = LLGlobalArray;
    return LLGlobalArray;
}

llvm::GlobalVariable* arco::IRGenerator::GenVTable(StructDecl* Struct) {
    
    ulen NumFuncs = 0;
    for (InterfaceDecl* Interface : Struct->Interfaces) {
        if (Interface->NumFuncs != 1) {
            NumFuncs += Interface->NumFuncs;
        }
    }
    llvm::SmallVector<llvm::Constant*> LLFuncPtrs;
    LLFuncPtrs.reserve(NumFuncs);
    llvm::Type* LLVoidPtrTy = llvm::Type::getInt8PtrTy(LLContext);
    for (InterfaceDecl* Interface : Struct->Interfaces) {
        if (Interface->NumFuncs != 1) {
            for (FuncDecl* Func : Interface->Funcs) {
                // TODO: calling GetMappedInterfaceFunc may be too slow. Although it only
                // has to go through overloaded functions so it might be okay.
                FuncDecl* FoundFunc = GetMappedInterfaceFunc(Func, Struct->Funcs[Func->Name]);
                GenFuncDecl(FoundFunc);
                llvm::Value* LLFuncPtr = Builder.CreateBitCast(FoundFunc->GetLLFunction(), LLVoidPtrTy);
                LLFuncPtrs.push_back(llvm::cast<llvm::Constant>(LLFuncPtr));
            }
        }
    }
    llvm::ArrayType* LLArrayTy = llvm::ArrayType::get(LLVoidPtrTy, NumFuncs);
    llvm::Constant* LLConstArray = llvm::ConstantArray::get(LLArrayTy, LLFuncPtrs);
    
    std::string LLGlobalVarName = std::string("__vtable.") + Struct->Name.Text.str() + "." + std::to_string(Context.NumGeneratedGlobalVars++);
    llvm::GlobalVariable* LLGlobal = GenLLVMGlobalVariable(LLGlobalVarName, LLArrayTy);
    LLGlobal->setInitializer(LLConstArray);

    return LLGlobal;
}

llvm::Value* arco::IRGenerator::DecayArray(llvm::Value* LLArray) {
    llvm::Value* LLValue = CreateInBoundsGEP(LLArray,
                { GetSystemUInt(0), GetSystemUInt(0) });
    LLValue->setName("array.decay");
    return LLValue;
}

llvm::Value* arco::IRGenerator::ArrayToPointer(llvm::Value* LLArray) {

    llvm::Type* LLType = LLArray->getType();
    
    // case 1: [n x BaseType]*    Happens when LLArray is the address to an array.
    // case 2: [n x BaseType]     Could happen if the array is a constant global array.
    if ((LLType->isPointerTy() && LLType->getPointerElementType()->isArrayTy()) ||
        LLType->isArrayTy()) {
        return DecayArray(LLArray);
    }

    // The array may be represented as:   BaseType**
    // This can happen because when arrays are passed to functions they are decayed
    // and the pointer to that array is stored in a local variable. So LLArray would
    // be the address of the variable storing the pointer to the array.
    if (LLType->isPointerTy() && LLType->getPointerElementType()->isPointerTy()) {
        return CreateLoad(LLArray);
    }

    // Already a pointer!
    return LLArray;
}

llvm::Value* arco::IRGenerator::MultiDimensionalArrayToPointerOnly(llvm::Value* LLArray, ArrayType* ArrTy) {

    llvm::Type* LLType = LLArray->getType();

    // case 1: [n x BaseType][..][n x BaseType]*    Happens when LLArray is the address to an array.
    // case 2: [n x BaseType][..][n x BaseType]     Could happen if the array is a constant global array.
    if ((LLType->isPointerTy() && LLType->getPointerElementType()->isArrayTy()) ||
        LLType->isArrayTy()) {
        
        ulen Depth = ArrTy->GetDepthLevel();
        llvm::SmallVector<llvm::Value*, 4> LLIdxs;
        for (ulen i = 0; i < Depth + 1; i++) {
            LLIdxs.push_back(GetSystemUInt(0));
        }
        return CreateInBoundsGEP(LLArray, LLIdxs);
    }
    // The array may be represented as:   BaseType**                          -> BaseType*  (good now its a pointer)
    //                                    [n x BaseType][..][n x BaseType]**  -> [n x BaseType][..][n x BaseType]* (No not what we want still have to GEP)
    // 
    // This can happen because when arrays are passed to functions they are decayed
    // and the pointer to that array is stored in a local variable. So LLArray would
    // be the address of the variable storing the pointer to the array.
    else if (LLType->isPointerTy() && LLType->getPointerElementType()->isPointerTy()) {
        llvm::Value* LLPtrValue = CreateLoad(LLArray);
        LLType = LLPtrValue->getType();
        if (LLType->isPointerTy() && LLType->getPointerElementType()->isArrayTy()) {
            // Great let's GEP into the array except with one less index.

            ulen Depth = ArrTy->GetDepthLevel();
            llvm::SmallVector<llvm::Value*, 4> LLIdxs;
            for (ulen i = 0; i < Depth; i++) {
                LLIdxs.push_back(GetSystemUInt(0));
            }
            return CreateInBoundsGEP(LLPtrValue, LLIdxs);
        } else {
            // Nothing else to GEP it's already in the form BaseType*
            return LLPtrValue;
        }
    } else {
        // Already a pointer and already check for [n x BaseType][..][n x BaseType]* case above so
        // it SHOULD be a pointer already. I forget when these type of cases even happen if I did I
        // would comment. // TODO: comment?
        return LLArray;
    }
}

inline llvm::Value* arco::IRGenerator::CreateInBoundsGEP(llvm::Value* LLAddr, llvm::ArrayRef<llvm::Value*> IdxList) {
    return Builder.CreateInBoundsGEP(
                      LLAddr->getType()->getScalarType()->getPointerElementType(),
                      LLAddr, IdxList);
}

inline llvm::Value* arco::IRGenerator::GetArrayIndexAddress(llvm::Value* LLArray, llvm::Value* LLIndex) {
    return CreateInBoundsGEP(LLArray, { GetSystemUInt(0), LLIndex });
}

inline llvm::Value* arco::IRGenerator::CreateStructGEP(llvm::Value* LLAddr, ulen Idx) {
    return Builder.CreateConstInBoundsGEP2_32(
        LLAddr->getType()->getScalarType()->getPointerElementType(), LLAddr, 0, Idx);
}

llvm::GlobalVariable* arco::IRGenerator::GenLLVMGlobalVariable(llvm::StringRef Name, llvm::Type* LLType) {
    LLModule.getOrInsertGlobal(Name, LLType);
    return LLModule.getNamedGlobal(Name);
}

llvm::GlobalVariable* arco::IRGenerator::GenConstGlobalArray(llvm::Constant* LLArray, bool DSOLocal) {

    std::string LLName = std::string("__global.array.")
                               + std::to_string(Context.NumGeneratedGlobalVars++);

    llvm::GlobalVariable* LLGlobalVar = GenLLVMGlobalVariable(LLName, LLArray->getType());
    LLGlobalVar->setInitializer(LLArray);

    LLGlobalVar->setConstant(true);
    LLGlobalVar->setDSOLocal(DSOLocal);

    return LLGlobalVar;
}

ulen arco::IRGenerator::SizeOfTypeInBytesNonVirtualInclusive(Type* Ty) {
    ulen Size = SizeOfTypeInBytes(GenType(Ty));
    if (Ty->GetKind() == TypeKind::Struct) {
        StructDecl* Struct = Ty->AsStructType()->GetStruct();
        if (!Struct->Interfaces.empty()) {
            // Need to remove the offset of the virtual interface pointers.
            return Size - Struct->VirtualOffset;
        }
    }
    return Size;
}

inline llvm::Align arco::IRGenerator::GetAlignment(llvm::Type* LLType) {
    return llvm::Align(LLModule.getDataLayout().getPrefTypeAlignment(LLType));
}

llvm::Value* arco::IRGenerator::GenReturnValueForOptimizedStructAsInt(llvm::Value* LLRetVal) {
    if (LLRetVal->getType()->isPointerTy()) {
        // Bitcast the struct type's address value to a integer pointer.
        llvm::Type* LLRetTy = LLFunc->getReturnType();
        llvm::Value* LLDestVal = Builder.CreateBitCast(LLRetVal, llvm::PointerType::get(LLRetTy, 0));
        return CreateLoad(LLDestVal);
    } else {
        // Ex.   return func();  // where function returns an optimized integer ret. value
        return LLRetVal;
    }
}

void arco::IRGenerator::GenReturnByStoreToElisionRetSlot(Expr* Value, llvm::Value* LLSlot) {
    if (Value->Is(AstKind::STRUCT_INITIALIZER)) {
        // Ex.  'return StructName{ 43, 22 };'
        GenStructInitializer(static_cast<StructInitializer*>(Value), LLSlot);
    } else if (Value->Is(AstKind::FUNC_CALL)) {
        // Ex.  'fn foo() StructName { return bar(); }
        GenFuncCall(static_cast<FuncCall*>(Value), LLSlot);
    } else {
        assert(!"Unreachable!");
    }
}

void arco::IRGenerator::CopyOrMoveStructObject(llvm::Value* LLToAddr, llvm::Value* LLFromAddr, StructDecl* Struct) {
    if (Struct->MoveConstructor) {
        MoveStructObject(LLToAddr, LLFromAddr, Struct);
    } else {
        CopyStructObject(LLToAddr, LLFromAddr, Struct);
    }
}

void arco::IRGenerator::CopyStructObject(llvm::Value* LLToAddr, llvm::Value* LLFromAddr, StructDecl* Struct) {
    // TODO: This should be changed to take into account of any of the fields have copy constructor's
    // then the default behavior should be that even if the struct doesn't have a copy constructor a compiler
    // generated copy constructor will be made which using memcpy but also calls the copy constructor
    // for the relevent fields.

    if (Struct->CopyConstructor) {
        // It has a copy constructor let's use that.
        GenFuncDecl(Struct->CopyConstructor);
        Builder.CreateCall(Struct->CopyConstructor->GetLLFunction(), { LLToAddr, LLFromAddr });
    } else {
        // Fallback on memcopy if no copy constructor.
        llvm::StructType* LLStructType =  llvm::cast<llvm::StructType>(LLFromAddr->getType()->getPointerElementType());
        const llvm::StructLayout* LLStructLayout = LLModule.getDataLayout().getStructLayout(LLStructType);
        llvm::Align LLAlignment =  LLStructLayout->getAlignment();
        // TODO: Could avoid copying interface data although it is correct either way.
        Builder.CreateMemCpy(
            LLToAddr, LLAlignment,
            LLFromAddr, LLAlignment,
            SizeOfTypeInBytes(LLStructType)
        );
    }
}

void arco::IRGenerator::MoveStructObject(llvm::Value* LLToAddr, llvm::Value* LLFromAddr, StructDecl* Struct) {
    GenFuncDecl(Struct->MoveConstructor);
    Builder.CreateCall(Struct->MoveConstructor->GetLLFunction(), { LLToAddr, LLFromAddr });
}

void arco::IRGenerator::GenConstructorBodyFieldAssignments(FuncDecl* Func, StructDecl* Struct) {
    if (!Struct->Interfaces.empty()) {
        GenCallToInitVTableFunc(LLThis, Struct);
    }
    FieldInitializingIdx = 0;
    for (VarDecl* Field : Struct->Fields) {
        llvm::Value* LLFieldAddr = CreateStructGEP(LLThis, Field->LLFieldIdx);
        if (Expr* InitValue = Func->GetInitializerValue(Field)) {
            GenAssignment(LLFieldAddr, Field->Ty, InitValue, Field->HasConstAddress);
        } else if (Field->Assignment) {
            GenAssignment(LLFieldAddr, Field->Ty, Field->Assignment, Field->HasConstAddress);
        } else if (!Field->LeaveUninitialized) {
            GenDefaultValue(Field->Ty, LLFieldAddr);
        }
        ++FieldInitializingIdx;
    }
}

void arco::IRGenerator::GenCallToInitVTableFunc(llvm::Value* LLAddr, StructDecl* Struct) {
    if (!Struct->LLInitVTableFunc) {
        Struct->LLInitVTableFunc = GenInitVTableFunc(Struct);
    }
    Builder.CreateCall(Struct->LLInitVTableFunc, { LLAddr });
}

llvm::Function* arco::IRGenerator::GenInitVTableFunc(StructDecl* Struct) {
    llvm::FunctionType* LLFuncType = llvm::FunctionType::get(
        llvm::Type::getVoidTy(LLContext),
        { llvm::PointerType::get(GenStructType(Struct), 0) },
        false
    );

    llvm::Function* LLFunc = llvm::Function::Create(
        LLFuncType,
        llvm::Function::ExternalLinkage,
        std::string("init.vtable.") + Struct->Name.Text,
        LLModule
    );
    LLFunc->setDSOLocal(true);
    llvm::Value* LLThis = LLFunc->getArg(0);

    llvm::BasicBlock* BackupInsertBlock = Builder.GetInsertBlock();
    llvm::BasicBlock* LLEntryBlock = llvm::BasicBlock::Create(LLContext, "func.entry", LLFunc);
    Builder.SetInsertPoint(LLEntryBlock);
    
    llvm::Value* LLVTable = nullptr;
    for (ulen i = 0; i < Struct->Interfaces.size(); i++) {
        llvm::Value* LLFuncPtrAddr = CreateStructGEP(LLThis, i);

        InterfaceDecl* Interface = Struct->Interfaces[i];
        if (Interface->NumFuncs == 1) {
            FuncDecl* Interfacefunc = Interface->Funcs[0];
            FuncDecl* MappedFunc    = GetMappedInterfaceFunc(Interfacefunc, Struct->Funcs[Interfacefunc->Name]);
            GenFuncDecl(MappedFunc);

            // No need to point to the VTable we can just store the function address immediately since there is only
            // one function.
            llvm::Value* LLBitCastFunc = Builder.CreateBitCast(MappedFunc->GetLLFunction(), LLFuncPtrAddr->getType()->getPointerElementType());
            Builder.CreateStore(LLBitCastFunc, LLFuncPtrAddr);
        } else {

            // Need to generate a global VTable and point to it since the interface has to select from multiple
            // function pointers.
            if (!LLVTable) {
                LLVTable = GenVTable(Struct);
            }
            
            ulen OffsetIntoVTable = 0;
            for (InterfaceDecl* InheritedInterface : Struct->Interfaces) {
                if (InheritedInterface == Interface) {
                    break;
                } else if (InheritedInterface->NumFuncs != 1) {
                    OffsetIntoVTable += InheritedInterface->NumFuncs;
                }
            }

            llvm::Value* LLPtrIntoVTable = GetArrayIndexAddress(LLVTable, GetLLUInt64(OffsetIntoVTable));
            LLPtrIntoVTable = Builder.CreateBitCast(LLPtrIntoVTable, llvm::Type::getInt8PtrTy(LLContext));
            Builder.CreateStore(LLPtrIntoVTable, LLFuncPtrAddr);

        }
    }
    
    Builder.CreateRetVoid();

    Builder.SetInsertPoint(BackupInsertBlock);

    return LLFunc;
}

std::tuple<bool, llvm::Constant*> arco::IRGenerator::GenGlobalVarInitializeValue(VarDecl* Global) {
    Type* Ty = Global->Ty;
    Expr* Assignment = Global->Assignment;
    if (Assignment) {
        // TODO: Assigned a slice?

        if (Assignment->Is(AstKind::ARRAY)) {
            // Is Array
            if (Assignment->IsFoldable) {
                // Creating a global constant array then decaying that
                // array and returning the decayed pointer to the array.
                Array*     Arr    = static_cast<Array*>(Assignment);
                ArrayType* DestTy = GetGenArrayDestType(Arr);
                if (Ty->IsPointer()) {
                    llvm::GlobalVariable* LLLGArray = GenConstGlobalArray(GenConstArray(Arr, DestTy));
                    if (Global->HasConstAddress) {
                        LLLGArray->setConstant(true);
                    }
                    return { true, llvm::cast<llvm::Constant>(DecayArray(LLLGArray)) };
                } else {
                    llvm::Constant* LLConstArr = GenConstArray(Arr, DestTy);
                    return { true, LLConstArr };
                }
            } else {
                return { false, GenZeroedValue(Ty) };
            }
        } else {
            // Not Array
            if (Assignment->IsFoldable) {
                return { true, llvm::cast<llvm::Constant>(GenRValue(Assignment)) };
            } else {
                return { false, GenZeroedValue(Ty) };
            }
        }
    } else {
        // Assignment == nullptr

        if (Ty->GetKind() == TypeKind::Struct ||
            (Ty->GetKind() == TypeKind::Array &&
             Ty->AsArrayTy()->GetBaseType()->GetKind() == TypeKind::Struct)) {
            // TODO: If foldable could get away with not calling the default constructor.
            // Need to call the default constructor.
            return { false, GenZeroedValue(Ty) };
        } else {
            return { true, GenZeroedValue(Ty) };
        }
    }
}

void arco::IRGenerator::AddObjectToDestroyOpt(Type* Ty, llvm::Value* LLAddr, llvm::Value* LLCondAddr) {
    if (Ty->TypeNeedsDestruction()) {
        AddObjectToDestroy(Ty, LLAddr, LLCondAddr);
    }
}

void arco::IRGenerator::AddObjectToDestroy(Type* Ty, llvm::Value* LLAddr, llvm::Value* LLCondAddr) {
    //  if cond {
    //     return;  
    //  } 
    //  a A;  <-- if cond is encountered then 'a' does not need to
    //            be destroyed and is therefore not put into
    //            AlwaysInitializedDestroyedObjects.
    if (!EncounteredReturn && !LocScope->Parent) {
        AlwaysInitializedDestroyedObjects.push_back({ Ty, LLAddr, LLCondAddr });
    } else {
        LocScope->ObjectsNeedingDestroyed.push_back({ Ty, LLAddr, LLCondAddr });
    }
}

void arco::IRGenerator::CallDestructors(llvm::SmallVector<DestroyObject>& Objects) {
    for (auto& DestroyObj : Objects) {
        CallDestructors(DestroyObj.Ty, DestroyObj.LLAddr, DestroyObj.LLCondAddr);
    }
}

void arco::IRGenerator::CallDestructors(Type* Ty, llvm::Value* LLAddr, llvm::Value* LLCondAddr) {
    if (Ty->GetKind() == TypeKind::Struct) {
        StructDecl* Struct = Ty->AsStructType()->GetStruct();

        llvm::BasicBlock* LLEndBB, * LLThenBB;
        if (LLCondAddr) {
            LLThenBB = llvm::BasicBlock::Create(LLContext, "if.noerr.clean.then", LLFunc);
            LLEndBB = llvm::BasicBlock::Create(LLContext, "if.noerr.clean.end", LLFunc);

            Builder.CreateCondBr(CreateLoad(LLCondAddr), LLThenBB, LLEndBB);
            Builder.SetInsertPoint(LLThenBB);
        }

        if (Struct->Destructor) {
            GenFuncDecl(Struct->Destructor);
            Builder.CreateCall(Struct->Destructor->GetLLFunction(), LLAddr);
        } else {
            // Calling compiler generated destructor.
            GenCompilerDestructorAndCall(Struct, LLAddr);
        }

        if (LLCondAddr) {
            GenBranchIfNotTerm(LLEndBB);
            Builder.SetInsertPoint(LLEndBB);
        }
    } else if (Ty->GetKind() == TypeKind::Array) {
        ArrayType* ArrayTy = Ty->AsArrayTy();

        llvm::Value* LLArrStartPtr       = MultiDimensionalArrayToPointerOnly(LLAddr, ArrayTy);
        llvm::Value* LLTotalLinearLength = GetSystemUInt(ArrayTy->GetTotalLinearLength());
        GenInternalArrayLoop(ArrayTy->GetBaseType(), LLArrStartPtr, LLTotalLinearLength,
            [this, LLCondAddr](llvm::PHINode* LLElmAddr, Type* BaseTy) {
                CallDestructors(BaseTy, LLElmAddr, LLCondAddr);
            });
    }
}

void arco::IRGenerator::GenCompilerDestructorAndCall(StructDecl* Struct, llvm::Value* LLAddr) {
    auto Itr = Context.CompilerGeneratedDestructors.find(Struct);
    if (Itr != Context.CompilerGeneratedDestructors.end()) {
        Builder.CreateCall(Itr->second, LLAddr);
        return;
    }

    llvm::Type* LLStructPtrTy = llvm::PointerType::get(GenStructType(Struct), 0);

    llvm::FunctionType* LLFuncType = llvm::FunctionType::get(
        llvm::Type::getVoidTy(LLContext), { LLStructPtrTy }, false);

    llvm::Function* LLFunc = llvm::Function::Create(
        LLFuncType,
        llvm::Function::ExternalLinkage,
        "__compiler.gen.destructor",
        LLModule
    );

    llvm::BasicBlock* LLEntryBlock = llvm::BasicBlock::Create(LLContext, "entry.block", LLFunc);
    llvm::BasicBlock* LLBackupBasicBlock = Builder.GetInsertBlock();
    Builder.SetInsertPoint(LLEntryBlock);

    for (VarDecl* Field : Struct->Fields) {
        if (Field->Ty->TypeNeedsDestruction()) {
            CallDestructors(Field->Ty, CreateStructGEP(LLFunc->getArg(0), Field->LLFieldIdx), nullptr);
        }
    }

    Builder.CreateRetVoid();

    Builder.SetInsertPoint(LLBackupBasicBlock);
    Builder.CreateCall(LLFunc, LLAddr);

    Context.CompilerGeneratedDestructors.insert({ Struct, LLFunc });
}

void arco::IRGenerator::DestroyLocScopeInitializedObjects() {
    // Only want to destroy the objects if the scope did not
    // branch because branching handles destruction.
    if (!Builder.GetInsertBlock()->getTerminator()) {
        CallDestructors(LocScope->ObjectsNeedingDestroyed);
    }
}

void arco::IRGenerator::DestroyCurrentlyInitializedObjects() {
    Scope* S = LocScope;
    while (S) {
        CallDestructors(S->ObjectsNeedingDestroyed);
        S = S->Parent;
    }
}

llvm::Value* arco::IRGenerator::GetOneValue(Type* Ty) {
    llvm::Value* LLOne = nullptr;
    switch (Ty->GetKind()) {
    case TypeKind::Int8:
    case TypeKind::Char:
        LLOne = GetLLInt8(1);
        break;
    case TypeKind::UInt8:
        LLOne = GetLLUInt8(1);
        break;
    case TypeKind::Int16:
        LLOne = GetLLInt16(1);
        break;
    case TypeKind::UInt16:
        LLOne = GetLLUInt16(1);
        break;
    case TypeKind::Int32:
        LLOne = GetLLInt32(1);
        break;
    case TypeKind::UInt32:
        LLOne = GetLLUInt32(1);
        break;
    case TypeKind::Int64:
        LLOne = GetLLInt64(1);
        break;
    case TypeKind::UInt64:
        LLOne = GetLLUInt64(1);
        break;
    case TypeKind::Int:
        LLOne = GetSystemInt(1);
        break;
    case TypeKind::Ptrsize:
        LLOne = GetSystemUInt(1);
        break;
    default: assert(!"unimplementd!"); break;
    }
    return LLOne;
}

void arco::IRGenerator::GenBranchIfNotTerm(llvm::BasicBlock* LLBB) {
    // Avoiding back-to-back branching.
    llvm::BasicBlock* CurBB = Builder.GetInsertBlock();
    if (!CurBB->getTerminator()) {
        // Unconditionally branch
        Builder.CreateBr(LLBB);
    }
}

void arco::IRGenerator::GenBranchOnCond(Expr* Cond, llvm::BasicBlock* LLTrueBB, llvm::BasicBlock* LLFalseBB) {
    // See: https://github.com/llvm/llvm-project/blob/839ac62c5085d895d3165bc5024db623a7a78813/clang/lib/CodeGen/CodeGenFunction.cpp
    // EmitBranchOnBoolExpr

    // TODO: This can be optimized for checking interfaces since interface checking requires
    //       branching.

    if (Cond->Is(AstKind::BINARY_OP)) {
        BinaryOp* BinOp = static_cast<BinaryOp*>(Cond);

        // Binary operators in the form:  a && b
        if (BinOp->Op == TokenKind::AMP_AMP) {
            if (BinOp->IsFoldable) {
                llvm::ConstantInt* LLLHS = llvm::cast<llvm::ConstantInt>(GenCond(BinOp->LHS));
                llvm::ConstantInt* LLRHS = llvm::cast<llvm::ConstantInt>(GenCond(BinOp->RHS));
                if (LLLHS->isOne() && LLRHS->isOne()) {
                    Builder.CreateBr(LLTrueBB);
                } else {
                    Builder.CreateBr(LLFalseBB);
                }
                return;
            }

            // a and b    <= if a is true go to the new 'LLLHSTrueBB' otherwise go to false block

            llvm::BasicBlock* LLLHSTrueBB = llvm::BasicBlock::Create(LLContext, "and.lhs.true", LLFunc);
            GenBranchOnCond(BinOp->LHS, LLLHSTrueBB, LLFalseBB);
            
            Builder.SetInsertPoint(LLLHSTrueBB);
            GenBranchOnCond(BinOp->RHS, LLTrueBB, LLFalseBB);
            return;
        }
        // Binary operators in the form:  a || b
        else if (BinOp->Op == TokenKind::BAR_BAR) {
            if (BinOp->IsFoldable) {
                llvm::ConstantInt* LLLHS = llvm::cast<llvm::ConstantInt>(GenCond(BinOp->LHS));
                llvm::ConstantInt* LLRHS = llvm::cast<llvm::ConstantInt>(GenCond(BinOp->RHS));
                if (LLLHS->isOne() || LLRHS->isOne()) {
                    Builder.CreateBr(LLTrueBB);
                } else {
                    Builder.CreateBr(LLFalseBB);
                }
                return;
            }

            // a or b    <= if a is true don't check b.

            llvm::BasicBlock* LLLHSFalseBB = llvm::BasicBlock::Create(LLContext, "or.lhs.false", LLFunc);
            GenBranchOnCond(BinOp->LHS, LLTrueBB, LLLHSFalseBB);

            Builder.SetInsertPoint(LLLHSFalseBB);
            GenBranchOnCond(BinOp->RHS, LLTrueBB, LLFalseBB);
            return;
        }
    }

    llvm::Value* LLCond = GenCond(Cond);
    Builder.CreateCondBr(LLCond, LLTrueBB, LLFalseBB);
}

void arco::IRGenerator::GenAssignment(llvm::Value* LLAddress,
                                      Type* AddrTy,
                                      Expr* Value,
                                      bool IsConstAddress,
                                      bool DestroyIfNeeded,
                                      const ErrorAddrList& LLErrorAddrs) {

    if (AddrTy->GetKind() == TypeKind::Struct) {
        StructDecl* Struct = AddrTy->AsStructType()->GetStruct();
        if (Struct == Context.StdAnyStruct) {
            // Assigning to Any type.
            llvm::Value* LLValue = GenNode(Value);
            GenToAny(LLAddress, LLValue, Value->Ty);
            return;
        }
    }

    if (Value->Is(AstKind::ARRAY)) {
        if (Value->CastTy && Value->CastTy->GetKind() == TypeKind::Slice) {
            llvm::Value* LLValue = GenArray(static_cast<Array*>(Value), nullptr, IsConstAddress);
            GenArrayToSlice(LLAddress, LLValue, Value->CastTy, Value->Ty);
        } else {
            llvm::Value* LLValue = GenArray(static_cast<Array*>(Value), LLAddress, IsConstAddress);
            if (Value->CastTy && Value->CastTy->IsPointer()) {
                llvm::Value* LLAssignment = GenCast(Value->CastTy, Value->Ty, LLValue);
                Builder.CreateStore(LLAssignment, LLAddress);
            }
        }
    } else if (Value->Is(AstKind::STRUCT_INITIALIZER)) {
        StructDecl* Struct = AddrTy->AsStructType()->GetStruct();

        if (Struct->Destructor && DestroyIfNeeded) {
            // Need a temporary struct so the the original memory isn't overwritten and
            // the destructor delete the original memory.
            llvm::Value* LLTempAddr =
                CreateUnseenAlloca(LLAddress->getType()->getPointerElementType(), "tmp");
            GenStructInitializer(static_cast<StructInitializer*>(Value), LLTempAddr, LLErrorAddrs);
            // Destroy the original memory.
            CallDestructors(AddrTy, LLAddress, nullptr);
            // Copy/move the new value.
            CopyOrMoveStructObject(LLAddress, LLTempAddr, Struct);
        } else {
            GenStructInitializer(static_cast<StructInitializer*>(Value), LLAddress, LLErrorAddrs);
        }
    } else if (Value->Is(AstKind::FUNC_CALL)) {

        FuncCall* Call = static_cast<FuncCall*>(Value);
        if (Call->Ty->GetKind() == TypeKind::Struct) {

            // TODO: I think this code is wrong when errors are generated since it can call copy
            // constructors of data that is invalid from the return of a call. This needs to not
            // happen.

            if (Call->CalledFunc->Struct) {
                // Fixing really obnoxious bug where when the user tries to call a member
                // function but the return value is also the address of the struct then
                // it has to make a new address as to not override the original.
                //
                // Ex.
                // 
                // S struct {
                //     fn foo() S {
                //         return *this;     
                //     }
                // }
                //
                // s S;
                // s = s.foo();
                // 
                // NOTE: We could compare addresses here but that would get too complicated because
                // we don't have access to knowing if two addresses are really equal unless we do a
                // comparison. But then that requires branching would also would not be good.

                llvm::Value* LLStructRetAddr =
                    CreateUnseenAlloca(LLAddress->getType()->getPointerElementType(), "tmp.ret");

                GenStoreStructRetFromCall(Call, LLStructRetAddr, LLErrorAddrs);
                
                // Want to call the destructor but only after having called the member function as to
                // not mess up the data of the struct but before overriding the destination address as.
                if (DestroyIfNeeded)
                    CallDestructors(AddrTy, LLAddress, nullptr);
                
                // Now we have to override the original address with the return value and since the temporary
                // object does not need to continue lasting we can use move if available.
                StructDecl* Struct = Call->CalledFunc->Struct;
                CopyOrMoveStructObject(LLAddress, LLStructRetAddr, Struct);
            } else {
                GenStoreStructRetFromCall(Call, LLAddress, LLErrorAddrs);
            }
        } else {
            llvm::Value* LLAssignment = GenFuncCall(Call, nullptr, LLErrorAddrs);
            // This must go after the call to the function because it is possible that the user still references
            // the original memory during the call. They could for example pass the value as a pointer
            // or it could be stored as a pointer in some address elsewhere which the function then uses.
            if (DestroyIfNeeded)
                CallDestructors(AddrTy, LLAddress, nullptr);
            Builder.CreateStore(LLAssignment, LLAddress);
        }
    } else if (Value->Is(AstKind::TERNARY)) {
        if (Value->Ty->GetKind() == TypeKind::Struct ||
            Value->Ty->GetKind() == TypeKind::Array
            ) {
            GenTernary(static_cast<Ternary*>(Value), LLAddress, DestroyIfNeeded);
        } else {
            Builder.CreateStore(GenTernary(static_cast<Ternary*>(Value), nullptr, false), LLAddress);
        }
    } else if (Value->Ty->GetKind() == TypeKind::Struct) { 
        StructDecl* Struct = AddrTy->AsStructType()->GetStruct();
        if (Struct->Destructor && DestroyIfNeeded) {
            // See comments for struct initializer for explaination.
            llvm::Value* LLTempAddr =
                CreateUnseenAlloca(LLAddress->getType()->getPointerElementType(), "tmp");
            if (Value->Is(AstKind::MOVEOBJ)) {
                CopyOrMoveStructObject(LLTempAddr, GenNode(static_cast<MoveObj*>(Value)), Struct);
            } else {
                CopyStructObject(LLTempAddr, GenNode(Value), Struct);
            }
            // Destroy the original memory.
            CallDestructors(AddrTy, LLAddress, nullptr);
            // Copy/move the new value.
            CopyOrMoveStructObject(LLAddress, LLTempAddr, Struct);
        } else {
            if (Value->Is(AstKind::MOVEOBJ)) {
                CopyOrMoveStructObject(LLAddress, GenNode(static_cast<MoveObj*>(Value)), Struct);
            } else {
                CopyStructObject(LLAddress, GenNode(Value), Struct);
            }
        }
    } else if (AddrTy->GetKind() == TypeKind::Slice) {
        if (Value->Ty->GetKind() == TypeKind::Array) {
            GenArrayToSlice(LLAddress, GenNode(Value), Value->CastTy, Value->Ty);
        } else {
            GenSliceToSlice(LLAddress, Value);
        }
    } else if (Value->Is(AstKind::HEAP_ALLOC)) {
        HeapAlloc* Alloc = static_cast<HeapAlloc*>(Value);
        llvm::Value* LLAssignment = GenHeapAlloc(Alloc, LLErrorAddrs);
        Builder.CreateStore(LLAssignment, LLAddress);
    } else {

        llvm::Value* LLAssignment = GenRValue(Value);
        // -- DEBUG
        // llvm::outs() << "Address Type: " << LLValTypePrinter(LLAddress) << " Assignment Type: " << LLValTypePrinter(LLAssignment) << "\n";
        Builder.CreateStore(LLAssignment, LLAddress);
    }
}

void arco::IRGenerator::GenDefaultValue(Type* Ty, llvm::Value* LLAddr) {
    if (Ty->GetKind() == TypeKind::Struct) {
        StructType* StructTy = Ty->AsStructType();
        StructDecl* Struct = StructTy->GetStruct();

        if (Struct->FieldsHaveAssignment || Struct->DefaultConstructor) {
            CallDefaultConstructor(LLAddr, StructTy);
        } else {
            llvm::StructType* LLStructTy = GenStructType(StructTy);
            ulen TotalLinearLength = SizeOfTypeInBytes(LLStructTy);
            
            // TODO: For performance may want to offset and not memset the vtable ptrs.

            llvm::Align LLAlignment = GetAlignment(LLStructTy);
            Builder.CreateMemSet(
                LLAddr,
                GetLLUInt8(0),
                GetLLUInt64(TotalLinearLength),
                LLAlignment
            );

            if (!Struct->Interfaces.empty()) {
                GenCallToInitVTableFunc(LLAddr, Struct);
            }
        }
    } else if (Ty->GetKind() == TypeKind::Array) {
        ArrayType* ArrTy = Ty->AsArrayTy();
        Type* BaseTy = ArrTy->GetBaseType();
        if (BaseTy->GetKind() == TypeKind::Struct) {
            StructDecl* Struct = BaseTy->AsStructType()->GetStruct();
            // TODO: This could be optimized to not call the default constructors in the case
            // in which there is no assignment or default constructor and instead just call the
            // code to initialize the VTable.
            if (Struct->FieldsHaveAssignment || Struct->DefaultConstructor || !Struct->Interfaces.empty()) {
                // Cannot simply memset the array to zero must call the default constructor.
                llvm::Value* LLArrStartPtr = MultiDimensionalArrayToPointerOnly(LLAddr, ArrTy);
                llvm::Value* LLTotalLinearLength = GetSystemUInt(ArrTy->GetTotalLinearLength());
                StructArrayCallDefaultConstructors(BaseTy, LLArrStartPtr, LLTotalLinearLength);
                return;
            }
        }
        
        // Memset to zero.
        ulen TotalLinearLength = ArrTy->GetTotalLinearLength();
        llvm::Type* LLBaseTy = GenType(BaseTy);
        TotalLinearLength *= SizeOfTypeInBytes(LLBaseTy);

        llvm::Align LLAlignment = GetAlignment(LLBaseTy);
        Builder.CreateMemSet(
            LLAddr,
            GetLLUInt8(0),
            GetLLUInt64(TotalLinearLength),
            LLAlignment
        );
        
    } else {
        Builder.CreateStore(GenZeroedValue(Ty), LLAddr);
    }
}

void arco::IRGenerator::GenSliceToSlice(llvm::Value* LLToAddr, Expr* Assignment) {
    // TODO: Performance: may be better to call GenNode and redirect the data
    // if it is a pointer otherwise store.
    Builder.CreateStore(GenRValue(Assignment), LLToAddr);
}

void arco::IRGenerator::CallDefaultConstructor(llvm::Value* LLAddr, StructType* StructTy) {
    llvm::Function* LLDefaultConstructor = GenDefaultConstructorDecl(StructTy->GetStruct());
    Builder.CreateCall(LLDefaultConstructor, { LLAddr });
}

llvm::Function* arco::IRGenerator::GenDefaultConstructorDecl(StructDecl* Struct) {
    if (Struct->LLDefaultConstructor) {
        return Struct->LLDefaultConstructor;
    }

    if (Struct->DefaultConstructor) {
        GenFuncDecl(Struct->DefaultConstructor);
        Struct->LLDefaultConstructor = Struct->DefaultConstructor->GetLLFunction();
        return Struct->LLDefaultConstructor;
    }

    // Compiler generated default constructor.

    llvm::FunctionType* LLFuncType = llvm::FunctionType::get(
        llvm::Type::getVoidTy(LLContext),
        { llvm::PointerType::get(GenStructType(Struct), 0) },
        false
    );

    llvm::Function* LLFunc = llvm::Function::Create(
        LLFuncType,
        llvm::Function::ExternalLinkage,
        std::string("default.constructor.") + Struct->Name.Text,
        LLModule
    );
    LLFunc->setDSOLocal(true);

    Context.DefaultConstrucorsNeedingCreated.push(Struct);

    Struct->LLDefaultConstructor = LLFunc;
    return Struct->LLDefaultConstructor;
}

llvm::Value* arco::IRGenerator::CreateUnseenAlloca(llvm::Type* LLTy, const char* Name, bool IsErrCond) {
    llvm::BasicBlock* BackupInsertBlock = Builder.GetInsertBlock();
    llvm::BasicBlock* LLEntryBlock = &LLFunc->getEntryBlock();
    if (LLEntryBlock->getInstList().empty()) {
        Builder.SetInsertPoint(LLEntryBlock);
    } else {
        Builder.SetInsertPoint(&LLEntryBlock->getInstList().front());
    }
    llvm::Value* LLAddr = Builder.CreateAlloca(LLTy, nullptr);
    if (IsErrCond) {
        Builder.CreateStore(llvm::ConstantInt::getFalse(LLContext), LLAddr);
    }
    LLAddr->setName(Name);
    Builder.SetInsertPoint(BackupInsertBlock);
    return LLAddr;
}

llvm::Value* arco::IRGenerator::GetElisionRetSlotAddr(FuncDecl* Func) {
    return Func->Struct ? Func->GetLLFunction()->getArg(1) : Func->GetLLFunction()->getArg(0);
}

llvm::Value* arco::IRGenerator::GetErrorRetAddr(ulen Idx) {
    ulen Offset = CFunc->Struct ? 1 : 0;
    Offset += CFunc->UsesParamRetSlot ? 1 : 0;
    return LLFunc->getArg(Offset + Idx);
}

void arco::IRGenerator::GenStoreStructRetFromCall(FuncCall* Call, llvm::Value* LLAddr, const ErrorAddrList& LLErrorAddrs) {
    StructDecl* Struct = Call->Ty->AsStructType()->GetStruct();
    llvm::StructType* LLStructTy = GenStructType(Call->Ty->AsStructType());

    // TODO: Shouldn't this consider also consider the case of a copy constructor?
    ulen StructByteSize = SizeOfTypeInBytes(LLStructTy);
    if (!Struct->NeedsDestruction && StructByteSize <= LLModule.getDataLayout().getPointerSize()) {
        // The return type is small enough to be shoved into an
        // integer so that returned value needs to be reinterpreted
        // as an integer to store the result.
        llvm::Value* LLRetVal = GenFuncCall(Call, nullptr, LLErrorAddrs);
        LLAddr = Builder.CreateBitCast(LLAddr, llvm::PointerType::get(LLRetVal->getType(), 0));
        Builder.CreateStore(LLRetVal, LLAddr);
    } else {
        // Needs to be passed as a parameter.
        GenFuncCall(Call, LLAddr, LLErrorAddrs);
    }
}

// For reference:
// https://github.com/llvm/llvm-project/blob/main/clang/lib/CodeGen/CGBuiltin.cpp
// https://github.com/google/swiftshader/blob/master/src/Reactor/LLVMReactor.cpp
llvm::Value* arco::IRGenerator::GenLLVMIntrinsicCall(SourceLoc CallLoc,
                                                     FuncDecl* CalledFunc,
                                                     const llvm::SmallVector<NonNamedValue>& Args,
                                                     const llvm::SmallVector<NamedValue>& NamedArgs) {
#define CALL_GET_DECL1(Name)                                          \
llvm::Value* Arg0 = GenRValue(GetArg(0));                             \
llvm::Function* LLFunc = llvm::Intrinsic::getDeclaration(			  \
            &LLModule, llvm::Intrinsic::##Name, { Arg0->getType() }); \
LLCall = Builder.CreateCall(LLFunc, Arg0);                            \
break;

#define CALL_GET_DECL2(Name)                                                           \
llvm::Value* Arg0 = GenRValue(GetArg(0));                                              \
llvm::Value* Arg1 = GenRValue(GetArg(1));                                              \
llvm::Function* LLFunc = llvm::Intrinsic::getDeclaration(                              \
            &LLModule, llvm::Intrinsic::##Name, { Arg0->getType(), Arg1->getType() }); \
LLCall = Builder.CreateCall(LLFunc, { Arg0, Arg1 });                                   \
break;

    auto GetArg = [&Args, &NamedArgs](ulen Idx) {
        if (Idx < Args.size()) {
            return Args[Idx].E;
        }
        for (const NamedValue& NamedArg : NamedArgs) {
            const VarDecl* Param = NamedArg.VarRef;
            if (Param->ParamIdx == Idx) {
                return NamedArg.AssignValue;
            }
        }
    };

    llvm::Instruction* LLCall;
    switch (CalledFunc->LLVMIntrinsicID) {
    case llvm::Intrinsic::memcpy: {
        // We know the first argument is a pointer/array type since
        // it is cast to a void* so to get the type that we are
        // aligning with we need to get the element type
        // of that pointer/array.
        Expr* Arg0 = GetArg(0);
        Expr* Arg1 = GetArg(1);
        Expr* Arg2 = GetArg(2);
        llvm::Type* LLType = GenType(Arg0->Ty->AsContainerType()->GetElementType());
        llvm::Align LLAlignment = GetAlignment(LLType);
        LLCall = Builder.CreateMemCpy(
            GenRValue(Arg0), LLAlignment,
            GenRValue(Arg1), LLAlignment,
            GenRValue(Arg2)
        );
        break;
    }
    case llvm::Intrinsic::memset: {
        // We know the first argument is a pointer/array type since
        // it is cast to a void* so to get the type that we are
        // aligning with we need to get the element type
        // of that pointer/array.
        Expr* Arg0 = GetArg(0);
        Expr* Arg1 = GetArg(1);
        Expr* Arg2 = GetArg(2);
        llvm::Type* LLType = GenType(Arg0->Ty->AsContainerType()->GetElementType());
        llvm::Align LLAlignment = GetAlignment(LLType);
        LLCall = Builder.CreateMemSet(
            GenRValue(Arg0),
            GenRValue(Arg1),
            GenRValue(Arg2),
            LLAlignment
        );
        break;
    }
    case llvm::Intrinsic::floor: {
        CALL_GET_DECL1(floor);
    }
    case llvm::Intrinsic::ceil: {
        CALL_GET_DECL1(ceil);
    }
    case llvm::Intrinsic::pow: {
        CALL_GET_DECL2(pow);
    }
    case llvm::Intrinsic::log: {
        CALL_GET_DECL1(log);
    }
    case llvm::Intrinsic::log10: {
        CALL_GET_DECL1(log10);
    }
    case llvm::Intrinsic::sqrt: {
        CALL_GET_DECL1(sqrt);
    }
    case llvm::Intrinsic::sin: {
        CALL_GET_DECL1(sin);
    }
    case llvm::Intrinsic::cos: {
        CALL_GET_DECL1(cos);
    }
    default:
    assert(!"Failed to implement intrinsic");
        break;
    }
    
    EMIT_DI(EmitDebugLocation(CallLoc));
#undef CALL_GET_DECL1
#undef CALL_GET_DECL2
    return LLCall;
}

void arco::IRGenerator::EmitDebugLocation(SourceLoc Loc) {
    if (CFunc) {
        CFunc->FScope->DIEmitter->EmitDebugLocation(Loc, Builder);
    }    
}
