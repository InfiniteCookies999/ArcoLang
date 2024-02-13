#include "Identifier.h"

#include <llvm/ADT/DenseMap.h>

arco::Identifier::Identifier(llvm::StringRef Text) {
    static llvm::DenseMap<llvm::StringRef, ulen> IdentifierCache;
    static ulen IdentiferIDCounter = 1;

    auto Itr = IdentifierCache.find(Text);
    if (Itr != IdentifierCache.end()) {
        this->Text = Itr->first;
        ID         = Itr->second;
    } else {
        this->Text = Text;
        ID = IdentiferIDCounter++;
        IdentifierCache.insert({ this->Text, ID });
    }
}

llvm::raw_ostream& llvm::operator<<(raw_ostream& OS, const arco::Identifier& Ident) {
    OS << Ident.Text;
    return OS;
}
