#include "AST.h"

arco::Expr* arco::FuncDecl::GetInitializerValue(VarDecl* Field) {
	auto Itr = std::find_if(InitializerValues.begin(),
				            InitializerValues.end(),
	[Field](const FuncDecl::InitializerValue& InitValue) {
		return InitValue.FieldName == Field->Name;
	});
	if (Itr == InitializerValues.end()) {
		return nullptr;
	}
	return Itr->Assignment;
}