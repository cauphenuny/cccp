#ifndef AST_HPP
#define AST_HPP

#include "ast/ast.h"

#include "ir/ir.h"
#include "util.hpp"

#include <map>
#include <string>
#include <variant>

class BaseAST;

using SymbolTable = std::map<std::string, std::variant<BaseAST*, int>>;

using AstObject = std::unique_ptr<BaseAST>;

BaseAST::BaseAST(int line, int column) : line(line), column(column) {}

FuncTypeAST::FuncTypeAST(int line, int column) : BaseAST(line, column) {}
std::string FuncTypeAST::toString() const {
    return serializeClass("FuncTypeAST", type);
}

VarTypeAST::VarTypeAST(int line, int column) : BaseAST(line, column) {}
std::string VarTypeAST::toString() const {
    return serializeClass("VarTypeAST", type);
}

std::string NumberAST::toString() const {
    return serializeClass("NumberAST", number);
}
IrObject NumberAST::toIR() const {
    return std::make_unique<ValueIR>(Inst::Integer, std::to_string(number));
}
bool NumberAST::isConstExpr() const {
    return true;
}
int NumberAST::calc() const {
    return number;
}
NumberAST::NumberAST(int num, int line, int column) : BaseAST(line, column), number(num) {}

std::string ExpAST::toString() const {
    return exp->toString();
}
ExpAST::ExpAST(AstObject&& _exp) : BaseAST(_exp->line, _exp->column), exp(std::move(_exp)) {
    exp->parent = this;
}
bool ExpAST::isConstExpr() const {
    return exp->isConstExpr();
}
int ExpAST::calc() const {
    return exp->calc();
}
IrObject ExpAST::toIR() const {
    if (isConstExpr()) return NumberAST(exp->calc()).toIR();
    return exp->toIR();
}

LValAST::LValAST(int line, int column) : BaseAST(line, column) {}
std::string LValAST::getName() const {
    auto scope = parent;
    while (scope) {
        auto& map = scope->symbol_table;
        if (map && map->find(ident) != map->end()) {
            return std::format("@{}_{}", ident, scope->scope_id);
        }
        scope = scope->parent;
    }
    throw compileError("{}:{}: undefined variable: {}", line, column, ident);
}
std::variant<BaseAST*, int> LValAST::getValue() const {
    auto scope = parent;
    while (scope) {
        auto& map = scope->symbol_table;
        if (map && map->find(ident) != map->end()) {
            return map->at(ident);
        }
        scope = scope->parent;
    }
    throw compileError("{}:{}: undefined variable: {}", line, column, ident);
}
bool LValAST::isConstExpr() const {
    auto value = getValue();
    return std::holds_alternative<int>(value);
}
int LValAST::calc() const {
    auto value = getValue();
    if (std::holds_alternative<BaseAST*>(value))
        throw compileError("{}:{}: not constant variable: {}", line, column, ident);
    return std::get<int>(value);
}
std::string LValAST::toString() const {
    return serializeClass("LValAST", ident);
}
IrObject LValAST::toIR() const {
    auto value = getValue();
    if (std::holds_alternative<int>(value)) {
        return NumberAST(std::get<int>(value)).toIR();
    } else {
        return std::make_unique<ValueIR>(Inst::Load, getName());
    }
}

PrimaryExpAST::PrimaryExpAST(Type type, AstObject&& obj)
    : BaseAST(obj->line, obj->column), type(type), content(std::move(obj)) {
    content->parent = this;
}
std::string PrimaryExpAST::toString() const {
    switch (type) {
        case Exp: return serializeClass("PrimaryExpAST", content);
        default: return content->toString();
    }
}
bool PrimaryExpAST::isConstExpr() const {
    return content->isConstExpr();
}
IrObject PrimaryExpAST::toIR() const {
    return content->toIR();
}
int PrimaryExpAST::calc() const {
    return content->calc();
}

UnaryExpAST::UnaryExpAST(int line, int column) : BaseAST(line, column) {}
UnaryExpAST::UnaryExpAST(AstObject&& obj)
    : BaseAST(obj->line, obj->column), content(std::move(obj)) {
    auto& virtual_exp = std::get<AstObject>(content);
    virtual_exp->parent = this;
}
UnaryExpAST::UnaryExpAST(Container&& cont)
    : BaseAST(cont.unary_exp->line, cont.unary_exp->column), content(std::move(cont)) {
    auto& real_exp = std::get<Container>(content);
    real_exp.unary_exp->parent = this;
}

bool UnaryExpAST::isConstExpr() const {
    return Match{content}(  // for clang-format split line
        [](const AstObject& obj) { return obj->isConstExpr(); },
        [](const Container& ctn) { return ctn.unary_exp->isConstExpr(); });
}

std::string UnaryExpAST::toString() const {
    return Match{content}(  //
        [](const AstObject& obj) { return obj->toString(); },
        [](const Container& ctn) {
            auto& [op, exp] = ctn;
            return serializeClass("UnaryExpAST", op, exp);
        });
}

IrObject UnaryExpAST::toIR() const {
    return Match{content}(  //
        [](const AstObject& obj) { return obj->toIR(); },
        [](const Container& ctn) -> IrObject {
            auto& [op, exp] = ctn;
            auto ir = std::make_unique<ValueIR>(Inst::Binary);
            switch (op) {
                case Operator::add: return exp->toIR();
                case Operator::sub:
                case Operator::no:
                    ir->content = op == Operator::sub ? "sub" : "eq";
                    ir->params.push_back(NumberAST(0).toIR());
                    ir->params.push_back(exp->toIR());
                    return ir;
                default: throw runtimeError("invalid operator {} for unary expression", op);
            }
        });
}

int UnaryExpAST::calc() const {
    return Match{content}(  //
        [](const AstObject& obj) { return obj->calc(); },
        [](const Container& ctn) {
            auto& [op, exp] = ctn;
            return getFunction<int>(op)(0, exp->calc());
        });
}

BinaryExpAST::BinaryExpAST(AstObject&& obj)
    : BaseAST(obj->line, obj->column), content(std::move(obj)) {
    auto& virtual_exp = std::get<AstObject>(content);
    virtual_exp->parent = this;
}
BinaryExpAST::BinaryExpAST(int line, int column, Container&& cont)
    : BaseAST(line, column), content(std::move(cont)) {
    auto& real_exp = std::get<Container>(content);
    real_exp.left->parent = this;
    real_exp.right->parent = this;
}
bool BinaryExpAST::isConstExpr() const {
    return Match{content}(  //
        [](const AstObject& obj) { return obj->isConstExpr(); },
        [](const Container& ctn) { return ctn.left->isConstExpr() && ctn.right->isConstExpr(); });
}
std::string BinaryExpAST::toString() const {
    return Match{content}(  //
        [](const AstObject& obj) { return obj->toString(); },
        [&](const Container& ctn) {
            auto& [left, op, right] = ctn;
            return serializeClass("BinaryExpAST", op, left, right);
        });
}
IrObject BinaryExpAST::toIR() const {
    return Match{content}(  //
        [](const AstObject& obj) { return obj->toIR(); },
        [&](const Container& ctn) -> IrObject {
            auto ir = std::make_unique<MultiValueIR>();
            auto mkvalue = [](auto&&... params) { return std::make_unique<ValueIR>(params...); };
            if (isConstExpr()) {
                return NumberAST(calc()).toIR();
            } else {
                auto& [left, op, right] = ctn;
                if (op != Operator::lor && op != Operator::land) {
                    ir->add(Inst::Binary, toIrOperatorName(op), left->toIR(), right->toIR());
                } else {
                    std::string right_label = std::format("rightexp_{}_{}", line, column);
                    std::string end_label = std::format("endexp_{}_{}", line, column);
                    std::string result = std::format("%result_{}_{}", line, column);
                    ir->add(Inst::Alloc, result);
                    ir->add(Inst::Store, result,
                            mkvalue(Inst::Integer, op == Operator::lor ? "1" : "0"));
                    if (op == Operator::lor) {
                        ir->add(Inst::Branch, "", left->toIR(), mkvalue(end_label),
                                mkvalue(right_label));
                    } else if (op == Operator::land) {
                        ir->add(Inst::Branch, "", left->toIR(), mkvalue(right_label),
                                mkvalue(end_label));
                    }
                    ir->add(Inst::Label, right_label);
                    ir->add(Inst::Store, result, right->toIR());
                    ir->add(Inst::Jump, end_label);
                    ir->add(Inst::Label, end_label);
                    ir->add(Inst::Load, result);
                }
            }
            return ir;
        });
}
int BinaryExpAST::calc() const {
    return Match{content}(  //
        [](const AstObject& obj) { return obj->calc(); },
        [](const Container& ctn) {
            auto& [left, op, right] = ctn;
            return getFunction<int>(op)(left->calc(), right->calc());
        });
}

ConstDefAST::ConstDefAST(int line, int column) : BaseAST(line, column) {}
ConstDefAST::ConstDefAST(const std::string& ident, AstObject&& init_exp)
    : BaseAST(init_exp->line, init_exp->column), ident(ident), init_exp(std::move(init_exp)) {}
void ConstDefAST::writeSymbol() const {
    if (!init_exp->isConstExpr()) {
        throw compileError("{}:{}: initializer is not a constant expression", line, column);
    }
    for (auto scope = parent; scope; scope = scope->parent) {
        if (auto& map = scope->symbol_table) {
            if (map->find(ident) != map->end())
                throw compileError("{}:{}: redefined variable: {}", line, column, ident);
            (*map)[ident] = init_exp->calc();
            return;
        }
    }
    throw compileError("{}:{}: no available scope for variable {}", line, column, ident);
}
std::string ConstDefAST::toString() const {
    return serializeClass("ConstDefAST", ident, init_exp);
}
IrObject ConstDefAST::toIR() const {
    writeSymbol();
    return nullptr;
}

ConstDeclAST::ConstDeclAST(int line, int column) : BaseAST(line, column) {}
std::string ConstDeclAST::toString() const {
    return serializeClass("ConstDeclAST", type, const_defs);
}
IrObject ConstDeclAST::toIR() const {
    for (const auto& const_def : const_defs) {
        const_def->toIR();
    }
    return nullptr;
}

VarDefAST::VarDefAST(int line, int column) : BaseAST(line, column) {}

VarDefAST::VarDefAST(const std::string& ident, AstObject&& init_exp)
    : BaseAST(init_exp->line, init_exp->column), ident(ident), init_exp(std::move(init_exp)) {}

std::string VarDefAST::toString() const {
    if (init_exp)
        return serializeClass("VarDefAST", ident, init_exp);
    else
        return serializeClass("VarDefAST", ident);
}

void VarDefAST::writeSymbol() const {
    for (auto scope = parent; scope; scope = scope->parent) {
        if (auto& map = scope->symbol_table) {
            if (map->find(ident) != map->end())
                throw compileError("{}:{}: redefined variable: {}", line, column, ident);
            (*map)[ident] = init_exp.get();
            return;
        }
    }
    throw compileError("{}:{}: no available scope for variable {}", line, column, ident);
}

std::string VarDefAST::getName() const {
    auto scope = parent;
    while (scope) {
        auto& map = scope->symbol_table;
        if (map && map->find(ident) != map->end()) {
            return std::format("@{}_{}", ident, scope->scope_id);
        }
        scope = scope->parent;
    }
    throw compileError("{}:{}: undefined variable: {}", line, column, ident);
}

IrObject VarDefAST::toIR() const {
    writeSymbol();
    auto ir = std::make_unique<MultiValueIR>();
    std::string mangled_ident = getName();
    auto inst1 = std::make_unique<ValueIR>(Inst::Alloc, mangled_ident);
    inst1->type->tag = BaseIR::Type::Tag::Int32;
    ir->add(std::move(inst1));
    if (init_exp) {
        auto inst2 = std::make_unique<ValueIR>(Inst::Store, mangled_ident);
        inst2->params.push_back(init_exp->toIR());
        ir->add(std::move(inst2));
    }
    return ir;
}

VarDeclAST::VarDeclAST(int line, int column) : BaseAST(line, column) {}

std::string VarDeclAST::toString() const {
    return serializeClass("VarDeclAST", type, var_defs);
}

IrObject VarDeclAST::toIR() const {
    auto ir = std::make_unique<MultiValueIR>();
    for (const auto& var_def : var_defs) {
        ir->add(var_def->toIR());
    }
    return ir;
}

DeclAST::DeclAST(int line, int column) : BaseAST(line, column) {}

DeclAST::DeclAST(AstObject&& decl) : BaseAST(decl->line, decl->column), decl(std::move(decl)) {}

std::string DeclAST::toString() const {
    return serializeClass("DeclAST", decl);
}

IrObject DeclAST::toIR() const {
    return decl->toIR();
}

StmtAST::StmtAST(int line, int column, Type type, AstObject&& _content)
    : BaseAST(line, column), type(type) {
    if (_content) _content->parent = this;
    content = std::move(_content);
}

StmtAST::StmtAST(int line, int column, AssignContainer&& assign_stmt)
    : BaseAST(line, column), type(Assign) {
    assign_stmt.lval->parent = assign_stmt.exp->parent = this;
    content = std::move(assign_stmt);
}

StmtAST::StmtAST(int line, int column, IfContainer&& if_stmt) : BaseAST(line, column), type(If) {
    if_stmt.exp->parent = this;
    if (if_stmt.then_stmt) if_stmt.then_stmt->parent = this;
    if (if_stmt.else_stmt) if_stmt.else_stmt->parent = this;
    content = std::move(if_stmt);
}

StmtAST::StmtAST(int line, int column, WhileContainer&& while_stmt)
    : BaseAST(line, column), type(While) {
    while_stmt.exp->parent = this;
    if (while_stmt.stmt) while_stmt.stmt->parent = this;
    content = std::move(while_stmt);
}

std::string StmtAST::toString() const {
    return Match{content}(
        [&](const AstObject& obj) {
            if (obj)
                return serializeClass("StmtAST", type, obj);
            else
                return serializeClass("StmtAST", type);
        },
        [&](const AssignContainer& ctn) {
            auto& [lval, exp] = ctn;
            return serializeClass("StmtAST", type, lval, exp);
        },
        [&](const IfContainer& ctn) {
            auto& [exp, then_stmt, else_stmt] = ctn;
            return serializeClass("StmtAST", type, exp, then_stmt, else_stmt);
        },
        [&](const WhileContainer& ctn) {
            auto& [exp, stmt] = ctn;
            return serializeClass("StmtAST", type, exp, stmt);
        });
}

IrObject StmtAST::toIR() const {
    auto new_value = [](auto&&... params) { return std::make_unique<ValueIR>(params...); };
    auto ir = std::make_unique<MultiValueIR>();
    switch (type) {
        case Expr:
        case Block: {
            if (const auto& obj_content = std::get<AstObject>(content))
                return obj_content->toIR();
            else
                return nullptr;
        }
        case Return: {
            ir->add(Inst::Return, "ret", std::get<AstObject>(content)->toIR());
            return ir;
        }
        case Assign: {
            auto& [raw_lval, raw_exp] = std::get<AssignContainer>(content);
            auto lval = dynamic_cast<LValAST*>(raw_lval.get());
            auto exp = dynamic_cast<ExpAST*>(raw_exp.get());
            ir->add(Inst::Store, lval->getName(), exp->toIR());
            return ir;
        }
        case If: {
            auto& [exp, then_stmt, else_stmt] = std::get<IfContainer>(content);
            std::string then_label = std::format("if_then_{}_{}", line, column),
                        else_label = std::format("if_else_{}_{}", line, column),
                        end_label = std::format("if_end_{}_{}", line, column);
            ir->add(Inst::Branch, "", exp->toIR(), new_value(then_label),
                    new_value(else_stmt ? else_label : end_label));
            ir->add(Inst::Label, then_label);
            if (then_stmt) ir->add(then_stmt->toIR());
            ir->add(Inst::Jump, end_label);
            if (else_stmt) {
                ir->add(Inst::Label, else_label);
                ir->add(else_stmt->toIR());
                ir->add(Inst::Jump, end_label);
            }
            ir->add(Inst::Label, end_label);
            return ir;
        }
        case While: {
            auto& [exp, stmt] = std::get<WhileContainer>(content);
            std::string entry_label = std::format("while_entry_{}_{}", line, column),
                        body_label = std::format("while_body_{}_{}", line, column),
                        end_label = std::format("while_end_{}_{}", line, column);

            ir->add(Inst::Jump, entry_label);
            ir->add(Inst::Label, entry_label);
            ir->add(Inst::Branch, "", exp->toIR(), new_value(body_label), new_value(end_label));

            ir->add(Inst::Label, body_label);
            if (stmt) ir->add(stmt->toIR());
            ir->add(Inst::Jump, entry_label);

            ir->add(Inst::Label, end_label);
            return ir;
        }
        case Break: {
            StmtAST* while_block = nullptr;
            for (auto p = parent; p; p = p->parent) {
                if (auto block = dynamic_cast<StmtAST*>(p)) {
                    if (block->type == StmtAST::While) {
                        while_block = block;
                        break;
                    }
                }
            }
            if (!while_block) throw compileError("break statement should be in a loop");
            ir->add(Inst::Jump,
                    std::format("while_end_{}_{}", while_block->line, while_block->column));
            return ir;
        }
        case Continue: {
            StmtAST* while_block = nullptr;
            for (auto p = parent; p; p = p->parent) {
                if (auto block = dynamic_cast<StmtAST*>(p)) {
                    if (block->type == StmtAST::While) {
                        while_block = block;
                        break;
                    }
                }
            }
            if (!while_block) throw compileError("continue statement should be in a loop");
            ir->add(Inst::Jump,
                    std::format("while_entry_{}_{}", while_block->line, while_block->column));
            return ir;
        }
        default: {
            throw runtimeError("unimplemented statement type `{}`", type);
        }
    }
}

std::string toString(StmtAST::Type t) {
    switch (t) {
        case StmtAST::Return: return "Return";
        case StmtAST::Assign: return "Assign";
        case StmtAST::Block: return "Block";
        case StmtAST::Expr: return "Expr";
        case StmtAST::If: return "If";
        case StmtAST::While: return "While";
        case StmtAST::Break: return "Break";
        case StmtAST::Continue: return "Continue";
        default: assert(false && "invalid statement type");
    }
}

BlockItemAST::BlockItemAST(Type type, AstObject&& _content)
    : BaseAST(_content->line, _content->column), type(type), content(std::move(_content)) {
    content->parent = this;
}
std::string BlockItemAST::toString() const {
    return serializeClass("BlockItemAST", type, content);
}
std::string toString(BlockItemAST::Type t) {
    return t == BlockItemAST::Decl ? "Decl" : "Stmt";
}
IrObject BlockItemAST::toIR() const {
    return content->toIR();
}

static int scope_id_tot;

BlockAST::BlockAST(int line, int column) : BaseAST(line, column) {
    symbol_table = std::make_unique<SymbolTable>();
    scope_id = scope_id_tot++;
}
std::string BlockAST::toString() const {
    return serializeClass("BlockAST", stmts);
}
IrObject BlockAST::toIR() const {
    if (scope_id < 0) throw runtimeError("scope id unspecified");
    auto ir = std::make_unique<MultiValueIR>();
    for (const auto& stmt : stmts) {
        ir->add(stmt->toIR());
    }
    symbol_table->clear();
    return ir;
}

FuncDefAST::FuncDefAST(int line, int column) : BaseAST(line, column) {}
FuncDefAST::FuncDefAST(const std::string& ident, AstObject&& block)
    : BaseAST(block->line, block->column), ident(ident), block(std::move(block)) {}
std::string FuncDefAST::toString() const {
    return serializeClass("FuncDefAST", func_type, ident, block);
}
IrObject FuncDefAST::toIR() const {
    scope_id_tot = 0;
    auto ir = std::make_unique<FunctionIR>(ident);
    auto block_ir = std::make_unique<MultiValueIR>();
    block_ir->add(Inst::Label, "");
    block_ir->add(block->toIR());
    ir->blocks = std::move(block_ir);
    return ir;
}

CompUnitAST::CompUnitAST(int line, int column) : BaseAST(line, column) {}
std::string CompUnitAST::toString() const {
    return serializeClass("CompUnitAST", func_def);
}
IrObject CompUnitAST::toIR() const {
    auto ir = std::make_unique<ProgramIR>();
    ir->funcs.push_back(func_def->toIR());
    return ir;
}

#endif
