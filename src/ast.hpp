#ifndef AST_HPP
#define AST_HPP

#include "ir.hpp"
#include "util.hpp"

#include <iostream>
#include <map>
#include <memory>
#include <optional>
#include <string>
#include <variant>

class BaseAST;

using SymbolTable = std::map<std::string, std::variant<BaseAST*, int>>;

using AstObject = std::unique_ptr<BaseAST>;

class BaseAST {
public:
    int line, column;
    std::unique_ptr<SymbolTable> symbol_table;
    BaseAST* parent;
    BaseAST() = delete;
    BaseAST(int line, int column) : line(line), column(column) {}
    virtual ~BaseAST() = default;
    virtual IrObject toIr() const {
        throw runtimeError("{}:{}: can not convert {} to IR", line, column, typeid(*this).name());
    }
    virtual bool isConstexpr() const { return false; }
    virtual int calc() const {
        throw runtimeError("{}:{}: can not calculate {}", line, column, typeid(*this).name());
    }
    virtual std::string toString() const = 0;
    explicit operator std::string() const { return toString(); }
    friend std::ostream& operator<<(std::ostream& os, const BaseAST& ast) {
        os << ast.toString();
        return os;
    }
};

class CompUnitAST : public BaseAST {
public:
    AstObject func_def;
    CompUnitAST(int line, int column) : BaseAST(line, column) {}
    std::string toString() const override { return serializeClass("CompUnitAST", func_def); }
    IrObject toIr() const override {
        auto ir = std::make_unique<ProgramIR>();
        ir->funcs.push_back(func_def->toIr());
        return ir;
    }
};

class FuncDefAST : public BaseAST {
public:
    AstObject func_type;
    std::string ident;
    AstObject block;
    FuncDefAST(int line, int column) : BaseAST(line, column) {}
    std::string toString() const override {
        return serializeClass("FuncDefAST", func_type, ident, block);
    }
    IrObject toIr() const override {
        auto ir = std::make_unique<FunctionIR>();
        ir->name = ident;
        ir->ret_type = func_type->toIr();
        ir->blocks.push_back(block->toIr());
        return ir;
    }
};

class FuncTypeAST : public BaseAST {
public:
    FuncTypeAST(int line, int column) : BaseAST(line, column) {}
    std::string type;
    std::string toString() const override { return serializeClass("FuncTypeAST", type); }
    IrObject toIr() const override {
        auto ir = std::make_unique<ValueIR>();
        if (type == "int") {
            ir->type = ValueType::Type;
            ir->content = "i32";
        } else {
            ir->type = ValueType::Unknown;
            ir->content = type;
        }
        return ir;
    }
};

class VarTypeAST : public BaseAST {
public:
    VarTypeAST(int line, int column) : BaseAST(line, column) {}
    std::string type;
    std::string toString() const override { return serializeClass("VarTypeAST", type); }
    IrObject toIr() const override {
        auto ir = std::make_unique<ValueIR>();
        if (type == "int") {
            ir->type = ValueType::Type;
            ir->content = "i32";
        } else {
            ir->type = ValueType::Unknown;
            ir->content = type;
        }
        return ir;
    }
};

class NumberAST : public BaseAST {
public:
    int number;
    std::string toString() const override { return serializeClass("NumberAST", number); }
    IrObject toIr() const override {
        auto ir = std::make_unique<ValueIR>();
        ir->type = ValueType::Integer;
        ir->content = std::to_string(number);
        return ir;
    }
    bool isConstexpr() const override { return true; }
    int calc() const override { return number; }
    NumberAST(int num, int line = -1, int column = -1) : BaseAST(line, column), number(num) {}
};

class ExpAST : public BaseAST {
public:
    AstObject exp;
    std::string toString() const override { return exp->toString(); }
    ExpAST(AstObject&& _exp) : BaseAST(_exp->line, _exp->column), exp(std::move(_exp)) {
        exp->parent = this;
    }
    bool isConstexpr() const override { return exp->isConstexpr(); }
    int calc() const override { return exp->calc(); }
    IrObject toIr() const override {
        if (isConstexpr()) return NumberAST(exp->calc()).toIr();
        return exp->toIr();
    }
};

class LValAST : public BaseAST {
public:
    LValAST(int line, int column) : BaseAST(line, column) {}
    std::string ident;
    std::optional<std::variant<BaseAST*, int>> get_value() const {
        auto scope = parent;
        while (scope) {
            auto& map = scope->symbol_table;
            if (map && map->find(ident) != map->end()) {
                return map->at(ident);
            }
            scope = scope->parent;
        }
        return std::nullopt;
    }
    bool isConstexpr() const override {
        auto value = get_value();
        return value.has_value() && std::holds_alternative<int>(value.value());
    }
    int calc() const override {
        auto value = get_value();
        if (!value.has_value()) {
            throw runtimeError("{}:{}: undefined variable: {}", line, column, ident);
        }
        if (std::holds_alternative<BaseAST*>(value.value()))
            throw runtimeError("{}:{}: not constant variable: {}", line, column, ident);
        return std::get<int>(value.value());
    }
    std::string toString() const override { return serializeClass("LValAST", ident); }
    IrObject toIr() const override {
        auto value = get_value();
        if (!value.has_value()) {
            throw runtimeError("{}:{}: undefined variable: {}", line, column, ident);
        }
        if (std::holds_alternative<int>(value.value())) {
            return NumberAST(std::get<int>(value.value())).toIr();
        } else {
            return std::make_unique<ValueIR>(ValueType::Variable, ident);
        }
    }
};

class PrimaryExpAST : public BaseAST {
public:
    enum Type {
        Number,  // Number
        Exp,     // "(" Exp ")"
        LVal,    // Lval
    } type;
    AstObject content;
    PrimaryExpAST(int line, int column) : BaseAST(line, column) {}
    PrimaryExpAST(Type type, AstObject&& obj)
        : BaseAST(obj->line, obj->column), type(type), content(std::move(obj)) {
        content->parent = this;
    }
    std::string toString() const override {
        switch (type) {
            case Exp: return serializeClass("PrimaryExpAST", content);
            default: return content->toString();
        }
    }
    bool isConstexpr() const override { return content->isConstexpr(); }
    IrObject toIr() const override { return content->toIr(); }
    int calc() const override { return content->calc(); }
};

class UnaryExpAST : public BaseAST {
public:
    enum {
        Virtual,  // PrimaryExp
        Real,     // UnaryOp UnaryExp
    } type;
    struct Container {
        Operator unary_op;
        AstObject unary_exp;
    };
    std::variant<AstObject, Container> content;
    UnaryExpAST(int line, int column) : BaseAST(line, column) {}
    UnaryExpAST(AstObject&& obj)
        : BaseAST(obj->line, obj->column), type(Virtual), content(std::move(obj)) {
        auto& virtual_exp = std::get<AstObject>(content);
        virtual_exp->parent = this;
    }
    UnaryExpAST(Container&& cont)
        : BaseAST(cont.unary_exp->line, cont.unary_exp->column), type(Real),
          content(std::move(cont)) {
        auto& real_exp = std::get<Container>(content);
        real_exp.unary_exp->parent = this;
    }

    bool isConstexpr() const override {
        switch (type) {
            case Virtual: return std::get<AstObject>(content)->isConstexpr();
            case Real: return std::get<Container>(content).unary_exp->isConstexpr();
        }
        throw runtimeError("invalid unary expression");
    }

    std::string toString() const override {
        switch (type) {
            case Virtual: return std::get<AstObject>(content)->toString();
            case Real:
                auto& [op, exp] = std::get<Container>(content);
                return serializeClass("UnaryExpAST", op, exp);
        }
        throw runtimeError("invalid unary expression");
    }

    IrObject toIr() const override {
        switch (type) {
            case Virtual: return std::get<AstObject>(content)->toIr();
            case Real: {
                auto& [op, exp] = std::get<Container>(content);
                auto ir = std::make_unique<ValueIR>();
                switch (op) {
                    case Operator::add: return exp->toIr();
                    case Operator::sub:
                    case Operator::no:
                        ir->type = ValueType::Binary;
                        ir->content = op == Operator::sub ? "sub" : "eq";
                        ir->params.push_back(NumberAST(0).toIr());
                        ir->params.push_back(exp->toIr());
                        return ir;
                    default:
                        throw runtimeError("invalid operator {} for unary expression",
                                           toRawOperator(op));
                }
            }
        }
        throw runtimeError("invalid unary expression");
    }

    int calc() const override {
        switch (type) {
            case Virtual: return std::get<AstObject>(content)->calc();
            case Real: {
                auto& [op, exp] = std::get<Container>(content);
                return getFunction<int>(op)(0, exp->calc());
            }
        }
        throw runtimeError("invalid unary expression");
    }
};

class BinaryExpAST : public BaseAST {
public:
    enum type_t {
        Virtual,
        Real,
    } type;
    struct Container {
        AstObject left;
        Operator op;
        AstObject right;
    };
    std::variant<AstObject, Container> content;
    BinaryExpAST(int line, int column) : BaseAST(line, column) {}
    BinaryExpAST(AstObject&& obj)
        : BaseAST(obj->line, obj->column), type(Virtual), content(std::move(obj)) {
        auto& virtual_exp = std::get<AstObject>(content);
        virtual_exp->parent = this;
    }
    BinaryExpAST(Container&& cont)
        : BaseAST(cont.left->line, cont.left->column), type(Real), content(std::move(cont)) {
        auto& real_exp = std::get<Container>(content);
        real_exp.left->parent = this;
        real_exp.right->parent = this;
    }
    bool isConstexpr() const override {
        switch (type) {
            case Virtual: return std::get<AstObject>(content)->isConstexpr();
            case Real:
                auto& [left, op, right] = std::get<Container>(content);
                return left->isConstexpr() && right->isConstexpr();
        }
        throw runtimeError("invalid binary expression");
    }
    std::string toString() const override {
        switch (type) {
            case Virtual: return std::get<AstObject>(content)->toString();
            case Real:
                auto& [left, op, right] = std::get<Container>(content);
                return serializeClass("BinaryExpAST", op, left, right);
        }
        throw runtimeError("invalid binary expression");
    }
    IrObject toIr() const override {
        switch (type) {
            case Virtual: return std::get<AstObject>(content)->toIr();
            case Real: {
                auto& [left, op, right] = std::get<Container>(content);
                auto ir = std::make_unique<ValueIR>();
                ir->type = ValueType::Binary;
                ir->content = toIrOperatorName(op);
                ir->params.push_back(left->toIr());
                ir->params.push_back(right->toIr());
                return ir;
            }
        }
        throw runtimeError("invalid binary expression");
    }
    int calc() const override {
        switch (type) {
            case Virtual: return std::get<AstObject>(content)->calc();
            case Real: {
                auto& [left, op, right] = std::get<Container>(content);
                return getFunction<int>(op)(left->calc(), right->calc());
            }
        }
        throw runtimeError("invalid binary expression");
    }
};

class StmtAST : public BaseAST {
public:
    enum type_t {
        Return,
        Assign,
    } type;
    std::string toString(type_t t) { return t == Return ? "Return" : "Assign"; }
    struct AssignContainer {
        AstObject lval;
        AstObject exp;
    };
    std::variant<AstObject, AssignContainer> content;
    StmtAST(int line, int column) : BaseAST(line, column) {}
    StmtAST(type_t type, AstObject&& _content) : BaseAST(_content->line, _content->column), type(type) {
        _content->parent = this;
        content = std::move(_content);
    }
    StmtAST(type_t type, AstObject&& _lval, AstObject&& _exp) : BaseAST(_lval->line, _lval->column), type(type) {
        _lval->parent = _exp->parent = this;
        content = AssignContainer{std::move(_lval), std::move(_exp)};
    }
    std::string toString() const override {
        switch (type) {
            case Return: {
                auto& exp = std::get<AstObject>(content);
                return serializeClass("StmtAST", type, exp);
            }
            case Assign: {
                auto& [lval, exp] = std::get<AssignContainer>(content);
                return serializeClass("StmtAST", type, lval, exp);
            }
        }
        throw runtimeError("invalid statement");
    }
    IrObject toIr() const override {
        switch (type) {
            case Return: {
                auto ir = std::make_unique<ValueIR>(ValueType::Return);
                ir->content = "ret";
                ir->params.push_back(std::get<AstObject>(content)->toIr());
                return ir;
            }
            case Assign: {
                auto& [raw_lval, raw_exp] = std::get<AssignContainer>(content);
                auto lval = dynamic_cast<LValAST*>(raw_lval.get());
                auto exp = dynamic_cast<ExpAST*>(raw_exp.get());
                auto ir = std::make_unique<ValueIR>(ValueType::Store, lval->ident);
                ir->params.push_back(exp->toIr());
                return ir;
            }
        }
        throw runtimeError("invalid statement");
    }
};

class ConstDefAST : public BaseAST {
public:
    std::string ident;
    AstObject init_exp;  // NumberAST
    ConstDefAST(int line, int column) : BaseAST(line, column) {}
    ConstDefAST(std::string ident, AstObject&& init_exp)
        : BaseAST(init_exp->line, init_exp->column), ident(ident), init_exp(std::move(init_exp)) {}
    void writeSymbol() const {
        if (!init_exp->isConstexpr()) {
            throw runtimeError("{}:{}: initializer is not a constant expression", line, column);
        }
        for (auto scope = parent; scope; scope = scope->parent) {
            auto& map = scope->symbol_table;
            if (map) {
                if (map->find(ident) != map->end())
                    throw runtimeError("{}:{}: redefined variable: {}", line, column, ident);
                (*map)[ident] = init_exp->calc();
                return;
            }
        }
        std::string trace = "";
        for (const BaseAST* ancestor = this; ancestor; ancestor = ancestor->parent) {
            trace += "---------------\n" + ancestor->toString() + "\n";
        }
        throw runtimeError("{}:{}: no available scope for variable {}\nback trace:\n{}", line,
                           column, ident, trace);
    }
    std::string toString() const override { return serializeClass("ConstDefAST", ident, init_exp); }
    IrObject toIr() const override {
        writeSymbol();
        return IrObject();
    }
};

class ConstDeclAST : public BaseAST {
public:
    AstObject type;
    std::vector<AstObject> const_defs;
    ConstDeclAST(int line, int column) : BaseAST(line, column) {}
    std::string toString() const override {
        return serializeClass("ConstDeclAST", type, const_defs);
    }
    IrObject toIr() const override {
        for (auto& const_def : const_defs) {
            const_def->toIr();
        }
        return IrObject();
    }
};

class VarDefAST : public BaseAST {
public:
    std::string ident;
    AstObject init_exp;  // NumberAST
    VarDefAST(int line, int column) : BaseAST(line, column) {}
    VarDefAST(std::string ident, AstObject&& init_exp)
        : BaseAST(init_exp->line, init_exp->column), ident(ident), init_exp(std::move(init_exp)) {}
    std::string toString() const override { return serializeClass("VarDefAST", ident, init_exp); }
    void writeSymbol() const {
        for (auto scope = parent; scope; scope = scope->parent) {
            auto& map = scope->symbol_table;
            if (map) {
                if (map->find(ident) != map->end())
                    throw runtimeError("{}:{}: redefined variable: {}", line, column, ident);
                (*map)[ident] = init_exp.get();
                return;
            }
        }
        std::string trace = "";
        for (const BaseAST* ancestor = this; ancestor; ancestor = ancestor->parent) {
            trace += "---------------\n" + ancestor->toString() + "\n";
        }
        throw runtimeError("{}:{}: no available scope for variable {}\nback trace:\n{}", line,
                           column, ident, trace);
    }
    IrObject toIr() const override {
        writeSymbol();
        auto ir = std::make_unique<MultiValueIR>();
        auto inst1 = std::make_unique<ValueIR>(ValueType::Alloc, ident);
        inst1->params.push_back(std::make_unique<ValueIR>(ValueType::Type, "i32"));
        ir->values.push_back(std::move(inst1));
        if (init_exp != nullptr) {
            auto inst2 = std::make_unique<ValueIR>(ValueType::Store, ident);
            inst2->params.push_back(init_exp->toIr());
            ir->values.push_back(std::move(inst2));
        }
        return ir;
    }
};

class VarDeclAST : public BaseAST {
public:
    AstObject type;
    std::vector<AstObject> var_defs;
    VarDeclAST(int line, int column) : BaseAST(line, column) {}
    std::string toString() const override { return serializeClass("VarDeclAST", type, var_defs); }
    IrObject toIr() const override {
        auto ir = std::make_unique<MultiValueIR>();
        for (auto& var_def : var_defs) {
            ir->values.push_back(var_def->toIr());
        }
        return ir;
    }
};

class DeclAST : public BaseAST {
public:
    AstObject decl;  // ConstDeclAST or VarDeclAST
    DeclAST(int line, int column) : BaseAST(line, column) {}
    DeclAST(AstObject&& decl) : BaseAST(decl->line, decl->column), decl(std::move(decl)) {}
    std::string toString() const override { return serializeClass("DeclAST", decl); }
    IrObject toIr() const override { return decl->toIr(); }
};

class BlockItemAST : public BaseAST {
public:
    enum type_t {
        Decl,
        Stmt,
    } type;
    std::string toString(type_t t) { return t == Decl ? "Decl" : "Stmt"; }
    AstObject content;
    BlockItemAST(int line, int column) : BaseAST(line, column) {}
    BlockItemAST(type_t type, AstObject&& _content) : BaseAST(_content->line, _content->column), type(type), content(std::move(_content)) {
        content->parent = this;
    }
    std::string toString() const override {
        return serializeClass("BlockItemAST", type, content);
    }
    IrObject toIr() const override { return content->toIr(); }
};

class BlockAST : public BaseAST {
public:
    std::vector<AstObject> stmts;
    BlockAST(int line, int column) : BaseAST(line, column) {
        symbol_table = std::make_unique<SymbolTable>();
    }
    std::string toString() const override { return serializeClass("BlockAST", stmts); }
    IrObject toIr() const override {
        auto ir = std::make_unique<BasicBlockIR>();
        ir->entrance = "entry";
        for (auto& stmt : stmts) {
            auto stmt_ir = stmt->toIr();
            if (stmt_ir)
                ir->insts.push_back(
                    std::move(stmt_ir));  // probably has empty IR so need to check it
        }
        symbol_table->clear();
        return ir;
    }
};

#endif