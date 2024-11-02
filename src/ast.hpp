#ifndef AST_HPP
#define AST_HPP

#include "ir.hpp"
#include "util.hpp"

#include <iostream>
#include <map>
#include <memory>
#include <stdexcept>
#include <string>
#include <variant>

class BaseAST;

using SymbolTable = std::map<std::string, std::variant<BaseAST*, int>>;

using AstObject = std::unique_ptr<BaseAST>;

class BaseAST {
public:
    std::unique_ptr<SymbolTable> symbol_table;
    BaseAST* parent;
    BaseAST() = default;
    virtual ~BaseAST() = default;
    virtual IrObject toIr() const {
        throw std::runtime_error("can not convert " + std::string(typeid(*this).name()) + "to IR");
    }
    virtual bool is_constexpr() const { return false; }
    virtual int calc() const {
        throw std::runtime_error("can not calculate " + std::string(typeid(*this).name()));
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
    CompUnitAST() = default;
    std::string toString() const override {
        return "CompUnitAST {\n" + addIndent("func_def: " + func_def->toString() + "\n") + "}";
    }
    IrObject toIr() const override {
        auto ir = std::make_unique<ProgramIR>();
        ir->funcs.push_back(func_def->toIr());
        return std::move(ir);
    }
};

class FuncDefAST : public BaseAST {
public:
    AstObject func_type;
    std::string ident;
    AstObject block;
    FuncDefAST() = default;
    std::string toString() const override {
        return "FuncDefAST {\n" + addIndent("func_type: " + func_type->toString() + ",\n") +
               addIndent("ident: \"" + ident + "\",\n") +
               addIndent("block: " + block->toString() + "\n") + "}";
    }
    IrObject toIr() const override {
        auto ir = std::make_unique<FunctionIR>();
        ir->name = ident;
        ir->ret_type = func_type->toIr();
        ir->blocks.push_back(block->toIr());
        return std::move(ir);
    }
};

class FuncTypeAST : public BaseAST {
public:
    FuncTypeAST() = default;
    std::string type;
    std::string toString() const override {
        return "FuncTypeAST {\n" + addIndent("type: " + type + "\n") + "}";
    }
    IrObject toIr() const override {
        auto ir = std::make_unique<ValueIR>();
        if (type == "int") {
            ir->type = ValueType::Integer;
            ir->content = "i32";
        } else {
            ir->type = ValueType::Unknown;
            ir->content = type;
        }
        return std::move(ir);
    }
};

class VarTypeAST : public BaseAST {
public:
    VarTypeAST() = default;
    std::string type;
    std::string toString() const override {
        return "VarTypeAST {\n" + addIndent("type: " + type + "\n") + "}";
    }
    IrObject toIr() const override {
        auto ir = std::make_unique<ValueIR>();
        if (type == "int") {
            ir->type = ValueType::Integer;
            ir->content = "i32";
        } else {
            ir->type = ValueType::Unknown;
            ir->content = type;
        }
        return std::move(ir);
    }
};

class NumberAST : public BaseAST {
public:
    int number;
    std::string toString() const override {
        return "NumberAST {\n" + addIndent("number: " + std::to_string(number) + "\n") + "}";
    }
    IrObject toIr() const override {
        auto ir = std::make_unique<ValueIR>();
        ir->type = ValueType::Integer;
        ir->content = std::to_string(number);
        return std::move(ir);
    }
    bool is_constexpr() const override { return true; }
    int calc() const override { return number; }
    NumberAST(int num) : number(num) {}
};

class ExpAST : public BaseAST {
public:
    AstObject exp;
    std::string toString() const override {
        return "ExpAST {\n" + addIndent("exp: " + exp->toString() + "\n") + "}";
    }
    ExpAST() = default;
    ExpAST(AstObject&& _exp) : exp(std::move(_exp)) { exp->parent = this; }
    bool is_constexpr() const override { return exp->is_constexpr(); }
    int calc() const override { return exp->calc(); }
    IrObject toIr() const override { return exp->toIr(); }
};

class LValAST : public BaseAST {
public:
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
    int calc() const override {
        auto value = get_value();
        if (!value.has_value()) {
            throw std::runtime_error("undefined variable: " + ident);
        }
        if (std::holds_alternative<BaseAST*>(value.value()))
            throw std::runtime_error("not constant variable: " + ident);
        return std::get<int>(value.value());
    }
    std::string toString() const override {
        return "LValAST {\n" + addIndent("ident: \"" + ident + "\"\n") + "}";
    }
    IrObject toIr() const override {
        auto value = get_value();
        if (!value.has_value()) {
            throw std::runtime_error("undefined variable: " + ident);
        }
        if (std::holds_alternative<int>(value.value())) {
            auto ir = std::make_unique<ValueIR>();
            ir->type = ValueType::Integer;
            ir->content = std::to_string(calc());
            return std::move(ir);
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
    PrimaryExpAST() = default;
    PrimaryExpAST(Type type, AstObject&& obj) : type(type), content(std::move(obj)) {
        content->parent = this;
    }
    bool is_constexpr() const override { return content->is_constexpr(); }
    std::string toString() const override {
        switch (type) {
            case Number:
            case LVal: return content->toString();
            case Exp:
                return "PrimaryExpAST {\n" + addIndent("content: " + content->toString() + "\n") +
                       "}";
        }
    }
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
    UnaryExpAST() = default;
    UnaryExpAST(AstObject&& obj) : type(Virtual), content(std::move(obj)) {
        auto& virtual_exp = std::get<AstObject>(content);
        virtual_exp->parent = this;
    }
    UnaryExpAST(Container&& cont) : type(Real), content(std::move(cont)) {
        auto& real_exp = std::get<Container>(content);
        real_exp.unary_exp->parent = this;
    }

    bool is_constexpr() const override {
        switch (type) {
            case Virtual: return std::get<AstObject>(content)->is_constexpr();
            case Real: return std::get<Container>(content).unary_exp->is_constexpr();
        }
    }

    std::string toString() const override {
        switch (type) {
            case Virtual:
                return std::get<AstObject>(content)->toString();
                // return "UnaryExpAST {\n" + addIndent("type: Primary,\n") +
                //        addIndent("content: " +
                //                  std::get<AstObject>(content)->toString()
                //                  +
                //                  "\n") +
                //        "}";
            case Real:
                auto& [op, exp] = std::get<Container>(content);
                return "UnaryExpAST {\n" +
                       addIndent(std::string("op: ") + toRawOperator(op) +
                                 ", \nexp: " + exp->toString() + "\n") +
                       "}";
        }
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
                        ir->type = ValueType::Binary;
                        ir->content = "sub";
                        ir->params.push_back(NumberAST(0).toIr());
                        ir->params.push_back(exp->toIr());
                        return std::move(ir);
                    case Operator::no:
                        ir->type = ValueType::Binary;
                        ir->content = "eq";
                        ir->params.push_back(NumberAST(0).toIr());
                        ir->params.push_back(exp->toIr());
                        return std::move(ir);
                    default:
                        eprintf("invalid operator %s for unary expression!", toRawOperator(op));
                        return IrObject();
                }
            }
        }
    }

    int calc() const override {
        switch (type) {
            case Virtual: return std::get<AstObject>(content)->calc();
            case Real: {
                auto& [op, exp] = std::get<Container>(content);
                return getFunction<int>(op)(0, exp->calc());
            }
        }
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
    BinaryExpAST() = default;
    BinaryExpAST(AstObject&& obj)  // cautious: move object!
        : type(Virtual), content(std::move(obj)) {
        auto& virtual_exp = std::get<AstObject>(content);
        virtual_exp->parent = this;
    }
    BinaryExpAST(Container&& cont)  // cautious: move object!
        : type(Real), content(std::move(cont)) {
        auto& real_exp = std::get<Container>(content);
        real_exp.left->parent = this;
        real_exp.right->parent = this;
    }
    bool is_constexpr() const override {
        switch (type) {
            case Virtual: return std::get<AstObject>(content)->is_constexpr();
            case Real:
                auto& [left, op, right] = std::get<Container>(content);
                return left->is_constexpr() && right->is_constexpr();
        }
    }
    std::string toString() const override {
        switch (type) {
            case Virtual:
                return std::get<AstObject>(content)->toString();
                // return "UnaryExpAST {\n" + addIndent("type: Virtual,\n") +
                //        addIndent("content: " +
                //                  std::get<AstObject>(content)->toString()
                //                  +
                //                  "\n") +
                //        "}";
            case Real:
                auto& [left, op, right] = std::get<Container>(content);
                return "BinaryExpAST {\n" +
                       addIndent("op: " + std::string(toRawOperator(op)) + ",\nleft: " +
                                 left->toString() + ",\nright: " + right->toString() + "\n") +
                       "}";
        }
    }
    IrObject toIr() const override {
        switch (type) {
            case Virtual: return std::get<AstObject>(content)->toIr();
            case Real: {
                auto& [left, op, right] = std::get<Container>(content);
                switch (op) {
                    // case Operator::land: {
                    //     Container left_side, right_side;
                    //     left_side.left = std::move(left);
                    //     left_side.right = AstObject(new NumberAST(0));
                    //     break;
                    // }
                    default: {
                        auto ir = std::make_unique<ValueIR>();
                        ir->type = ValueType::Binary;
                        ir->content = toIrOperatorName(op);
                        ir->params.push_back(left->toIr());
                        ir->params.push_back(right->toIr());
                        return std::move(ir);
                    }
                }
            }
        }
    }
    int calc() const override {
        switch (type) {
            case Virtual: return std::get<AstObject>(content)->calc();
            case Real: {
                auto& [left, op, right] = std::get<Container>(content);
                return getFunction<int>(op)(left->calc(), right->calc());
            }
        }
    }
};

class StmtAST : public BaseAST {
public:
    enum type_t {
        Return,
        Assign,
    } type;
    struct AssignContainer {
        AstObject lval;
        AstObject exp;
    };
    std::variant<AstObject, AssignContainer> content;
    StmtAST() = delete;
    StmtAST(type_t type, AstObject&& _content) : type(type) {
        _content->parent = this;
        content = std::move(_content);
    }
    StmtAST(type_t type, AstObject&& _lval, AstObject&& _exp) : type(type) {
        _lval->parent = _exp->parent = this;
        content = AssignContainer{std::move(_lval), std::move(_exp)};
    }
    std::string toString() const override {
        switch (type) {
            case Return:
                return "StmtAST {\n" + addIndent("type: Return\n") +
                       addIndent("exp: " + std::get<AstObject>(content)->toString() + "\n") + "}";
            case Assign: {
                auto& [lval, exp] = std::get<AssignContainer>(content);
                return "StmtAST {\n" + addIndent("type: Assign\n") +
                       addIndent("lval: " + lval->toString() + "\n") +
                       addIndent("exp: " + exp->toString() + "\n") + "}";
            }
        }
    }
    IrObject toIr() const override {
        switch (type) {
            case Return: {
                auto ir = std::make_unique<ValueIR>(ValueType::Return);
                ir->content = "ret";
                ir->params.push_back(std::get<AstObject>(content)->toIr());
                return std::move(ir);
            }
            case Assign: {
                auto& [raw_lval, raw_exp] = std::get<AssignContainer>(content);
                auto lval = dynamic_cast<LValAST*>(raw_lval.get());
                auto exp = dynamic_cast<ExpAST*>(raw_exp.get());
                auto ir = std::make_unique<ValueIR>(ValueType::Store, lval->ident);
                ir->params.push_back(exp->toIr());
                return std::move(ir);
            }
        }
    }
};

class ConstDefAST : public BaseAST {
public:
    std::string ident;
    AstObject init_exp;  // NumberAST
    ConstDefAST() = default;
    ConstDefAST(std::string ident, AstObject&& init_exp)
        : ident(ident), init_exp(std::move(init_exp)) {}
    void writeSymbol() const {
        for (auto scope = parent; scope; scope = scope->parent) {
            auto& map = scope->symbol_table;
            if (map) {
                if (map->find(ident) != map->end())
                    throw std::runtime_error("redefined variable: " + ident);
                (*map)[ident] = init_exp->calc();
                return;
            }
        }
        std::string trace = "";
        for (const BaseAST* ancestor = this; ancestor; ancestor = ancestor->parent) {
            trace += "---------------\n" + ancestor->toString() + "\n";
        }
        throw std::runtime_error("no available scope for variable " + ident + "\nback trace:\n" +
                                 trace);
    }
    std::string toString() const override {
        return "ConstDefAST {\n" + addIndent("ident: \"" + ident + "\",\n") +
               addIndent("init_exp: " + init_exp->toString() + "\n") + "}";
    }
    IrObject toIr() const override {
        writeSymbol();
        return IrObject();
    }
};

class ConstDeclAST : public BaseAST {
public:
    AstObject type;
    std::vector<AstObject> const_defs;
    ConstDeclAST() = default;
    std::string toString() const override {
        std::string const_defs_str;
        for (auto& const_def : const_defs) {
            const_defs_str += const_def->toString() + ",\n";
        }
        return "ConstDeclAST {\n" + addIndent("type: " + type->toString() + ",\n") +
               addIndent("const_defs: [\n" + addIndent(const_defs_str) + "]\n") + "}";
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
    VarDefAST() = default;
    VarDefAST(std::string ident, AstObject&& init_exp)
        : ident(ident), init_exp(std::move(init_exp)) {}
    std::string toString() const override {
        return "VarDefAST {\n" + addIndent("ident: \"" + ident + "\",\n") +
               addIndent("init_exp: " + init_exp->toString() + "\n") + "}";
    }
    void writeSymbol() const {
        for (auto scope = parent; scope; scope = scope->parent) {
            auto& map = scope->symbol_table;
            if (map) {
                if (map->find(ident) != map->end())
                    throw std::runtime_error("redefined variable: " + ident);
                (*map)[ident] = init_exp.get();
                return;
            }
        }
        std::string trace = "";
        for (const BaseAST* ancestor = this; ancestor; ancestor = ancestor->parent) {
            trace += "---------------\n" + ancestor->toString() + "\n";
        }
        throw std::runtime_error("no available scope for variable " + ident + "\nback trace:\n" +
                                 trace);
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
        return std::move(ir);
    }
};

class VarDeclAST : public BaseAST {
public:
    AstObject type;
    std::vector<AstObject> var_defs;
    VarDeclAST() = default;
    std::string toString() const override {
        std::string var_defs_str;
        for (auto& var_def : var_defs) {
            var_defs_str += var_def->toString() + ",\n";
        }
        return "VarDeclAST {\n" + addIndent("type: " + type->toString() + ",\n") +
               addIndent("var_defs: [\n" + addIndent(var_defs_str) + "]\n") + "}";
    }
    IrObject toIr() const override {
        auto ir = std::make_unique<MultiValueIR>();
        for (auto& var_def : var_defs) {
            ir->values.push_back(var_def->toIr());
        }
        return std::move(ir);
    }
};

class DeclAST : public BaseAST {
public:
    AstObject decl;  // ConstDeclAST or VarDeclAST
    DeclAST() = default;
    DeclAST(AstObject&& decl) : decl(std::move(decl)) {}
    IrObject toIr() const override { return decl->toIr(); }
};

class BlockItemAST : public BaseAST {
public:
    enum type_t {
        Decl,
        Stmt,
    } type;
    AstObject content;
    BlockItemAST() = delete;
    BlockItemAST(type_t type, AstObject&& _content) : type(type), content(std::move(_content)) {
        content->parent = this;
    }
    std::string toString() const override {
        std::string type_str = type == Decl ? "Decl" : "Stmt";
        return "BlockItemAST {\n" + addIndent("type: " + type_str + ",\n") +
               addIndent("content: " + content->toString() + "\n") + "}";
    }
    IrObject toIr() const override { return content->toIr(); }
};

class BlockAST : public BaseAST {
public:
    std::vector<AstObject> stmts;

    std::string toString() const override {
        std::string stmts_str;
        for (auto& stmt : stmts) {
            stmts_str += stmt->toString() + ",\n";
        }
        std::string block_str =
            "BlockAST {\n" + addIndent("stmts: [\n" + addIndent(stmts_str) + "]\n") + "}";
        return block_str;
    }

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
        return std::move(ir);
    }
    BlockAST() { symbol_table = std::make_unique<SymbolTable>(); }
};

#endif