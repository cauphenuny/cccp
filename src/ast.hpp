#ifndef AST_HPP
#define AST_HPP

#include "ir.hpp"
#include "util.hpp"

#include <map>
#include <string>
#include <variant>

class BaseAST;

using SymbolTable = std::map<std::string, std::variant<BaseAST*, int>>;

using AstObject = std::unique_ptr<BaseAST>;

class BaseAST {
public:
    int line, column;
    std::unique_ptr<SymbolTable> symbol_table;
    int block_depth{-1};
    BaseAST* parent{nullptr};
    BaseAST() = delete;
    BaseAST(int line, int column) : line(line), column(column) {}
    virtual ~BaseAST() = default;
    virtual IrObject toIR() const {
        throw runtimeError("{}:{}: can not convert {} to IR", line, column, typeid(*this).name());
    }
    virtual bool isConstExpr() const { return false; }
    virtual int calc() const {
        throw runtimeError("{}:{}: can not calculate {}", line, column, typeid(*this).name());
    }
    virtual void init() {
        if (parent) block_depth = parent->block_depth;
    }
    virtual std::string toString() const = 0;
};

class CompUnitAST : public BaseAST {
public:
    AstObject func_def;
    CompUnitAST(int line, int column) : BaseAST(line, column) {}
    std::string toString() const override { return serializeClass("CompUnitAST", func_def); }
    IrObject toIR() const override {
        auto ir = std::make_unique<ProgramIR>();
        ir->funcs.push_back(func_def->toIR());
        return ir;
    }
    void init() override { func_def->init(); }
};

class FuncDefAST : public BaseAST {
public:
    AstObject func_type;
    std::string ident;
    AstObject block;
    FuncDefAST(int line, int column) : BaseAST(line, column) {}
    FuncDefAST(const std::string& ident, AstObject&& block)
        : BaseAST(block->line, block->column), ident(ident), block(std::move(block)) {}
    std::string toString() const override {
        return serializeClass("FuncDefAST", func_type, ident, block);
    }
    IrObject toIR() const override {
        auto ir = std::make_unique<FunctionIR>(ident);
        ir->blocks.push_back(block->toIR());
        return ir;
    }
    void init() override {
        BaseAST::init();
        block->init();
    }
};

class FuncTypeAST : public BaseAST {
public:
    FuncTypeAST(int line, int column) : BaseAST(line, column) {}
    std::string type;
    std::string toString() const override { return serializeClass("FuncTypeAST", type); }
};

class VarTypeAST : public BaseAST {
public:
    VarTypeAST(int line, int column) : BaseAST(line, column) {}
    std::string type;
    std::string toString() const override { return serializeClass("VarTypeAST", type); }
};

class NumberAST : public BaseAST {
public:
    int number;
    std::string toString() const override { return serializeClass("NumberAST", number); }
    IrObject toIR() const override {
        return std::make_unique<ValueIR>(Inst::Integer, std::to_string(number));
    }
    bool isConstExpr() const override { return true; }
    int calc() const override { return number; }
    explicit NumberAST(int num, int line = -1, int column = -1)
        : BaseAST(line, column), number(num) {}
};

class ExpAST : public BaseAST {
public:
    AstObject exp;
    std::string toString() const override { return exp->toString(); }
    explicit ExpAST(AstObject&& _exp) : BaseAST(_exp->line, _exp->column), exp(std::move(_exp)) {
        exp->parent = this;
    }
    bool isConstExpr() const override { return exp->isConstExpr(); }
    int calc() const override { return exp->calc(); }
    IrObject toIR() const override {
        if (isConstExpr()) return NumberAST(exp->calc()).toIR();
        return exp->toIR();
    }
    void init() override {
        BaseAST::init();
        exp->init();
    }
};

class LValAST : public BaseAST {
public:
    LValAST(int line, int column) : BaseAST(line, column) {}
    std::string ident;
    std::string getName() const {
        auto scope = parent;
        while (scope) {
            auto& map = scope->symbol_table;
            if (map && map->find(ident) != map->end()) {
                return std::format("{}_{}", ident, scope->block_depth);
            }
            scope = scope->parent;
        }
        throw compileError("{}:{}: undefined variable: {}", line, column, ident);
    }
    std::variant<BaseAST*, int> getValue() const {
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
    bool isConstExpr() const override {
        auto value = getValue();
        return std::holds_alternative<int>(value);
    }
    int calc() const override {
        auto value = getValue();
        if (std::holds_alternative<BaseAST*>(value))
            throw compileError("{}:{}: not constant variable: {}", line, column, ident);
        return std::get<int>(value);
    }
    std::string toString() const override { return serializeClass("LValAST", ident); }
    IrObject toIR() const override {
        auto value = getValue();
        if (std::holds_alternative<int>(value)) {
            return NumberAST(std::get<int>(value)).toIR();
        } else {
            return std::make_unique<ValueIR>(Inst::Load, getName());
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
    bool isConstExpr() const override { return content->isConstExpr(); }
    IrObject toIR() const override { return content->toIR(); }
    int calc() const override { return content->calc(); }
    void init() override {
        BaseAST::init();
        content->init();
    }
};

class UnaryExpAST : public BaseAST {
public:
    enum Type {
        Virtual,  // PrimaryExp
        Real,     // UnaryOp UnaryExp
    } type;
    struct Container {
        Operator unary_op;
        AstObject unary_exp;
    };
    std::variant<AstObject, Container> content;
    UnaryExpAST(int line, int column) : BaseAST(line, column) {}
    explicit UnaryExpAST(AstObject&& obj)
        : BaseAST(obj->line, obj->column), type(Virtual), content(std::move(obj)) {
        auto& virtual_exp = std::get<AstObject>(content);
        virtual_exp->parent = this;
    }
    explicit UnaryExpAST(Container&& cont)
        : BaseAST(cont.unary_exp->line, cont.unary_exp->column), type(Real),
          content(std::move(cont)) {
        auto& real_exp = std::get<Container>(content);
        real_exp.unary_exp->parent = this;
    }

    bool isConstExpr() const override {
        switch (type) {
            case Virtual: return std::get<AstObject>(content)->isConstExpr();
            case Real: return std::get<Container>(content).unary_exp->isConstExpr();
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

    IrObject toIR() const override {
        switch (type) {
            case Virtual: return std::get<AstObject>(content)->toIR();
            case Real: {
                auto& [op, exp] = std::get<Container>(content);
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

    void init() override {
        BaseAST::init();
        if (type == Virtual) {
            std::get<AstObject>(content)->init();
        } else {
            std::get<Container>(content).unary_exp->init();
        }
    }
};

class BinaryExpAST : public BaseAST {
public:
    enum Type {
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
    explicit BinaryExpAST(AstObject&& obj)
        : BaseAST(obj->line, obj->column), type(Virtual), content(std::move(obj)) {
        auto& virtual_exp = std::get<AstObject>(content);
        virtual_exp->parent = this;
    }
    explicit BinaryExpAST(Container&& cont)
        : BaseAST(cont.left->line, cont.left->column), type(Real), content(std::move(cont)) {
        auto& real_exp = std::get<Container>(content);
        real_exp.left->parent = this;
        real_exp.right->parent = this;
    }
    bool isConstExpr() const override {
        switch (type) {
            case Virtual: return std::get<AstObject>(content)->isConstExpr();
            case Real:
                auto& [left, op, right] = std::get<Container>(content);
                return left->isConstExpr() && right->isConstExpr();
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
    IrObject toIR() const override {
        switch (type) {
            case Virtual: return std::get<AstObject>(content)->toIR();
            case Real: {
                auto& [left, op, right] = std::get<Container>(content);
                auto ir = std::make_unique<ValueIR>(Inst::Binary);
                ir->content = toIrOperatorName(op);
                ir->params.push_back(left->toIR());
                ir->params.push_back(right->toIR());
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
    void init() override {
        BaseAST::init();
        if (type == Virtual) {
            std::get<AstObject>(content)->init();
        } else {
            const auto& [left, op, right] = std::get<Container>(content);
            left->init();
            right->init();
        }
    }
};

class StmtAST : public BaseAST {
public:
    enum Type {
        Return,
        Assign,
        Block,
        Expr,
    } type;
    struct AssignContainer {
        AstObject lval;
        AstObject exp;
    };
    std::variant<AstObject, AssignContainer> content;
    StmtAST(int line, int column, Type type, AstObject&& _content)
        : BaseAST(line, column), type(type) {
        if (_content) _content->parent = this;
        content = std::move(_content);
    }
    StmtAST(int line, int column, Type type, AstObject&& _lval, AstObject&& _exp)
        : BaseAST(line, column), type(type) {
        _lval->parent = _exp->parent = this;
        content = (AssignContainer){std::move(_lval), std::move(_exp)};
    }
    std::string toString() const override {
        switch (type) {
            case Return:
            case Expr:
            case Block: {
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
    IrObject toIR() const override {
        switch (type) {
            case Expr:
            case Block: {
                if (const auto& obj_content = std::get<AstObject>(content))
                    return obj_content->toIR();
                else
                    return nullptr;
            }
            case Return: {
                auto ir = std::make_unique<ValueIR>(Inst::Return);
                ir->content = "ret";
                ir->params.push_back(std::get<AstObject>(content)->toIR());
                return ir;
            }
            case Assign: {
                auto& [raw_lval, raw_exp] = std::get<AssignContainer>(content);
                auto lval = dynamic_cast<LValAST*>(raw_lval.get());
                auto exp = dynamic_cast<ExpAST*>(raw_exp.get());
                auto ir = std::make_unique<ValueIR>(Inst::Store, lval->getName());
                ir->params.push_back(exp->toIR());
                return ir;
            }
        }
        throw runtimeError("invalid statement");
    }
    friend std::string toString(StmtAST::Type t) {
        return t == StmtAST::Return ? "Return" : "Assign";
    }
    void init() override {
        BaseAST::init();
        if (type == Assign) {
            auto& [lval, exp] = std::get<AssignContainer>(content);
            lval->init();
            exp->init();
        } else {
            if (const auto& exp = std::get<AstObject>(content)) exp->init();
        }
    }
};

class ConstDefAST : public BaseAST {
public:
    std::string ident;
    AstObject init_exp;  // NumberAST
    ConstDefAST(int line, int column) : BaseAST(line, column) {}
    ConstDefAST(const std::string& ident, AstObject&& init_exp)
        : BaseAST(init_exp->line, init_exp->column), ident(ident), init_exp(std::move(init_exp)) {}
    void writeSymbol() const {
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
    void init() override {
        BaseAST::init();
        if (!init_exp->isConstExpr()) {
            throw compileError("{}:{}: initializer is not a constant expression", line, column);
        }
    }
    std::string toString() const override { return serializeClass("ConstDefAST", ident, init_exp); }
    IrObject toIR() const override {
        writeSymbol();
        return nullptr;
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
    IrObject toIR() const override {
        for (const auto& const_def : const_defs) {
            const_def->toIR();
        }
        return nullptr;
    }
    void init() override {
        BaseAST::init();
        for (const auto& const_def : const_defs) {
            const_def->init();
        }
    }
};

class VarDefAST : public BaseAST {
public:
    std::string ident;
    AstObject init_exp;  // NumberAST
    VarDefAST(int line, int column) : BaseAST(line, column) {}
    VarDefAST(const std::string& ident, AstObject&& init_exp)
        : BaseAST(init_exp->line, init_exp->column), ident(ident), init_exp(std::move(init_exp)) {}
    std::string toString() const override {
        if (init_exp)
            return serializeClass("VarDefAST", ident, init_exp);
        else
            return serializeClass("VarDefAST", ident);
    }
    void writeSymbol() const {
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
    void init() override { BaseAST::init(); }
    std::string getName() const {
        auto scope = parent;
        while (scope) {
            auto& map = scope->symbol_table;
            if (map && map->find(ident) != map->end()) {
                return std::format("{}_{}", ident, scope->block_depth);
            }
            scope = scope->parent;
        }
        throw compileError("{}:{}: undefined variable: {}", line, column, ident);
    }
    IrObject toIR() const override {
        writeSymbol();
        auto ir = std::make_unique<MultiValueIR>();
        std::string mangled_ident = getName();
        auto inst1 = std::make_unique<ValueIR>(Inst::Alloc, mangled_ident);
        inst1->type->tag = IrType::Tag::Int32;
        ir->add(std::move(inst1));
        if (init_exp) {
            auto inst2 = std::make_unique<ValueIR>(Inst::Store, mangled_ident);
            inst2->params.push_back(init_exp->toIR());
            ir->add(std::move(inst2));
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
    IrObject toIR() const override {
        auto ir = std::make_unique<MultiValueIR>();
        for (const auto& var_def : var_defs) {
            ir->add(var_def->toIR());
        }
        return ir;
    }
    void init() override {
        BaseAST::init();
        for (const auto& var_def : var_defs) {
            var_def->init();
        }
    }
};

class DeclAST : public BaseAST {
public:
    AstObject decl;  // ConstDeclAST or VarDeclAST
    DeclAST(int line, int column) : BaseAST(line, column) {}
    explicit DeclAST(AstObject&& decl) : BaseAST(decl->line, decl->column), decl(std::move(decl)) {}
    std::string toString() const override { return serializeClass("DeclAST", decl); }
    IrObject toIR() const override { return decl->toIR(); }
    void init() override {
        BaseAST::init();
        decl->init();
    }
};

class BlockItemAST : public BaseAST {
public:
    enum Type {
        Decl,
        Stmt,
    } type;
    AstObject content;
    BlockItemAST(int line, int column) : BaseAST(line, column) {}
    BlockItemAST(Type type, AstObject&& _content)
        : BaseAST(_content->line, _content->column), type(type), content(std::move(_content)) {
        content->parent = this;
    }
    std::string toString() const override { return serializeClass("BlockItemAST", type, content); }
    friend std::string toString(Type t) { return t == Decl ? "Decl" : "Stmt"; }
    IrObject toIR() const override { return content->toIR(); }
    void init() override {
        BaseAST::init();
        content->init();
    }
};

class BlockAST : public BaseAST {
public:
    std::vector<AstObject> stmts;
    BlockAST(int line, int column) : BaseAST(line, column) {
        symbol_table = std::make_unique<SymbolTable>();
    }
    void init() override {
        BaseAST::init();
        block_depth++;
        for (const auto& stmt : stmts) {
            stmt->init();
        }
    }
    std::string toString() const override { return serializeClass("BlockAST", stmts); }
    IrObject toIR() const override {
        if (block_depth < 0) throw runtimeError("block depth unspecified");
        if (block_depth == 0) {
            auto ir = std::make_unique<BasicBlockIR>();
            ir->entrance = "entry";
            for (const auto& stmt : stmts) {
                ir->add(stmt->toIR());
            }
            symbol_table->clear();
            return ir;
        } else {
            auto ir = std::make_unique<MultiValueIR>();
            for (const auto& stmt : stmts) {
                ir->add(stmt->toIR());
            }
            symbol_table->clear();
            return ir;
        }
    }
};

#endif