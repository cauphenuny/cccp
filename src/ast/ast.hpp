#ifndef AST_HPP
#define AST_HPP

#include "ir/ir.hpp"
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
    int scope_id{-1};
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
        if (parent) scope_id = parent->scope_id;
    }
    virtual std::string toString() const = 0;
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
                return std::format("{}_{}", ident, scope->scope_id);
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
    struct Container {
        Operator unary_op;
        AstObject unary_exp;
    };
    std::variant<AstObject, Container> content;
    UnaryExpAST(int line, int column) : BaseAST(line, column) {}
    explicit UnaryExpAST(AstObject&& obj)
        : BaseAST(obj->line, obj->column), content(std::move(obj)) {
        auto& virtual_exp = std::get<AstObject>(content);
        virtual_exp->parent = this;
    }
    explicit UnaryExpAST(Container&& cont)
        : BaseAST(cont.unary_exp->line, cont.unary_exp->column), content(std::move(cont)) {
        auto& real_exp = std::get<Container>(content);
        real_exp.unary_exp->parent = this;
    }

    bool isConstExpr() const override {
        return Match{content}(  // for clang-format split line
            [](const AstObject& obj) { return obj->isConstExpr(); },
            [](const Container& ctn) { return ctn.unary_exp->isConstExpr(); });
    }

    std::string toString() const override {
        return Match{content}(  //
            [](const AstObject& obj) { return obj->toString(); },
            [](const Container& ctn) {
                auto& [op, exp] = ctn;
                return serializeClass("UnaryExpAST", op, exp);
            });
    }

    IrObject toIR() const override {
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

    int calc() const override {
        return Match{content}(  //
            [](const AstObject& obj) { return obj->calc(); },
            [](const Container& ctn) {
                auto& [op, exp] = ctn;
                return getFunction<int>(op)(0, exp->calc());
            });
    }

    void init() override {
        BaseAST::init();
        Match{content}(  //
            [](const AstObject& obj) { obj->init(); },
            [](const Container& ctn) { ctn.unary_exp->init(); });
    }
};

class BinaryExpAST : public BaseAST {
public:
    struct Container {
        AstObject left;
        Operator op;
        AstObject right;
    };
    std::variant<AstObject, Container> content;
    explicit BinaryExpAST(AstObject&& obj)
        : BaseAST(obj->line, obj->column), content(std::move(obj)) {
        auto& virtual_exp = std::get<AstObject>(content);
        virtual_exp->parent = this;
    }
    explicit BinaryExpAST(int line, int column, Container&& cont)
        : BaseAST(line, column), content(std::move(cont)) {
        auto& real_exp = std::get<Container>(content);
        real_exp.left->parent = this;
        real_exp.right->parent = this;
    }
    bool isConstExpr() const override {
        return Match{content}(  //
            [](const AstObject& obj) { return obj->isConstExpr(); },
            [](const Container& ctn) {
                return ctn.left->isConstExpr() && ctn.right->isConstExpr();
            });
    }
    std::string toString() const override {
        return Match{content}(  //
            [](const AstObject& obj) { return obj->toString(); },
            [&](const Container& ctn) {
                auto& [left, op, right] = ctn;
                return serializeClass("BinaryExpAST", line, column, op, left, right);
            });
    }
    IrObject toIR() const override {
        return Match{content}(  //
            [&](const AstObject& obj) { return obj->toIR(); },
            [&](const Container& ctn) -> IrObject {
                if (isConstExpr()) {
                    return NumberAST(calc()).toIR();
                } else {
                    auto& [left, op, right] = ctn;
                    if (op != Operator::lor && op != Operator::land) {
                        auto ir = std::make_unique<ValueIR>(Inst::Binary);
                        ir->content = toIrOperatorName(op);
                        ir->params.push_back(left->toIR());
                        ir->params.push_back(right->toIR());
                        return ir;
                    } else {
                        auto ir = std::make_unique<MultiValueIR>();
                        auto left_ir = left->toIR();
                        auto right_ir = right->toIR();
                        std::string short_circuit_label = std::format("right_exp_{}_{}", line, column);
                        std::string end_label = std::format("end_exp_{}_{}", line, column);
                        std::string result = std::format("result_{}_{}", line, column);
                        auto new_value = [](auto&&... params) {
                            return std::make_unique<ValueIR>(params...);
                        };
                        ir->add(new_value(Inst::Alloc, result));
                        ir->add(new_value(Inst::Store, result, new_value(Inst::Integer, op == Operator::lor ? "1" : "0")));
                        if (op == Operator::lor) {
                            ir->add(std::make_unique<ValueIR>(Inst::Branch, "", left_ir,
                                                              new_value(end_label),
                                                              new_value(short_circuit_label)));
                        } else if (op == Operator::land) {
                            ir->add(std::make_unique<ValueIR>(Inst::Branch, "", left_ir,
                                                              new_value(short_circuit_label),
                                                              new_value(end_label)));
                        }
                        ir->add(new_value(Inst::Label, short_circuit_label));
                        ir->add(new_value(Inst::Store, result, right->toIR()));
                        ir->add(new_value(Inst::Label, end_label));
                        ir->add(new_value(Inst::Load, result));
                        return ir;
                    }
                }
            });
    }
    int calc() const override {
        return Match{content}(  //
            [](const AstObject& obj) { return obj->calc(); },
            [](const Container& ctn) {
                auto& [left, op, right] = ctn;
                return getFunction<int>(op)(left->calc(), right->calc());
            });
    }
    void init() override {
        BaseAST::init();
        Match{content}(  //
            [](const AstObject& obj) { obj->init(); },
            [](const Container& ctn) {
                ctn.left->init();
                ctn.right->init();
            });
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
    void init() override { BaseAST::init(); }
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
                return std::format("{}_{}", ident, scope->scope_id);
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
        inst1->type->tag = BaseIR::Type::Tag::Int32;
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

class StmtAST : public BaseAST {
public:
    enum Type {
        Return,
        Assign,
        Block,
        Expr,
        If,
    } type;
    struct AssignContainer {
        AstObject lval, exp;
    };
    struct IfContainer {
        AstObject exp, then_stmt, else_stmt;
    };
    std::variant<AstObject, AssignContainer, IfContainer> content;
    StmtAST(int line, int column, Type type, AstObject&& _content)
        : BaseAST(line, column), type(type) {
        if (_content) _content->parent = this;
        content = std::move(_content);
    }
    StmtAST(int line, int column, AssignContainer&& assign_stmt)
        : BaseAST(line, column), type(Assign) {
        assign_stmt.lval->parent = assign_stmt.exp->parent = this;
        content = std::move(assign_stmt);
    }
    StmtAST(int line, int column, IfContainer&& if_stmt) : BaseAST(line, column), type(If) {
        if_stmt.exp->parent = if_stmt.then_stmt->parent = this;
        if (if_stmt.else_stmt) if_stmt.else_stmt->parent = this;
        content = std::move(if_stmt);
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
            case If: {
                auto& [exp, then_stmt, else_stmt] = std::get<IfContainer>(content);
                return serializeClass("StmtAST", type, exp, then_stmt, else_stmt);
            }
        }
        assert(false && "invalid statement");
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
            case If: {
                auto& [exp, then_stmt, else_stmt] = std::get<IfContainer>(content);
                auto ir = std::make_unique<MultiValueIR>();
                std::string then_label = std::format("ifthen_{}_{}", line, column),
                            else_label = std::format("ifelse_{}_{}", line, column),
                            end_label = std::format("endif_{}_{}", line, column);
                auto new_value = [](auto&&... params) {
                    return std::make_unique<ValueIR>(params...);
                };
                ir->add(new_value(Inst::Branch, "", exp->toIR(), new_value(then_label),
                                  new_value(else_stmt ? else_label : end_label)));
                ir->add(std::make_unique<MultiValueIR>(new_value(Inst::Label, then_label),
                                                       then_stmt->toIR(),
                                                       new_value(Inst::Jump, end_label)));
                if (else_stmt) {
                    ir->add(std::make_unique<MultiValueIR>(new_value(Inst::Label, else_label),
                                                           else_stmt->toIR(),
                                                           new_value(Inst::Jump, end_label)));
                }
                ir->add(new_value(Inst::Label, end_label));
                return ir;
            }
            default: {
                throw runtimeError("unimplemented statement type {}", type);
            }
        }
    }
    friend std::string toString(StmtAST::Type t) {
        switch (t) {
            case Return: return "Return";
            case Assign: return "Assign";
            case Block: return "Block";
            case Expr: return "Expr";
            case If: return "If";
            default: assert(false && "invalid statement");
        }
    }
    void init() override {
        BaseAST::init();
        switch (type) {
            case Assign: {
                auto& [lval, exp] = std::get<AssignContainer>(content);
                lval->init();
                exp->init();
                break;
            }
            case If: {
                auto& [exp, then_stmt, else_stmt] = std::get<IfContainer>(content);
                exp->init();
                then_stmt->init();
                if (else_stmt) else_stmt->init();
                break;
            }
            default:
                if (const auto& exp = std::get<AstObject>(content)) exp->init();
        }
    }
};

class BlockItemAST : public BaseAST {
public:
    enum Type {
        Decl,
        Stmt,
    } type;
    AstObject content;
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

inline int block_id_tot;

class BlockAST : public BaseAST {
public:
    std::vector<AstObject> stmts;
    BlockAST(int line, int column) : BaseAST(line, column) {
        symbol_table = std::make_unique<SymbolTable>();
    }
    void init() override {
        BaseAST::init();
        scope_id = block_id_tot++;
        for (const auto& stmt : stmts) {
            stmt->init();
        }
    }
    std::string toString() const override { return serializeClass("BlockAST", stmts); }
    IrObject toIR() const override {
        if (scope_id < 0) throw runtimeError("block depth unspecified");
        auto ir = std::make_unique<MultiValueIR>();
        for (const auto& stmt : stmts) {
            ir->add(stmt->toIR());
        }
        symbol_table->clear();
        return ir;
    }
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
        block_id_tot = 0;
        auto ir = std::make_unique<FunctionIR>(ident);
        auto block_ir = std::make_unique<MultiValueIR>();
        block_ir->add(std::make_unique<ValueIR>(Inst::Label, "entry"));
        block_ir->add(block->toIR());
        ir->blocks = std::move(block_ir);
        return ir;
    }
    void init() override {
        BaseAST::init();
        block->init();
    }
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

#endif