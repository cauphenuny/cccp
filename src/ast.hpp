#ifndef AST_HPP
#define AST_HPP

#include "ir.hpp"
#include "util.hpp"

#include <iostream>
#include <memory>
#include <string>
#include <variant>

enum class Operator {
    no,    //!
    add,   // +
    sub,   // -
    mul,   // *
    div,   // /
    mod,   // %
    eq,    // ==
    neq,   // !=
    leq,   // <=
    geq,   // >=
    lt,    // <
    gt,    // >
    band,  // &
    bor,   // |
    land,  // &&
    lor,   // ||
};

constexpr const char* toRawOperator(Operator op)
{
    switch (op) {
        case Operator::no: return "!";
        case Operator::add: return "+";
        case Operator::sub: return "-";
        case Operator::mul: return "*";
        case Operator::div: return "/";
        case Operator::mod: return "%";
        case Operator::eq: return "==";
        case Operator::neq: return "!=";
        case Operator::leq: return "<=";
        case Operator::geq: return ">=";
        case Operator::lt: return "<";
        case Operator::gt: return ">";
        case Operator::band: return "&";
        case Operator::bor: return "|";
        case Operator::land: return "&&";
        case Operator::lor: return "||";
    }
}

constexpr const char* toIrOperatorName(Operator op)
{
    switch (op) {
        case Operator::neq: return "ne";
        case Operator::eq: return "eq";

        case Operator::gt: return "gt";
        case Operator::lt: return "lt";

        case Operator::leq: return "le";
        case Operator::geq: return "ge";

        case Operator::add: return "add";
        case Operator::sub: return "sub";
        case Operator::mul: return "mul";
        case Operator::div: return "div";
        case Operator::mod: return "mod";

        case Operator::band: return "and";
        case Operator::bor: return "or";

        default: return "unknown";
    }
}

class BaseAST
{
public:
    virtual ~BaseAST() = default;
    virtual std::string toString() const = 0;
    // virtual std::string toString() const { return "not implemented"; }
    // virtual IrObject toIr() const = 0;
    virtual IrObject toIr() const { return IrObject(); }
    friend std::ostream& operator<<(std::ostream& os, const BaseAST& ast)
    {
        os << ast.toString();
        return os;
    }
};

using AstObject = std::unique_ptr<BaseAST>;

class CompUnitAST : public BaseAST
{
public:
    AstObject func_def;
    CompUnitAST() = default;
    std::string toString() const override
    {
        return "CompUnitAST {\n" +
               addIndent("func_def: " + func_def->toString() + "\n") + "}";
    }
    IrObject toIr() const override
    {
        auto ir = std::make_unique<ProgramIR>();
        ir->funcs.push_back(func_def->toIr());
        return IrObject(std::move(ir));
    }
};

class FuncDefAST : public BaseAST
{
public:
    AstObject func_type;
    std::string ident;
    AstObject block;
    FuncDefAST() = default;
    std::string toString() const override
    {
        return "FuncDefAST {\n" +
               addIndent("func_type: " + func_type->toString() + ",\n") +
               addIndent("ident: \"" + ident + "\",\n") +
               addIndent("block: " + block->toString() + "\n") + "}";
    }
    IrObject toIr() const override
    {
        auto ir = std::make_unique<FunctionIR>();
        ir->name = ident;
        ir->ret_type = func_type->toIr();
        ir->blocks.push_back(block->toIr());
        return IrObject(std::move(ir));
    }
};

class FuncTypeAST : public BaseAST
{
public:
    FuncTypeAST() = default;
    std::string type;
    std::string toString() const override
    {
        return "FuncTypeAST {\n" + addIndent("type: " + type + "\n") + "}";
    }
    IrObject toIr() const override
    {
        auto ir = std::make_unique<ValueIR>();
        if (type == "int") {
            ir->type = ValueType::Integer;
            ir->content = "i32";
        } else {
            ir->type = ValueType::Unknown;
            ir->content = type;
        }
        return IrObject(std::move(ir));
    }
};

class BlockAST : public BaseAST
{
public:
    AstObject stmt;
    BlockAST() = default;
    std::string toString() const override
    {
        return "BlockAST {\n" + addIndent("stmt: " + stmt->toString() + "\n") +
               "}";
    }
    IrObject toIr() const override
    {
        auto ir = std::make_unique<BasicBlockIR>();
        ir->entrance = "entry";
        ir->insts.push_back(stmt->toIr());
        return IrObject(std::move(ir));
    }
};

class NumberAST : public BaseAST
{
public:
    int number;
    std::string toString() const override
    {
        return "NumberAST {\n" +
               addIndent("number: " + std::to_string(number) + "\n") + "}";
    }
    IrObject toIr() const override
    {
        auto ir = std::make_unique<ValueIR>();
        ir->type = ValueType::Integer;
        ir->content = std::to_string(number);
        return IrObject(std::move(ir));
    }
    NumberAST() = default;
    NumberAST(int num) : number(num) {}
};

class ExpAST : public BaseAST
{
public:
    AstObject exp;
    std::string toString() const override
    {
        return "ExpAST {\n" + addIndent("exp: " + exp->toString() + "\n") + "}";
    }
    IrObject toIr() const override { return exp->toIr(); }
};

class PrimaryExpAST : public BaseAST
{
public:
    enum {
        Virtual,  // Number
        Real,     // "(" Exp ")"
    } type;
    AstObject content;
    std::string toString() const override
    {
        switch (type) {
            case Virtual: return content->toString();
            case Real:
                return "PrimaryExpAST {\n" +
                       addIndent("content: " + content->toString() + "\n") +
                       "}";
        }
    }
    IrObject toIr() const override { return content->toIr(); }
};

class UnaryExpAST : public BaseAST
{
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
    UnaryExpAST(AstObject&& obj) : type(Virtual), content(std::move(obj)) {}
    UnaryExpAST(Container&& content) : type(Real), content(std::move(content))
    {
    }

    std::string toString() const override
    {
        switch (type) {
            case Virtual:
                return std::get<AstObject>(content)->toString();
                // return "UnaryExpAST {\n" + addIndent("type: Primary,\n") +
                //        addIndent("content: " +
                //                  std::get<AstObject>(content)->toString() +
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
    IrObject toIr() const override
    {
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
                        return IrObject(std::move(ir));
                    case Operator::no:
                        ir->type = ValueType::Binary;
                        ir->content = "eq";
                        ir->params.push_back(NumberAST(0).toIr());
                        ir->params.push_back(exp->toIr());
                        return IrObject(std::move(ir));
                    default:
                        std::cerr << "unknown type!" << std::endl;
                        return IrObject();
                }
            }
        }
    }
};

class BinaryExpAST : public BaseAST
{
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
    BinaryExpAST(AstObject&& obj)  // cautios: move object!
        : type(Virtual), content(std::move(obj))
    {
    }
    BinaryExpAST(Container&& content)  // cautious: move object!
        : type(Real), content(std::move(content))
    {
    }
    std::string toString() const override
    {
        switch (type) {
            case Virtual:
                return std::get<AstObject>(content)->toString();
                // return "UnaryExpAST {\n" + addIndent("type: Virtual,\n") +
                //        addIndent("content: " +
                //                  std::get<AstObject>(content)->toString() +
                //                  "\n") +
                //        "}";
            case Real:
                auto& [left, op, right] = std::get<Container>(content);
                return "BinaryExpAST {\n" +
                       addIndent("op: " + std::string(toRawOperator(op)) +
                                 ",\nleft: " + left->toString() +
                                 ",\nright: " + right->toString() + "\n") +
                       "}";
        }
    }
    IrObject toIr() const override
    {
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
                        return IrObject(std::move(ir));
                    }
                }
            }
        }
    }
};

class StmtAST : public BaseAST
{
public:
    AstObject exp;
    StmtAST() = default;
    std::string toString() const override
    {
        return "StmtAST {\n" + addIndent("exp: " + exp->toString() + "\n") +
               "}";
    }
    IrObject toIr() const override
    {
        auto ir = std::make_unique<ValueIR>();
        ir->type = ValueType::Return;
        ir->content = "ret";
        ir->params.push_back(exp->toIr());
        return IrObject(std::move(ir));
    }
};

#endif