#ifndef IR_HPP
#define IR_HPP

#include "util.hpp"

#include <cassert>
#include <functional>
#include <iostream>
#include <map>
#include <memory>
#include <string>
#include <vector>

enum class Operator : unsigned {
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

template <typename T>
std::array<std::function<T(T, T)>, 16> function_map = {
    std::not_equal_to<T>(),  // no,    // !    // note:
    std::plus<T>(),   // add,   // +    // for unary expression, the first parameter should be 0
    std::minus<T>(),  // sub,   // -    // the second parameter is the given parameter
    std::multiplies<T>(),     // mul,   // *
    std::divides<T>(),        // div,   // /
    std::modulus<T>(),        // mod,   // %
    std::equal_to<T>(),       // eq,    // ==
    std::not_equal_to<T>(),   // neq,   // !=
    std::less_equal<T>(),     // leq,   // <=
    std::greater_equal<T>(),  // geq,   // >=
    std::less<T>(),           // lt,    // <
    std::greater<T>(),        // gt,    // >
    std::bit_and<T>(),        // band,  // &
    std::bit_or<T>(),         // bor,   // |
    std::logical_and<T>(),    // land,  // &&
    std::logical_or<T>()      // lor,   // ||
};

template <typename T> const std::function<T(T, T)> getFunction(Operator op) {
    return function_map<T>[(size_t)op];
}

inline std::array<std::pair<Operator, const char*>, 16> raw_map = {  //
    {{Operator::no, "!"},
     {Operator::add, "+"},
     {Operator::sub, "-"},
     {Operator::mul, "*"},
     {Operator::div, "/"},
     {Operator::mod, "%"},
     {Operator::eq, "=="},
     {Operator::neq, "!="},
     {Operator::leq, "<="},
     {Operator::geq, ">="},
     {Operator::lt, "<"},
     {Operator::gt, ">"},
     {Operator::band, "&"},
     {Operator::bor, "|"},
     {Operator::land, "&&"},
     {Operator::lor, "||"}}};

inline std::array<std::pair<Operator, const char*>, 13> name_map = {  //
    {{Operator::neq, "ne"},
     {Operator::eq, "eq"},
     {Operator::gt, "gt"},
     {Operator::lt, "lt"},
     {Operator::leq, "le"},
     {Operator::geq, "ge"},
     {Operator::add, "add"},
     {Operator::sub, "sub"},
     {Operator::mul, "mul"},
     {Operator::div, "div"},
     {Operator::mod, "mod"},
     {Operator::band, "and"},
     {Operator::bor, "or"}}};

inline std::string toRawOperator(Operator oper) {
    for (const auto& [op, raw] : raw_map) {
        if (op == oper) {
            return raw;
        }
    }
    throw runtimeError("unknown operator: {}", (int)oper);
}

inline std::string serialize(const Operator& op) {
    return toRawOperator(op);
}

inline const char* toIrOperatorName(Operator oper) {
    for (const auto& [op, name] : name_map) {
        if (op == oper) {
            return name;
        }
    }
    throw runtimeError("unknown operator: {}", (int)oper);
}

inline Operator toOperator(const std::string& str) {
    for (const auto& [op, name] : name_map) {
        if (name == str) {
            return op;
        }
    }
    throw runtimeError("unknown operator: {}", str);
}

class BaseIR {
public:
    virtual ~BaseIR() = default;
    virtual std::string toString() const = 0;
    virtual std::string print(void* context = nullptr) const = 0;
    virtual std::string printRiscV(void* context = nullptr) const = 0;
    virtual std::string printBf(void* context = nullptr) const = 0;
    friend std::ostream& operator<<(std::ostream& os, const BaseIR& ir) {
        os << ir.print();
        return os;
    }
    virtual operator bool() const { return false; }  // default state
};

using IrObject = std::unique_ptr<BaseIR>;

// clang-format off
enum ValueType {
    Unknown, ZeroInit, FuncArgRef, GlobalAlloc, Alloc, Load, Store,
    GetPtr, GetElemPtr, Binary, Branch, Jump, Call, Return,
    // non-instrument values
    Type, Integer, Variable,
};

inline std::string toString(ValueType t) {
    switch (t) {
        case ValueType::Unknown: return "Unknown"; case ValueType::ZeroInit: return "ZeroInit";
        case ValueType::FuncArgRef: return "FuncArgRef"; case ValueType::GlobalAlloc: return "GlobalAlloc";
        case ValueType::Alloc: return "Alloc"; case ValueType::Load: return "Load";
        case ValueType::Store: return "Store"; case ValueType::GetPtr: return "GetPtr";
        case ValueType::GetElemPtr: return "GetElemPtr"; case ValueType::Binary: return "Binary";
        case ValueType::Branch: return "Branch"; case ValueType::Jump: return "Jump";
        case ValueType::Call: return "Call"; case ValueType::Return: return "Return";
        case ValueType::Type: return "Type"; case ValueType::Integer: return "Integer";
        case ValueType::Variable: return "Variable"; default: return "Invalid";
    }
}
// clang-format on

struct Context {
    std::string ret;
    int reg_cnt;
};

class ValueIR : public BaseIR {
public:
    ValueType type;
    std::string content;
    std::vector<IrObject> params;
    ValueIR() = default;
    ValueIR(ValueType type) : type(type) {}
    ValueIR(ValueType type, std::string str) : type(type), content(str) {}
    std::string toString() const override {
        return serializeClass("ValueIR", type, content, params);
    }
    operator bool() const override { return true; }

    std::string print(void* context) const override {
        assert(context != nullptr);
        auto ctx = (Context*)context;
        std::string str;
        switch (type) {
            case Type:
            case Integer: ctx->ret = content; break;
            case Variable:
                ctx->ret = "%" + std::to_string(ctx->reg_cnt++);
                str = ctx->ret + " = load @" + content + "\n";
                break;
            case Alloc:
                params[0]->print(context);
                str = "@" + content + " = alloc " + ctx->ret + "\n";
                break;
            case Store:
                str = params[0]->print(context);
                str += "store " + ctx->ret + ", @" + content + "\n";
                break;
            case Binary: {
                std::string op1, op2;
                str += params[0]->print(context);
                op1 = ctx->ret;
                str += params[1]->print(context);
                op2 = ctx->ret;
                ctx->ret = "%" + std::to_string(ctx->reg_cnt++);
                str += ctx->ret + " = " + content + " " + op1 + ", " + op2 + "\n";
                break;
            }
            case Return:
                str += params[0]->print(context);
                str += "ret " + ctx->ret + "\n";
                break;
            default:
                throw runtimeError("not implemented value type {}!", serialize(type));
        }
        return str;
    }
    std::string printRiscV(void*) const override;
    std::string printBf(void*) const override;

private:
};

class MultiValueIR : public BaseIR {
public:
    std::vector<IrObject> values;
    std::string toString() const override { return serializeClass("MultiValueIR", values); }
    std::string print(void* context) const override {
        std::string str;
        for (auto& value : values) {
            str += value->print(context);
        }
        return str;
    }
    std::string printRiscV(void*) const override;
    std::string printBf(void*) const override;
};

class BasicBlockIR : public BaseIR {
public:
    operator bool() const override { return true; }
    std::string entrance;
    std::vector<IrObject> insts;  // instruments
    std::map<std::string, std::string> symbol_map;
    // std::string exit;

    std::string toString() const override {
        return serializeClass("BasicBlockIR", entrance, symbol_map, insts);
    }
    std::string print(void* context) const override {
        assert(context != nullptr);
        std::string str = "%" + entrance + ":\n";
        std::string insts_str = "";
        for (const auto& inst : insts) {
            insts_str += inst->print(context);
        }
        str += addIndent(insts_str);
        return str;
    }
    std::string printRiscV(void* context) const override;
    std::string printBf(void* context) const override;
};

class FunctionIR : public BaseIR {
public:
    operator bool() const override { return true; }
    std::string name;
    IrObject ret_type;
    std::vector<IrObject> blocks;

    std::string toString() const override {return serializeClass("FunctionIR", name, ret_type, blocks);}
    std::string print(void*) const override {
        Context ctx = {"", 0};
        std::string str = "fun @" + name + "(): " + "i32" + " {\n";
        for (const auto& block : blocks) {
            str += block->print((void*)&ctx);
        }
        str += "}\n";
        return str;
    }
    std::string printRiscV(void*) const override;
    std::string printBf(void*) const override;
};

class ProgramIR : public BaseIR {
public:
    operator bool() const override { return true; }
    std::vector<IrObject> global_vars;  // global variables
    std::vector<IrObject> funcs;
    std::string toString() const override {
        return serializeClass("ProgramIR", global_vars, funcs);
    }
    std::string print(void*) const override {
        std::string str;
        // for (const auto& var : global_vars) {
        //     // TODO:
        // }
        for (const auto& func : funcs) {
            str += func->print();
        }
        return str;
    }
    std::string printRiscV(void*) const override;
    std::string printBf(void*) const override;
};

#endif