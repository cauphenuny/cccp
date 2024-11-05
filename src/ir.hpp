#ifndef IR_HPP
#define IR_HPP

#include "util.hpp"

#include <cassert>
#include <functional>
#include <map>
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
    std::not_equal_to<T>(),   // no,    // !
    std::plus<T>(),           // add,   // +
    std::minus<T>(),          // sub,   // -
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
/*
    note: for unary expression, the first parameter should be 0, and the second parameter is the
given parameter
    example: for a unary expression `!a`, the function should be `function_map[(size_t)no](0, a)`
*/

template <typename T> std::function<T(T, T)> getFunction(Operator op) {
    return function_map<T>[(size_t)op];
}

// clang-format off
inline std::array<std::pair<Operator, const char*>, 16> raw_map = {{
    {Operator::no, "!"},{Operator::eq, "=="}, {Operator::neq, "!="},
    {Operator::leq, "<="}, {Operator::geq, ">="}, {Operator::lt, "<"}, {Operator::gt, ">"},
    {Operator::add, "+"}, {Operator::sub, "-"}, {Operator::mul, "*"}, {Operator::div, "/"}, {Operator::mod, "%"},
    {Operator::band, "&"}, {Operator::bor, "|"}, {Operator::land, "&&"}, {Operator::lor, "||"}
}};

inline std::array<std::pair<Operator, const char*>, 13> name_map = {{
    {Operator::neq, "ne"}, {Operator::eq, "eq"},
    {Operator::leq, "le"}, {Operator::geq, "ge"}, {Operator::lt, "lt"}, {Operator::gt, "gt"},
    {Operator::add, "add"}, {Operator::sub, "sub"}, {Operator::mul, "mul"}, {Operator::div, "div"}, {Operator::mod, "mod"},
    {Operator::band, "and"}, {Operator::bor, "or"}
}};
// clang-format on

inline std::string toString(const Operator& oper) {
    for (const auto& [op, raw] : raw_map) {
        if (op == oper) {
            return raw;
        }
    }
    throw runtimeError("unknown operator: {}", (int)oper);
}

inline const char* toIrOperatorName(Operator oper) {
    for (const auto& [op, name] : name_map) {
        if (op == oper) {
            return name;
        }
    }
    throw runtimeError("unknown operator: {}", oper);
}

inline Operator toOperator(const std::string& str) {
    for (const auto& [op, name] : name_map) {
        if (name == str) {
            return op;
        }
    }
    throw runtimeError("unknown operator: {}", str);
}

// clang-format off
enum class Inst {
    Unknown, ZeroInit, FuncArgRef, GlobalAlloc, Alloc, Load, Store,
    GetPtr, GetElemPtr, Binary, Branch, Jump, Call, Return,
    // non-instrument values
    Integer,
};

inline std::string toString(Inst t) {
    switch (t) {
        case Inst::Unknown: return "Unknown"; 
        case Inst::ZeroInit: return "ZeroInit";
        case Inst::FuncArgRef: return "FuncArgRef"; 
        case Inst::GlobalAlloc: return "GlobalAlloc";
        case Inst::Alloc: return "Alloc"; 
        case Inst::Load: return "Load";
        case Inst::Store: return "Store"; 
        case Inst::GetPtr: return "GetPtr";
        case Inst::GetElemPtr: return "GetElemPtr"; 
        case Inst::Binary: return "Binary";
        case Inst::Branch: return "Branch"; 
        case Inst::Jump: return "Jump";
        case Inst::Call: return "Call"; 
        case Inst::Return: return "Return";
        case Inst::Integer: return "Integer";
        default: return "Invalid";
    }
}
// clang-format on

class IrType {
public:
    enum class Tag { Unit, Int32, Array, Pointer, Function };
    struct ArrayData {
        std::unique_ptr<IrType> base;
        int size;
    };
    struct PointerData {
        std::unique_ptr<IrType> base;
    };
    struct FunctionData {
        std::unique_ptr<IrType> ret;
        std::vector<std::unique_ptr<IrType>> args;
    };
    Tag tag;
    std::variant<std::monostate, ArrayData, PointerData, FunctionData> data;
    explicit IrType(Tag t) : tag(t) {}
    std::string toString() const {
        switch (tag) {
            case Tag::Unit: return "unit";
            case Tag::Int32: return "i32";
            case Tag::Array: {
                auto& [base, size] = std::get<ArrayData>(data);
                return std::format("{}[{}]", base, size);
            }
            case Tag::Pointer: {
                auto& [base] = std::get<PointerData>(data);
                return std::format("{}*", base);
            }
            case Tag::Function: {
                auto& [ret, args] = std::get<FunctionData>(data);
                std::string args_str;
                for (const auto& arg : args) {
                    if (!args_str.empty()) args_str += ", ";
                    args_str += arg->toString();
                }
                return std::format("{}({})", ret, args_str);
            }
            default: throw runtimeError("invalid type tag {}", (int)tag);
        }
    }
};

class BaseIR {
public:
    std::unique_ptr<IrType> type;
    BaseIR() { type = std::make_unique<IrType>(IrType::Tag::Unit); }
    virtual ~BaseIR() = default;
    virtual std::string toString() const = 0;
    struct IrContext;
    virtual std::string print(IrContext* context = nullptr) const = 0;
    class RiscvContext;
    virtual std::string printRiscV(RiscvContext* context = nullptr) const = 0;
    struct BfContext;
    virtual std::string printBf(BfContext* context = nullptr) const = 0;
    virtual int stackSize() const {
        throw runtimeError("can not calculate stack size for {}", typeid(*this).name());
    }
};

using IrObject = std::unique_ptr<BaseIR>;

struct BaseIR::IrContext {
    std::string ret;
    int cnt;
};

class ValueIR : public BaseIR {
public:
    Inst inst;
    std::string content;
    std::string comment;
    std::vector<IrObject> params;
    ValueIR() = delete;
    explicit ValueIR(Inst inst) : inst(inst) { setType(); }
    ValueIR(Inst inst, const std::string& str) : inst(inst), content(str) { setType(); }
    std::string toString() const override {
        return serializeClass("ValueIR", inst, content, params);
    }

    std::string print(IrContext* context) const override {
        assert(context != nullptr);
        auto ctx = (IrContext*)context;
        std::string str;
        std::vector<std::string> ret;
        for (const auto& param : params) {
            str += param->print(context);
            ret.push_back(ctx->ret);
        }
        using std::format;
        switch (inst) {
            case Inst::Integer: ctx->ret = content; break;
            case Inst::Load:
                ctx->ret = format("%{}", ctx->cnt++);
                str += format("{} = load @{}\n", ctx->ret, content);
                break;
            case Inst::Alloc:
                str += format("@{} = alloc {}\n", content, type);
                break;
            case Inst::Store:
                str += format("store {}, @{}\n", ret[0], content);
                break;
            case Inst::Binary: {
                ctx->ret = format("%{}", ctx->cnt++);
                str += format("{} = {} {}, {}\n", ctx->ret, content, ret[0], ret[1]);
                break;
            }
            case Inst::Return:
                str += "ret " + ret[0] + "\n";
                break;
            default: throw runtimeError("not implemented value type {}!", serialize(inst));
        }
        return str;
    }
    std::string printRiscV(RiscvContext*) const override;
    std::string printBf(BfContext*) const override;
    int stackSize() const override {
        int size;
        switch (inst) {
            case Inst::Binary:
            case Inst::Alloc: size = 4; break;
            default: size = 0;
        }
        for (const auto& param : params) size += param->stackSize();
        return size;
    }

private:
    void setType() {
        switch (inst) {
            case Inst::Integer:
            case Inst::Load:
            case Inst::Binary: type = std::make_unique<IrType>(IrType::Tag::Int32); break;
            default: break;
        }
    }
};

class MultiValueIR : public BaseIR {
public:
    std::vector<IrObject> values;
    std::string toString() const override { return serializeClass("MultiValueIR", values); }
    std::string print(IrContext* context) const override {
        std::string str;
        for (const auto& value : values) {
            str += value->print(context);
        }
        return str;
    }
    void add(IrObject&& value) { if (value) values.emplace_back(std::move(value)); }
    std::string printRiscV(RiscvContext*) const override;
    std::string printBf(BfContext*) const override;
    int stackSize() const override {
        int size = 0;
        for (const auto& value : values) {
            size += value->stackSize();
        }
        return size;
    }
};

class BasicBlockIR : public BaseIR {
public:
    std::string entrance;
    std::vector<IrObject> insts;  // instruments
    std::map<std::string, std::string> symbol_map;
    // std::string exit;

    std::string toString() const override {
        return serializeClass("BasicBlockIR", type, entrance, symbol_map, insts);
    }
    std::string print(IrContext* context) const override {
        assert(context != nullptr);
        std::string str = "%" + entrance + ":\n";
        std::string insts_str;
        for (const auto& inst : insts) {
            insts_str += inst->print(context);
        }
        str += addIndent(insts_str);
        return str;
    }
    std::string printRiscV(RiscvContext* context) const override;
    std::string printBf(BfContext* context) const override;
    void add(IrObject&& inst) {if (inst) insts.emplace_back(std::move(inst));}
    int stackSize() const override {
        int size = 0;
        for (const auto& inst : insts) {
            size += inst->stackSize();
        }
        return size;
    }
};

class FunctionIR : public BaseIR {
public:
    std::string name;
    std::vector<IrObject> blocks;
    explicit FunctionIR(const std::string& name) : name(name) {
        type = std::make_unique<IrType>(IrType::Tag::Int32);
    }

    std::string toString() const override { return serializeClass("FunctionIR", name, blocks); }
    std::string print(IrContext*) const override {
        IrContext ctx = {"", 0};
        std::string blocks_str;
        for (const auto& block : blocks) {
            blocks_str += block->print(&ctx);
        }
        return std::format("fun @{}(): {} {{\n{}}}\n", name, type, blocks_str);
    }
    std::string printRiscV(RiscvContext*) const override;
    std::string printBf(BfContext*) const override;
    int stackSize() const override {
        int size = 0;
        for (const auto& block : blocks) {
            size += block->stackSize();
        }
        return size;
    }
};

class ProgramIR : public BaseIR {
public:
    std::vector<IrObject> global_vars;  // global variables
    std::vector<IrObject> funcs;
    std::string toString() const override {
        return serializeClass("ProgramIR", global_vars, funcs);
    }
    std::string print(IrContext*) const override {
        std::string str;
        // for (const auto& var : global_vars) {
        //     // TODO:
        // }
        for (const auto& func : funcs) {
            str += func->print();
        }
        return str;
    }
    std::string printRiscV(RiscvContext*) const override;
    std::string printBf(BfContext*) const override;
};

#endif