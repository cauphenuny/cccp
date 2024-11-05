#ifndef RISCV_HPP
#define RISCV_HPP

#include "ir.hpp"
#include "util.hpp"

#include <cmath>
#include <format>
#include <iostream>
#include <string>

class BaseIR::RiscvContext {
public:
    static constexpr int REG_SIZE = 7;
    class Register {
    public:
        RiscvContext* ctx = nullptr;
        int idx = -1;
        Register() = default;
        Register(RiscvContext* context, int index) : ctx(context), idx(index) {}
        ~Register() {
            if (ctx) ctx->freeRegister(*this);
            ctx = nullptr;
        }
        std::string toString() const {
            if (idx < 0 || idx > REG_SIZE) throw runtimeError("invalid register %{}", idx);
            return "t" + std::to_string(idx);
        }
    };
    using RegisterPtr = std::unique_ptr<Register>;
    enum { REG, STACK, INT };
    struct {
        int type = 0;
        RegisterPtr reg_val;
        int int_val = 0;
    } ret;
    std::map<std::string, int> symbol_table;
    RegisterPtr newRegister() {
        int idx = -1;
        for (int i = 0; i < REG_SIZE; i++)
            if (!_reg_used[i]) {
                idx = i;
                break;
            }
        if (idx == -1) throw runtimeError("no available register");
        _reg_used[idx] = true;
        return std::make_unique<Register>(this, idx);
    }
    void freeRegister(const Register& reg) {
        debugLog("free {}", reg);
        if (reg.idx < 0 || reg.idx > REG_SIZE) throw runtimeError("invalid register %{}", reg.idx);
        _reg_used[reg.idx] = false;
    }
    int alloc(int size) {
        if (_stack_used + size > _stack_limit) throw runtimeError("stack overflow");
        int cur = _stack_used;
        _stack_used += size;
        return cur;
    }
    void allocVariable(const std::string& name) {
        int pos = alloc(4);
        symbol_table[name] = pos;
    }
    int getVariable(std::string name) {
        if (!symbol_table.contains(name))
            throw runtimeError("undefined variable {}", name);
        debugLog("got {}: {}", name, symbol_table[name]);
        return symbol_table[name];
    }
    explicit RiscvContext(int limit) : _stack_limit(limit) {}

private:
    bool _reg_used[REG_SIZE] = {false};
    int _stack_used = 0;
    int _stack_limit;
};

inline std::string ValueIR::printRiscV(RiscvContext* context) const {
    debugLog("{}", serializeClass("ValueIR", inst, content, params));
#ifdef DEBUG
    auto format = [&](const char* fmt, const auto&... args) {
        std::string str = std::vformat(fmt, std::make_format_args(args...));
        debugLog(BLUE "{}" RESET, str);
        return str;
    };
#else
    using std::format;
#endif
    using RegisterPtr = std::unique_ptr<RiscvContext::Register>;
    assert(context != nullptr);
    auto ctx = (RiscvContext*)context;
    std::string str;
    auto write_to_reg = [&](int type, const RegisterPtr& reg, int val) {
        switch (type) {
            case RiscvContext::STACK: str += format("lw\t{}, {}(sp)\n", reg, val); break;
            case RiscvContext::INT: str += format("li\t{}, {}\n", reg, val); break;
            default: break;
        }
    };
    auto return_to_stack = [&](RegisterPtr&& reg, int pos) {
        str += format("sw\t{}, {}(sp)\n", reg, pos);
        ctx->ret = {RiscvContext::STACK, nullptr, pos};
    };
    std::vector<decltype(RiscvContext::ret)> rets;
    for (const auto& param : params) {
        str += param->printRiscV(context);
        rets.push_back(std::move(ctx->ret));
    }
    switch (inst) {
        case Inst::Return: {
            auto& [type, reg, int_val] = rets[0];
            switch (type) {
                case RiscvContext::STACK: str += format("lw\ta0, {}(sp)\n", int_val); break;
                case RiscvContext::REG: str += format("mv\ta0, {}\n", reg); break;
                default: str += format("li\ta0, {}\n", int_val);
            }
            break;
        }
        case Inst::Binary: {
            auto& [type0, left, int_val0] = rets[0];
            auto& [type1, right, int_val1] = rets[1];
            if (type0 != RiscvContext::REG)
                left = ctx->newRegister(), write_to_reg(type0, left, int_val0);
            if (type1 != RiscvContext::REG)
                right = ctx->newRegister(), write_to_reg(type1, right, int_val1);
            auto res = ctx->newRegister();
            switch (toOperator(content)) {
                case Operator::gt: str += format("sgt\t{}, {}, {}\n", res, left, right); break;
                case Operator::lt: str += format("slt\t{}, {}, {}\n", res, left, right); break;
                case Operator::geq:
                    str += format("slt\t{}, {}, {}\n", res, left, right);
                    str += format("seqz\t{0}, {0}\n", res);
                    break;
                case Operator::leq:
                    str += format("sgt\t{}, {}, {}\n", res, left, right);
                    str += format("seqz\t{0}, {0}\n", res);
                    break;
                case Operator::neq:
                    str += format("xor\t{}, {}, {}\n", res, left, right);
                    str += format("seqz\t{0}, {0}\n", res);
                    str += format("seqz\t{0}, {0}\n", res);
                    break;
                case Operator::eq:
                    str += format("xor\t{}, {}, {}\n", res, left, right);
                    str += format("seqz\t{0}, {0}\n", res);
                    break;
                case Operator::mod: str += format("rem\t{}, {}, {}\n", res, left, right); break;
                case Operator::sub:
                case Operator::add:
                case Operator::mul:
                case Operator::div:
                case Operator::band:
                case Operator::bor:
                    str += format("{}\t{}, {}, {}\n", content, res, left, right);
                    break;
                default: throw runtimeError("not implemented binary operator {}!", content);
            }
            return_to_stack(std::move(res), ctx->alloc(4));
            break;
        }
        case Inst::Load: ctx->ret = {RiscvContext::STACK, nullptr, ctx->getVariable(content)}; break;
        case Inst::Alloc: ctx->allocVariable(content); break;
        case Inst::Store: {
            auto& [type, reg, int_val] = rets[0];
            if (type != RiscvContext::REG) reg = ctx->newRegister(), write_to_reg(type, reg, int_val);
            return_to_stack(std::move(reg), ctx->getVariable(content));
            break;
        }
        case Inst::Integer: ctx->ret = {RiscvContext::INT, nullptr, std::stoi(content)}; break;
        default: throw runtimeError("not implemented value type {}!", serialize(inst));
    }
    return str;
}

inline std::string MultiValueIR::printRiscV(RiscvContext* context) const {
    std::string str;
    for (const auto& value : values) {
        str += value->printRiscV(context);
    }
    return str;
}

inline std::string BasicBlockIR::printRiscV(RiscvContext* context) const {
    assert(context != nullptr);
    std::string str;
    for (const auto& inst : insts) {
        str += inst->printRiscV(context);
    }
    return str;
}

inline std::string FunctionIR::printRiscV(RiscvContext*) const {
    std::string str = name + ":\n";
    std::string content_str;
    int size = stackSize();
    debugLog("stack size: {}", size);
    RiscvContext ctx(size);
    size = (int)(std::ceil(size / 16.0) * 16);
    RiscvContext::RegisterPtr size_reg;
    if (size < (1 << 12)) {
        content_str += std::format("addi\tsp, sp, -{}\n", size);
    } else {
        size_reg = ctx.newRegister();
        content_str += std::format("li\t{}, {}\n", size_reg, std::to_string(size));
        content_str += std::format("add\tsp, sp, {}\n", size_reg);
    }
    for (const auto& block : blocks) {
        content_str += block->printRiscV(&ctx);
    }
    if (size < (1 << 12)) {
        content_str += std::format("addi\tsp, sp, {}\n", size);
    } else {
        content_str += std::format("sub\tsp, sp, {}\n", size_reg);
    }
    content_str += "ret\t\n";
    str += addIndent(content_str);
    return str;
}
inline std::string ProgramIR::printRiscV(RiscvContext*) const {
    std::string str;
    str += "  .text\n";
    for (const auto& obj : funcs) {
        if (const auto func = dynamic_cast<FunctionIR*>(obj.get())) {
            str += "  .globl " + func->name + "\n";
        }
    }
    for (const auto& func : funcs) {
        str += func->printRiscV();
    }
    return str;
}
#endif