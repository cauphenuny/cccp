#ifndef RISCV_HPP
#define RISCV_HPP

#include "ir.hpp"

#include <string>

inline std::string toRegister(int count) {
    if (count < 7)
        return "t" + std::to_string(count);
    else
        return "a" + std::to_string(count - 7 + 2);
}

inline std::string ValueIR::toAssembly(void* context) const {
    assert(context != nullptr);
    auto ctx = (Context*)context;
    std::string str, op1, op2, pos;
    switch (type) {
        case Return:
            str += params[0]->toAssembly(context);
            str += "mv\ta0, " + ctx->ret + "\n";
            str += "ret\t\n";
            break;
        case Binary: {
            str += params[0]->toAssembly(context), op1 = ctx->ret;
            str += params[1]->toAssembly(context), op2 = ctx->ret;
            if (ctx->ret != "x0") {
                pos = ctx->ret;
            } else {
                pos = ctx->ret = toRegister(ctx->reg_cnt++);
            }
            auto op = toOperator(content);
            switch (op) {
                case Operator::gt: str += "sgt\t" + pos + ", " + op1 + ", " + op2 + "\n"; break;
                case Operator::lt: str += "slt\t" + pos + ", " + op1 + ", " + op2 + "\n"; break;
                case Operator::geq:
                    str += "slt\t" + pos + ", " + op1 + ", " + op2 + "\n";
                    str += "seqz\t" + pos + ", " + pos + "\n";
                    break;
                case Operator::leq:
                    str += "sgt\t" + pos + ", " + op1 + ", " + op2 + "\n";
                    str += "seqz\t" + pos + ", " + pos + "\n";
                    break;
                case Operator::neq:
                    str += "xor\t" + pos + ", " + op1 + ", " + op2 + "\n";
                    str += "seqz\t" + pos + ", " + pos + "\n";
                    str += "seqz\t" + pos + ", " + pos + "\n";
                    break;
                case Operator::eq:
                    str += "xor\t" + pos + ", " + op1 + ", " + op2 + "\n";
                    str += "seqz\t" + pos + ", " + pos + "\n";
                    break;
                case Operator::mod: str += "rem\t" + pos + ", " + op1 + ", " + op2 + "\n"; break;
                case Operator::sub:
                case Operator::add:
                case Operator::mul:
                case Operator::div:
                case Operator::band:
                case Operator::bor:
                    str += content + "\t" + pos + ", " + op1 + ", " + op2 + "\n";
                    break;
                default: eprintf("not implemented binary operator %s!", content.c_str());
            }
            break;
        }
        case Integer:
            if (content == "0") {
                ctx->ret = "x0";
            } else {
                ctx->ret = toRegister(ctx->reg_cnt++);
                str += "li\t" + ctx->ret + ", " + content + "\n";
            }
            break;
        default: eprintf("not implemented value type %d!", type); break;
    }
    return str;
}

inline std::string MultiValueIR::toAssembly(void* context) const {
    std::string str;
    for (auto& value : values) {
        str += value->toAssembly(context);
    }
    return str;
}

inline std::string BasicBlockIR::toAssembly(void* context) const {
    assert(context != nullptr);
    std::string str;
    for (const auto& inst : insts) {
        str += inst->toAssembly(context);
    }
    return str;
}

inline std::string FunctionIR::toAssembly(void*) const {
    Context ctx = {"", 0};
    std::string str = name + ":\n";
    std::string blocks_str;
    for (const auto& block : blocks) {
        blocks_str += block->toAssembly((void*)&ctx);
    }
    str += addIndent(blocks_str);
    return str;
}
inline std::string ProgramIR::toAssembly(void*) const {
    std::string str;
    str += "  .text\n";
    for (const auto& obj : funcs) {
        auto func = static_cast<FunctionIR*>(obj.get());
        if (func) {
            str += "  .globl " + func->name + "\n";
        }
    }
    for (const auto& func : funcs) {
        str += func->toAssembly();
    }
    return str;
}
#endif