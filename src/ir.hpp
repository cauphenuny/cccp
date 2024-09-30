#ifndef IR_HPP
#define IR_HPP

#include "util.hpp"

#include <cassert>
#include <iostream>
#include <memory>
#include <string>
#include <vector>

class BaseIR
{
public:
    virtual ~BaseIR() = default;
    virtual std::string toString(void* context = nullptr) const = 0;
    virtual std::string toAssembly(void* context = nullptr) const = 0;
    friend std::ostream& operator<<(std::ostream& os, const BaseIR& ir)
    {
        os << ir.toString();
        return os;
    }
};

using IrObject = std::unique_ptr<BaseIR>;

enum ValueType {
    Unknown,
    Integer,
    ZeroInit,
    FuncArgRef,
    GlobalAlloc,
    Alloc,
    Load,
    Store,
    GetPtr,
    GetElemPtr,
    Binary,
    Branch,
    Jump,
    Call,
    Return,
};

struct Context {
    std::string val_pos;
    int reg_cnt;
};

class ValueIR : public BaseIR
{
public:
    ValueType type;
    std::string content;
    std::vector<IrObject> params;
    std::string toString(void* context) const override
    {
        assert(context != nullptr);
        /*
        std::string str = content;
        for (auto& p : params) {
            str += ", (" + p->toString(context) + ")";
        }
        if (params.size()) str += "\n";
        return str;
        */
        assert(context != nullptr);
        // std::cerr << "entered toString" << std::endl;
        auto ctx = (Context*)context;
        std::string str, op1, op2;
        // std::cerr << "toString(), type = " << type << std::endl;
        switch (type) {
            case Integer:
                // std::cerr << "fuck..." << std::endl;
                ctx->val_pos = content;
                // std::cerr << "fuck!" << std::endl;
                break;
            case Binary:
                str += params[0]->toString(context);
                op1 = ctx->val_pos;
                str += params[1]->toString(context);
                op2 = ctx->val_pos;
                ctx->val_pos = "%" + std::to_string(ctx->reg_cnt++);
                str += ctx->val_pos + " = " + content + " " + op1 + ", " + op2 +
                       "\n";
                break;
            case Return:
                str += params[0]->toString(context);
                op1 = ctx->val_pos;
                str += "ret " + op1 + "\n";
                break;
            default: std::cerr << "not implemented value type!" << std::endl;
        }
        return str;
    }

private:
    std::string _toRegister(int count) const
    {
        if (count < 7)
            return "t" + std::to_string(count);
        else
            return "a" + std::to_string(count - 7 + 2);
    }

public:
    std::string toAssembly(void* context) const override
    {
        assert(context != nullptr);
        auto ctx = (Context*)context;
        std::string str, op1, op2, pos;
        switch (type) {
            case Return:
                str += params[0]->toAssembly(context);
                str += "mv\ta0, " + ctx->val_pos + "\n";
                str += "ret\t\n";
                break;
            case Binary:
                str += params[0]->toAssembly(context), op1 = ctx->val_pos;
                str += params[1]->toAssembly(context), op2 = ctx->val_pos;
                if (ctx->val_pos != "x0") {
                    pos = ctx->val_pos;
                } else {
                    pos = ctx->val_pos = _toRegister(ctx->reg_cnt++);
                }
                if (content == "gt") {
                    str += "sgt\t" + pos + ", " + op1 + ", " + op2 + "\n";
                } else if (content == "lt") {
                    str += "slt\t" + pos + ", " + op1 + ", " + op2 + "\n";
                } else if (content == "ge") {
                    str += "slt\t" + pos + ", " + op1 + ", " + op2 + "\n";
                    str += "seqz\t" + pos + ", " + pos + "\n";
                } else if (content == "le") {
                    str += "sgt\t" + pos + ", " + op1 + ", " + op2 + "\n";
                    str += "seqz\t" + pos + ", " + pos + "\n";
                } else if (content == "ne") {
                    str += "xor\t" + pos + ", " + op1 + ", " + op2 + "\n";
                    str += "seqz\t" + pos + ", " + pos + "\n";
                    str += "seqz\t" + pos + ", " + pos + "\n";
                } else if (content == "eq") {
                    str += "xor\t" + pos + ", " + op1 + ", " + op2 + "\n";
                    str += "seqz\t" + pos + ", " + pos + "\n";
                } else if (content == "mod") {
                    str += "rem\t" + pos + ", " + op1 + ", " + op2 + "\n";
                } else if (content == "sub" || content == "add" ||
                           content == "mul" || content == "div" ||
                           content == "and" || content == "or") {
                    str +=
                        content + "\t" + pos + ", " + op1 + ", " + op2 + "\n";
                } else {
                    std::cerr << "not implemented binary operator!"
                              << std::endl;
                }
                break;
            case Integer:
                if (content == "0") {
                    ctx->val_pos = "x0";
                } else {
                    ctx->val_pos = _toRegister(ctx->reg_cnt++);
                    str += "li\t" + ctx->val_pos + ", " + content + "\n";
                }
                break;
            default:
                std::cerr << "not implemented value type!" << std::endl;
                break;
        }
        return str;
    }
};

class BasicBlockIR : public BaseIR
{
public:
    std::string entrance;
    std::vector<IrObject> insts;  // instruments
    // std::string exit;

    std::string toString(void* context) const override
    {
        assert(context != nullptr);
        std::string str = "%" + entrance + ":\n";
        std::string insts_str = "";
        for (const auto& inst : insts) {
            insts_str += inst->toString(context);
        }
        str += addIndent(insts_str);
        return str;
    }
    std::string toAssembly(void* context) const override
    {
        assert(context != nullptr);
        std::string str;
        for (const auto& inst : insts) {
            str += inst->toAssembly(context);
        }
        return str;
    }
};

class FunctionIR : public BaseIR
{
public:
    std::string name;
    IrObject ret_type;
    std::vector<IrObject> blocks;
    std::string toString(void*) const override
    {
        Context ctx = {"", 0};
        std::string str = "fun @" + name + "(): " + "i32" + " {\n";
        for (const auto& block : blocks) {
            str += block->toString((void*)&ctx);
        }
        str += "}\n";
        return str;
    }
    std::string toAssembly(void*) const override
    {
        Context ctx = {"", 0};
        std::string str = name + ":\n";
        std::string blocks_str;
        for (const auto& block : blocks) {
            blocks_str += block->toAssembly((void*)&ctx);
        }
        str += addIndent(blocks_str);
        return str;
    }
};

class ProgramIR : public BaseIR
{
public:
    std::vector<IrObject> global_vars;  // global variables
    std::vector<IrObject> funcs;
    std::string toString(void*) const override
    {
        std::string str;
        // for (const auto& var : global_vars) {
        //     // TODO:
        // }
        for (const auto& func : funcs) {
            str += func->toString();
        }
        return str;
    }
    std::string toAssembly(void*) const override
    {
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
};

#endif