#ifndef IR_HPP
#define IR_HPP

#include <iostream>
#include <memory>
#include <string>
#include <vector>

class BaseIR
{
public:
    virtual ~BaseIR() = default;
    virtual std::string toString() const = 0;
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

class ValueIR : public BaseIR
{
public:
    ValueType type;
    std::string content;
    std::vector<IrObject> params;
    const char* typeName() const
    {
        switch (type) {
            case Integer: return "i32";
            case ZeroInit: return "zero_init";
            case FuncArgRef: return "func_arg_ref";
            case GlobalAlloc: return "global_alloc";
            case Alloc: return "alloc";
            case Load: return "load";
            case Store: return "store";
            case GetPtr: return "get_ptr";
            case GetElemPtr: return "get_elem_ptr";
            case Binary: return "binary";
            case Branch: return "branch";
            case Jump: return "jump";
            case Call: return "call";
            case Return: return "ret";
            default: return "unknown";
        }
    }
    std::string toString() const override
    {
        std::string str;
        switch (type) {
            case Integer: str = content; break;
            default: str = typeName(); break;
        }
        for (const auto& p : params) {
            str += " " + p->toString();
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

    std::string toString() const override
    {
        std::string str = "%" + entrance + ":\n";
        for (const auto& inst : insts) {
            str += "  " + inst->toString() + "\n";
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
    std::string toString() const override
    {
        std::string str =
            "fun @" + name + "(): " + ret_type->toString() + " {\n";
        for (const auto& block : blocks) {
            str += block->toString();
        }
        str += "}\n";
        return str;
    }
};

class ProgramIR : public BaseIR
{
public:
    std::vector<IrObject> global_vars;  // global variables
    std::vector<IrObject> funcs;
    std::string toString() const override
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
};

#endif