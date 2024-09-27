#ifndef IR_HPP
#define IR_HPP

#include <iostream>
#include <string>
#include <utility>
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

enum ValueType {
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
    std::string name;
    std::vector<ValueIR> params;
    std::string toString() const override
    {
        std::string str;
        switch (type) {
            case Integer: str = name; break;
            case Return: str = "ret"; break;
            default: break;
        }
        for (const auto& p : params) {
            str += " " + p.toString();
        }
        return str;
    }
};

class BasicBlockIR : public BaseIR
{
public:
    std::string entrance;
    std::vector<ValueIR> insts;  // instruments
    // std::string exit;
    std::string toString() const override
    {
        std::string str = "%" + entrance + ":\n";
        for (const auto& inst : insts) {
            str += "  " + inst.toString() + "\n";
        }
        return str;
    }
};

class FunctionIR : public BaseIR
{
public:
    std::string name;
    ValueIR ret_type;
    std::vector<BasicBlockIR> blocks;
    std::string toString() const override
    {
        std::string str = "fun @" + name + "(): i32 {\n";
        for (const auto& block : blocks) {
            str += block.toString();
        }
        str += "}\n";
        return str;
    }
};

class ProgramIR : public BaseIR
{
public:
    std::vector<ValueIR> global_vars;  // global variables
    std::vector<FunctionIR> funcs;
    std::string toString() const override
    {
        std::string str;
        for (const auto& var : global_vars) {
            // TODO:
        }
        for (const auto& func : funcs) {
            str += func.toString();
        }
        return str;
    }
};

#endif