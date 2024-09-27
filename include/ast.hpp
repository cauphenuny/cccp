#ifndef AST_HPP
#define AST_HPP

#include "ir.hpp"
#include "koopa.h"

#include <iostream>
#include <memory>
#include <string>

inline std::string addIndent(const std::string& str, int indent = 1)
{
    std::string indent_str;
    for (int i = 0; i < indent; i++) indent_str += "    ";
    bool indent_flag = 1;
    std::string result_str;
    for (auto i : str) {
        if (indent_flag) result_str += indent_str, indent_flag = 0;
        result_str += i;
        if (i == '\n') {
            indent_flag = 1;
        }
    }
    return result_str;
}

class BaseAST
{
public:
    virtual ~BaseAST() = default;
    virtual std::string toString() const = 0;
    virtual IrObject toIr() const = 0;
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
        ProgramIR* ir = new ProgramIR();
        ir->funcs.push_back(func_def->toIr());
        return IrObject(ir);
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
        FunctionIR* ir = new FunctionIR();
        ir->name = ident;
        ir->ret_type = func_type->toIr();
        ir->blocks.push_back(block->toIr());
        return IrObject(ir);
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
        ValueIR* ir = new ValueIR();
        if (type == "int") {
            ir->type = ValueType::Integer;
        } else {
            ir->type = ValueType::Unknown;
        }
        ir->content = type;
        return IrObject(ir);
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
        BasicBlockIR* ir = new BasicBlockIR();
        ir->entrance = "entry";
        ir->insts.push_back(stmt->toIr());
        return IrObject(ir);
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
        ValueIR* ir = new ValueIR();
        ir->type = ValueType::Integer;
        ir->content = std::to_string(number);
        return IrObject(ir);
    }
};

class StmtAST : public BaseAST
{
public:
    AstObject number;
    StmtAST() = default;
    std::string toString() const override
    {
        return "StmtAST {\n" +
               addIndent("number: " + number->toString() + "\n") + "}";
    }
    IrObject toIr() const override
    {
        ValueIR* ir = new ValueIR();
        ir->type = ValueType::Return;
        ir->params.push_back(number->toIr());
        return IrObject(ir);
    }
};

#endif