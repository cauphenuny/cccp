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
    virtual std::string toIrString() const = 0;
    friend std::ostream& operator<<(std::ostream& os, const BaseAST& ast)
    {
        os << ast.toString();
        return os;
    }
};

class CompUnitAST : public BaseAST
{
public:
    std::unique_ptr<BaseAST> func_def;
    CompUnitAST() = default;
    std::string toString() const override
    {
        return "CompUnitAST {\n" +
               addIndent("func_def: " + func_def->toString() + "\n") + "}";
    }
    std::string toIrString() const override { return func_def->toIrString(); }
};

class FuncDefAST : public BaseAST
{
public:
    std::unique_ptr<BaseAST> func_type;
    std::string ident;
    std::unique_ptr<BaseAST> block;
    FuncDefAST() = default;
    std::string toString() const override
    {
        return "FuncDefAST {\n" +
               addIndent("func_type: " + func_type->toString() + ",\n") +
               addIndent("ident: \"" + ident + "\",\n") +
               addIndent("block: " + block->toString() + "\n") + "}";
    }
    std::string toIrString() const override
    {
        std::string str =
            "fun @" + ident + "(): " + func_type->toIrString() + " {\n";
        str += block->toIrString();
        str += "}\n";
        return str;
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
    std::string toIrString() const override { return "i32"; }
};

class BlockAST : public BaseAST
{
public:
    std::unique_ptr<BaseAST> stmt;
    BlockAST() = default;
    std::string toString() const override
    {
        return "BlockAST {\n" + addIndent("stmt: " + stmt->toString() + "\n") +
               "}";
    }
    std::string toIrString() const override
    {
        std::string str = "\%entry:\n" + stmt->toIrString();
        return str;
    }
};

class StmtAST : public BaseAST
{
public:
    int number;
    StmtAST() = default;
    std::string toString() const override
    {
        return "StmtAST {\n" +
               addIndent("number: " + std::to_string(number) + "\n") + "}";
    }
    std::string toIrString() const override
    {
        return "  ret " + std::to_string(number) + "\n";
    }
};

#endif