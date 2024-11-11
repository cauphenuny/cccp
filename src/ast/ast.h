#pragma once

#include "ir/ir.h"
#include "util.hpp"

#include <string>
#include <variant>

struct BaseAST;
using AstObject = std::unique_ptr<BaseAST>;

struct BaseAST {
    int line, column;
    BaseAST* parent{nullptr};
    BaseAST() = delete;
    explicit BaseAST(int line, int column);
    virtual ~BaseAST() = default;
    virtual auto toString() const -> std::string = 0;
    virtual auto toIR() const -> IrObject {
        throw runtimeError("{}:{}: can not convert {} to IR", line, column, typeid(*this).name());
    }
    virtual auto isConstExpr() const -> bool {
        return false;
    }
    virtual auto calc() const -> int {
        throw runtimeError("{}:{}: can not calculate {}", line, column, typeid(*this).name());
    }
};

struct FuncTypeAST : public BaseAST {
    explicit FuncTypeAST(int line, int column, const std::string& name);
    auto toString() const -> std::string override;

private:
    std::string name;
};

struct VarTypeAST : public BaseAST {
    explicit VarTypeAST(int line, int column, const std::string& name);
    auto toString() const -> std::string override;

private:
    std::string name;
};

struct NumberAST : public BaseAST {
    explicit NumberAST(int num, int line = -1, int column = -1);
    auto toString() const -> std::string override;
    auto toIR() const -> IrObject override;
    auto isConstExpr() const -> bool override;
    auto calc() const -> int override;

private:
    int number;
};

struct ExpAST : public BaseAST {
    explicit ExpAST(AstObject&& _exp);
    auto toString() const -> std::string override;
    auto toIR() const -> IrObject override;
    auto isConstExpr() const -> bool override;
    auto calc() const -> int override;

private:
    AstObject exp;
};

using VarValue = std::variant<BaseAST*, int>;

struct LValAST : public BaseAST {
    explicit LValAST(int line, int column, const std::string& name);
    auto toString() const -> std::string override;
    auto toIR() const -> IrObject override;
    auto isConstExpr() const -> bool override;
    auto calc() const -> int override;
    auto getName() const -> std::string;
    auto getValue() const -> VarValue;

private:
    std::string ident;
};

struct PrimaryExpAST : public BaseAST {
    enum Type {
        Number,
        Exp,
        LVal,
    };
    explicit PrimaryExpAST(Type type, AstObject&& obj);
    auto toString() const -> std::string override;
    auto toIR() const -> IrObject override;
    auto isConstExpr() const -> bool override;
    auto calc() const -> int override;

private:
    Type type;
    AstObject content;
};

struct UnaryExpAST : public BaseAST {
    struct Container {
        Operator unary_op;
        AstObject unary_exp;
    };
    explicit UnaryExpAST(int line, int column);
    explicit UnaryExpAST(AstObject&& obj);
    explicit UnaryExpAST(Container&& cont);
    auto toString() const -> std::string override;
    auto toIR() const -> IrObject override;
    auto isConstExpr() const -> bool override;
    auto calc() const -> int override;

private:
    std::variant<AstObject, Container> content;
};

struct FuncFParamAST : public BaseAST {
    explicit FuncFParamAST(int line, int column);
    auto toString() const -> std::string override;
    auto toIR() const -> IrObject override;

private:
    std::vector<std::pair<AstObject, std::string>> _params;
};

struct FuncRParamAST : public BaseAST {
    explicit FuncRParamAST(int line, int column);
    auto toString() const -> std::string override;
    auto toIR() const -> IrObject override;

private:
    std::vector<AstObject> _params;
};

struct FuncCallAST : public BaseAST {
    std::string func_name;
    AstObject params;
};

struct BinaryExpAST : public BaseAST {
    struct Container {
        AstObject left;
        Operator op;
        AstObject right;
    };
    explicit BinaryExpAST(AstObject&& obj);
    explicit BinaryExpAST(int line, int column, Container&& cont);
    auto toString() const -> std::string override;
    auto toIR() const -> IrObject override;
    auto isConstExpr() const -> bool override;
    auto calc() const -> int override;

private:
    std::variant<AstObject, Container> content;
};

struct ConstDefAST : public BaseAST {
    explicit ConstDefAST(int line, int column, const std::string& ident, AstObject&& init_exp);
    auto toString() const -> std::string override;
    auto toIR() const -> IrObject override;
    void writeSymbol() const;

private:
    std::string ident;
    AstObject init_exp;
};

struct ConstDeclAST : public BaseAST {
    explicit ConstDeclAST(int line, int column);
    auto toString() const -> std::string override;
    auto toIR() const -> IrObject override;
    void add(ConstDefAST* const_def);
    void setType(VarTypeAST* type);

private:
    AstObject type;
    std::vector<AstObject> const_defs;
};

struct VarDefAST : public BaseAST {
    explicit VarDefAST(int line, int column, const std::string& ident, AstObject&& init_exp);
    auto toString() const -> std::string override;
    auto toIR() const -> IrObject override;
    void writeSymbol() const;
    auto getName() const -> std::string;

private:
    std::string ident;
    AstObject init_exp;
};

struct VarDeclAST : public BaseAST {
    explicit VarDeclAST(int line, int column);
    auto toString() const -> std::string override;
    auto toIR() const -> IrObject override;
    void add(VarDefAST* var_def);
    void setType(VarTypeAST* type);

private:
    AstObject type;
    std::vector<AstObject> var_defs;
};

struct DeclAST : public BaseAST {
    AstObject decl;
    explicit DeclAST(int line, int column);
    explicit DeclAST(AstObject&& decl);
    auto toString() const -> std::string override;
    auto toIR() const -> IrObject override;
};

struct StmtAST : public BaseAST {
    enum Type {
        Return,
        Assign,
        Block,
        Expr,
        If,
        While,
        Break,
        Continue,
    };
    struct AssignContainer {
        AstObject lval, exp;
    };
    struct WhileContainer {
        AstObject exp, stmt;
    };
    struct IfContainer {
        AstObject exp, then_stmt, else_stmt;
    };
    explicit StmtAST(int line, int column, Type type, AstObject&& content);
    explicit StmtAST(int line, int column, AssignContainer&& assign_stmt);
    explicit StmtAST(int line, int column, IfContainer&& if_stmt);
    explicit StmtAST(int line, int column, WhileContainer&& while_stmt);
    auto toString() const -> std::string override;
    auto toIR() const -> IrObject override;
    friend auto toString(StmtAST::Type t) -> std::string;

private:
    Type type;
    std::variant<AstObject, AssignContainer, IfContainer, WhileContainer> content;
};

struct BlockItemAST : public BaseAST {
    enum Type {
        Decl,
        Stmt,
    };
    explicit BlockItemAST(Type type, AstObject&& _content);
    auto toString() const -> std::string override;
    auto toIR() const -> IrObject override;
    friend auto toString(Type t) -> std::string;

private:
    Type type;
    AstObject content;
};

struct BlockAST : public BaseAST {
    explicit BlockAST(int line, int column);
    auto toString() const -> std::string override;
    auto toIR() const -> IrObject override;
    void addVar(const std::string& name, VarValue value) const;
    auto hasVar(const std::string& name) const -> bool;
    auto getVar(const std::string& name) const -> VarValue;
    std::string mangledName(const std::string& ident) const;
    void addItem(AstObject&& item);

private:
    std::unique_ptr<std::map<std::string, VarValue>> symbol_table;
    std::vector<AstObject> items;
    int scope_id{-1};
};

struct FuncDefAST : public BaseAST {
    explicit FuncDefAST(int line, int column, AstObject&& type, const std::string& ident,
                        AstObject&& block);
    auto toString() const -> std::string override;
    auto toIR() const -> IrObject override;
    auto name() const -> std::string;

private:
    AstObject func_type;
    std::string ident;
    AstObject block;
};

struct CompUnitAST : public BaseAST {
    explicit CompUnitAST(int line, int column);
    auto toString() const -> std::string override;
    auto toIR() const -> IrObject override;
    void add(FuncDefAST* func_def);

private:
    std::vector<AstObject> func_defs;
    std::map<std::string, FuncDefAST*> func_table;
};

template <typename T> T* find_ancestor(const BaseAST* cur) {
    do {
        cur = cur->parent;
        if (auto get = dynamic_cast<T*>(cur)) {
            return get;
        }
    } while (cur != nullptr);
    return nullptr;
}