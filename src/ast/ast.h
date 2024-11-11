#ifndef AST_H
#define AST_H

#include "ir/ir.h"
#include "util.hpp"

#include <map>
#include <string>
#include <variant>

class BaseAST;

using SymbolTable = std::map<std::string, std::variant<BaseAST*, int>>;
using AstObject = std::unique_ptr<BaseAST>;

class BaseAST {
public:
    int line, column;
    std::unique_ptr<SymbolTable> symbol_table;
    int scope_id{-1};
    BaseAST* parent{nullptr};
    BaseAST() = delete;
    explicit BaseAST(int line, int column);
    virtual ~BaseAST() = default;
    virtual auto toString() const -> std::string = 0;
    virtual auto toIR() const -> IrObject {
        throw runtimeError("{}:{}: can not convert {} to IR", line, column, typeid(*this).name());
    }
    virtual auto isConstExpr() const -> bool { return false; }
    virtual auto calc() const -> int {
        throw runtimeError("{}:{}: can not calculate {}", line, column, typeid(*this).name());
    }
};

class FuncTypeAST : public BaseAST {
public:
    explicit FuncTypeAST(int line, int column);
    std::string type;
    auto toString() const -> std::string override;
};

class VarTypeAST : public BaseAST {
public:
    explicit VarTypeAST(int line, int column);
    std::string type;
    auto toString() const -> std::string override;
};

class NumberAST : public BaseAST {
public:
    int number;
    explicit NumberAST(int num, int line = -1, int column = -1);
    auto toString() const -> std::string override;
    auto toIR() const -> IrObject override;
    auto isConstExpr() const -> bool override;
    auto calc() const -> int override;
};

class ExpAST : public BaseAST {
public:
    AstObject exp;
    explicit ExpAST(AstObject&& _exp);
    auto toString() const -> std::string override;
    auto toIR() const -> IrObject override;
    auto isConstExpr() const -> bool override;
    auto calc() const -> int override;
};

class LValAST : public BaseAST {
public:
    explicit LValAST(int line, int column);
    std::string ident;
    auto toString() const -> std::string override;
    auto toIR() const -> IrObject override;
    auto isConstExpr() const -> bool override;
    auto calc() const -> int override;
    auto getName() const -> std::string;
    auto getValue() const -> std::variant<BaseAST*, int>;
};

class PrimaryExpAST : public BaseAST {
public:
    enum Type {
        Number,
        Exp,
        LVal,
    } type;
    AstObject content;
    explicit PrimaryExpAST(Type type, AstObject&& obj);
    auto toString() const -> std::string override;
    auto toIR() const -> IrObject override;
    auto isConstExpr() const -> bool override;
    auto calc() const -> int override;
};

class UnaryExpAST : public BaseAST {
public:
    struct Container {
        Operator unary_op;
        AstObject unary_exp;
    };
    std::variant<AstObject, Container> content;
    explicit UnaryExpAST(int line, int column);
    explicit UnaryExpAST(AstObject&& obj);
    explicit UnaryExpAST(Container&& cont);
    auto toString() const -> std::string override;
    auto toIR() const -> IrObject override;
    auto isConstExpr() const -> bool override;
    auto calc() const -> int override;
};

class BinaryExpAST : public BaseAST {
public:
    struct Container {
        AstObject left;
        Operator op;
        AstObject right;
    };
    std::variant<AstObject, Container> content;
    explicit BinaryExpAST(AstObject&& obj);
    explicit BinaryExpAST(int line, int column, Container&& cont);
    auto toString() const -> std::string override;
    auto toIR() const -> IrObject override;
    auto isConstExpr() const -> bool override;
    auto calc() const -> int override;
};

class ConstDefAST : public BaseAST {
public:
    std::string ident;
    AstObject init_exp;
    explicit ConstDefAST(int line, int column);
    explicit ConstDefAST(const std::string& ident, AstObject&& init_exp);
    auto toString() const -> std::string override;
    auto toIR() const -> IrObject override;
    void writeSymbol() const;
};

class ConstDeclAST : public BaseAST {
public:
    AstObject type;
    std::vector<AstObject> const_defs;
    explicit ConstDeclAST(int line, int column);
    auto toString() const -> std::string override;
    auto toIR() const -> IrObject override;
};

class VarDefAST : public BaseAST {
public:
    std::string ident;
    AstObject init_exp;
    explicit VarDefAST(int line, int column);
    explicit VarDefAST(const std::string& ident, AstObject&& init_exp);
    auto toString() const -> std::string override;
    auto toIR() const -> IrObject override;
    void writeSymbol() const;
    auto getName() const -> std::string;
};

class VarDeclAST : public BaseAST {
public:
    AstObject type;
    std::vector<AstObject> var_defs;
    explicit VarDeclAST(int line, int column);
    auto toString() const -> std::string override;
    auto toIR() const -> IrObject override;
};

class DeclAST : public BaseAST {
public:
    AstObject decl;
    explicit DeclAST(int line, int column);
    explicit DeclAST(AstObject&& decl);
    auto toString() const -> std::string override;
    auto toIR() const -> IrObject override;
};

class StmtAST : public BaseAST {
public:
    enum Type {
        Return,
        Assign,
        Block,
        Expr,
        If,
        While,
        Break,
        Continue,
    } type;
    struct AssignContainer {
        AstObject lval, exp;
    };
    struct WhileContainer {
        AstObject exp, stmt;
    };
    struct IfContainer {
        AstObject exp, then_stmt, else_stmt;
    };
    std::variant<AstObject, AssignContainer, IfContainer, WhileContainer> content;
    explicit StmtAST(int line, int column, Type type, AstObject&& _content);
    explicit StmtAST(int line, int column, AssignContainer&& assign_stmt);
    explicit StmtAST(int line, int column, IfContainer&& if_stmt);
    explicit StmtAST(int line, int column, WhileContainer&& while_stmt);
    auto toString() const -> std::string override;
    auto toIR() const -> IrObject override;
    friend auto toString(StmtAST::Type t) -> std::string;
};

class BlockItemAST : public BaseAST {
public:
    enum Type {
        Decl,
        Stmt,
    } type;
    AstObject content;
    explicit BlockItemAST(Type type, AstObject&& _content);
    auto toString() const -> std::string override;
    auto toIR() const -> IrObject override;
    friend auto toString(Type t) -> std::string;
};

class BlockAST : public BaseAST {
public:
    std::vector<AstObject> stmts;
    explicit BlockAST(int line, int column);
    auto toString() const -> std::string override;
    auto toIR() const -> IrObject override;
};

class FuncDefAST : public BaseAST {
public:
    AstObject func_type;
    std::string ident;
    AstObject block;
    explicit FuncDefAST(int line, int column);
    explicit FuncDefAST(const std::string& ident, AstObject&& block);
    auto toString() const -> std::string override;
    auto toIR() const -> IrObject override;
};

class CompUnitAST : public BaseAST {
public:
    AstObject func_def;
    explicit CompUnitAST(int line, int column);
    auto toString() const -> std::string override;
    auto toIR() const -> IrObject override;
};

#endif