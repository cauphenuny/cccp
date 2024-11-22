#pragma once

#include "ir/ir.h"
#include "util.hpp"

#include <map>
#include <memory>
#include <string>
#include <variant>

struct BaseAST;
using AstObject = std::unique_ptr<BaseAST>;

struct BaseAST {
    int line, column;
    BaseAST* parent{nullptr};
    explicit BaseAST(int line, int column);
    virtual ~BaseAST() = default;
    virtual auto toString() const -> std::string = 0;
    virtual auto toIR() const -> IrObject {
        runtimeError("toIR() not implemented for {}", typeid(*this).name());
    }
};

using SymbolValue = std::variant<BaseAST*, int>;

struct ScopeAST : BaseAST {
    explicit ScopeAST(int line, int column);
    virtual ~ScopeAST() = default;
    void addVar(const std::string& name, SymbolValue value) const;
    auto hasVar(const std::string& name) const -> bool;
    auto getVar(const std::string& name) const -> SymbolValue;
    auto mangleIdent(const std::string& ident) const -> std::string;

protected:
    std::unique_ptr<std::map<std::string, SymbolValue>> symbol_table;
    int scope_id{-1};
};

struct ExpAST : BaseAST {
    explicit ExpAST(int line, int column);
    virtual ~ExpAST() = default;
    virtual auto isConstExpr() const -> bool = 0;
    virtual auto calc() const -> int = 0;
};

using ExpObject = std::unique_ptr<ExpAST>;

struct FuncTypeAST : BaseAST {
    explicit FuncTypeAST(int line, int column, const std::string& name);
    auto toString() const -> std::string override;
    auto toIR() const -> IrObject override;
    auto getIrType() const -> std::unique_ptr<BaseIR::Type>;

private:
    std::string name;
};

struct VarTypeAST : BaseAST {
    explicit VarTypeAST(int line, int column, const std::string& name);
    auto toString() const -> std::string override;
    auto toIR() const -> IrObject override;
    auto print() const -> std::string;

private:
    std::string name;
};

struct NumberAST : ExpAST {
    explicit NumberAST(int num, int line = -1, int column = -1);
    auto toString() const -> std::string override;
    auto toIR() const -> IrObject override;
    auto isConstExpr() const -> bool override;
    auto calc() const -> int override;

private:
    int number;
};

struct LValAST : ExpAST {
    explicit LValAST(int line, int column, const std::string& name);
    auto toString() const -> std::string override;
    auto toIR() const -> IrObject override;
    auto isConstExpr() const -> bool override;
    auto calc() const -> int override;
    auto getName() const -> std::string;
    auto getValue() const -> SymbolValue;

private:
    std::string ident;
};

struct PrimaryExpAST : ExpAST {
    enum Type {
        Number,
        Exp,
        LVal,
    };
    explicit PrimaryExpAST(Type type, ExpObject&& obj);
    auto toString() const -> std::string override;
    auto toIR() const -> IrObject override;
    auto isConstExpr() const -> bool override;
    auto calc() const -> int override;

private:
    Type type;
    ExpObject content;
};

struct FuncFParamAST : BaseAST {
    explicit FuncFParamAST(int line, int column, VarTypeAST* type, const std::string& ident);
    auto toString() const -> std::string override;
    auto toIR() const -> IrObject override;
    auto name() const -> std::string;
    auto type() const -> VarTypeAST*;
    auto getIrType() const -> decltype(BaseIR::Type::FunctionData::args)::value_type;

private:
    std::string ident;
    std::unique_ptr<VarTypeAST> type_;
};

struct FuncFParamsAST : BaseAST {
    explicit FuncFParamsAST(int line, int column);
    auto toString() const -> std::string override;
    auto toIR() const -> IrObject override;
    void add(FuncFParamAST*);
    auto print() const -> std::string;
    auto getIrType() const -> decltype(BaseIR::Type::FunctionData::args);

private:
    std::vector<std::unique_ptr<FuncFParamAST>> params;
};

struct FuncRParamsAST : BaseAST {
    explicit FuncRParamsAST(int line, int column);
    auto toString() const -> std::string override;
    // auto toIR() const -> IrObject override;
    void add(ExpAST* exp);

    std::vector<ExpObject> params;
};

struct UnaryExpAST : ExpAST {
    struct ExpContainer {
        Operator unary_op;
        ExpObject unary_exp;
    };
    struct FuncCallContainer {
        std::string func_name;
        std::unique_ptr<FuncRParamsAST> params;
    };
    explicit UnaryExpAST(int line, int column);
    explicit UnaryExpAST(ExpObject&& obj);
    explicit UnaryExpAST(int line, int column, ExpContainer&& cont);
    explicit UnaryExpAST(int line, int column, FuncCallContainer&& cont);
    auto toString() const -> std::string override;
    auto toIR() const -> IrObject override;
    auto isConstExpr() const -> bool override;
    auto calc() const -> int override;

private:
    std::variant<ExpObject, ExpContainer, FuncCallContainer> content;
};

struct BinaryExpAST : ExpAST {
    struct Container {
        ExpObject lhs;
        Operator op;
        ExpObject rhs;
    };
    explicit BinaryExpAST(ExpObject&& obj);
    explicit BinaryExpAST(int line, int column, Container&& cont);
    auto toString() const -> std::string override;
    auto toIR() const -> IrObject override;
    auto isConstExpr() const -> bool override;
    auto calc() const -> int override;

private:
    std::variant<ExpObject, Container> content;
};

struct ConstDefAST : BaseAST {
    explicit ConstDefAST(int line, int column, const std::string& ident, ExpObject&& init_exp);
    auto toString() const -> std::string override;
    auto toIR() const -> IrObject override;
    void writeSymbol() const;

private:
    std::string ident;
    ExpObject init_exp;
};

struct ConstDeclAST : BaseAST {
    explicit ConstDeclAST(int line, int column);
    auto toString() const -> std::string override;
    auto toIR() const -> IrObject override;
    void add(ConstDefAST* const_def);
    void setType(VarTypeAST* type);

private:
    AstObject type;
    std::vector<AstObject> const_defs;
};

struct VarDefAST : BaseAST {
    explicit VarDefAST(int line, int column, const std::string& ident, ExpObject&& init_exp);
    auto toString() const -> std::string override;
    auto toIR() const -> IrObject override;
    void writeSymbol() const;
    auto getName() const -> std::string;

private:
    std::string ident;
    ExpObject init_exp;
};

struct VarDeclAST : BaseAST {
    explicit VarDeclAST(int line, int column);
    auto toString() const -> std::string override;
    auto toIR() const -> IrObject override;
    void add(VarDefAST* var_def);
    void setType(VarTypeAST* type);

private:
    AstObject type;
    std::vector<AstObject> var_defs;
};

struct DeclAST : BaseAST {
    AstObject decl;
    explicit DeclAST(int line, int column);
    explicit DeclAST(AstObject&& decl);
    auto toString() const -> std::string override;
    auto toIR() const -> IrObject override;
};

struct StmtAST : BaseAST {
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
        ExpObject exp;
        AstObject stmt;
    };
    struct IfContainer {
        ExpObject exp;
        AstObject then_stmt, else_stmt;
    };
    friend auto toString(StmtAST::Type t) -> std::string;
    explicit StmtAST(int line, int column, Type type, AstObject&& content);
    explicit StmtAST(int line, int column, AssignContainer&& assign_stmt);
    explicit StmtAST(int line, int column, IfContainer&& if_stmt);
    explicit StmtAST(int line, int column, WhileContainer&& while_stmt);
    auto toString() const -> std::string override;
    auto toIR() const -> IrObject override;

private:
    Type type;
    std::variant<AstObject, AssignContainer, IfContainer, WhileContainer> content;
};

struct BlockItemAST : BaseAST {
    enum Type {
        Decl,
        Stmt,
    };
    friend auto toString(Type t) -> std::string;
    explicit BlockItemAST(Type type, AstObject&& _content);
    auto toString() const -> std::string override;
    auto toIR() const -> IrObject override;

private:
    Type type;
    AstObject content;
};

struct BlockAST : ScopeAST {
    explicit BlockAST(int line, int column);
    auto toString() const -> std::string override;
    auto toIR() const -> IrObject override;
    void addItem(AstObject&& item);

private:
    std::vector<AstObject> items;
};

struct FuncDefAST : ScopeAST {
    explicit FuncDefAST(int line, int column, FuncTypeAST* type, const std::string& ident,
                        FuncFParamsAST* params, AstObject&& block);
    auto toString() const -> std::string override;
    auto toIR() const -> IrObject override;
    auto name() const -> std::string;
    auto getIrType() const -> std::unique_ptr<BaseIR::Type>;

private:
    std::unique_ptr<FuncTypeAST> func_type;
    std::string ident;
    std::unique_ptr<FuncFParamsAST> params;
    AstObject block;
};

struct CompUnitAST : ScopeAST {
    explicit CompUnitAST(int line, int column);
    auto toString() const -> std::string override;
    auto toIR() const -> IrObject override;
    void add(FuncDefAST* func_def);
    auto hasFunc(const std::string&) const -> bool;
    auto getFunc(const std::string&) const -> FuncDefAST*;

private:
    std::vector<AstObject> func_defs;
    std::vector<AstObject> var_defs;
    std::map<std::string, FuncDefAST*> func_table;
    std::map<std::string, BaseAST*> var_table;
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