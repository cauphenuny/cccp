%code requires {
  #include <memory>
  #include <string>
  #include "ast/ast.hpp"
  #include "ir/ir.hpp"
  // 添加行列信息的头文件
  extern int yylineno;
  extern int yycolumn;
}

%{

#include <iostream>
#include <memory>
#include <string>
#include "ast/ast.hpp"

// 声明 lexer 函数和错误处理函数
int yylex();
void yyerror(std::unique_ptr<BaseAST> &ast, const char *s);

using namespace std;

%}

// 定义 parser 函数和错误处理函数的附加参数
// 我们需要返回一个字符串作为 AST, 所以我们把附加参数定义成字符串的智能指针
// 解析完成后, 我们要手动修改这个参数, 把它设置成解析得到的字符串
%define parse.error verbose
%parse-param { std::unique_ptr<BaseAST> &ast }

%union {
  struct {
    int line;
    int column;
  } pos_val;
  struct {
    std::string* val;
    int line;
    int column;
  } str_val;
  struct {
    long val;
    int line;
    int column;
  } int_val;
  struct {
    Operator val;
    int line;
    int column;
  } op_val;
  BaseAST* ast_val;
}

// lexer 返回的所有 token 种类的声明
%token <pos_val> INT RETURN LAND LOR CONST IF ELSE WHILE BREAK CONTINUE
%token <str_val> IDENT
%token <int_val> INT_CONST
%token <op_val> REL_OP EQ_OP

// 非终结符的类型定义
%type <ast_val> FuncDef FuncType Block BlockItem Stmt OpenStmt CloseStmt SimpleStmt
%type <ast_val> Decl ConstDecl ConstDef VarType ConstDefList VarDecl VarDef VarDefList
%type <ast_val> Exp PrimaryExp UnaryExp MulExp AddExp RelExp EqExp LAndExp LOrExp LVal Number ConstInitVal InitVal
%type <op_val> UnaryOp MulOp AddOp

%%

/*

CompUnit      ::= FuncDef;

Decl          ::= ConstDecl | VarDecl;
ConstDecl     ::= "const" BType ConstDef {"," ConstDef} ";";
BType         ::= "int";
ConstDef      ::= IDENT "=" ConstInitVal;
ConstInitVal  ::= ConstExp;
VarDecl       ::= BType VarDef {"," VarDef} ";";
VarDef        ::= IDENT | IDENT "=" InitVal;
InitVal       ::= Exp;

FuncDef       ::= FuncType IDENT "(" ")" Block;
FuncType      ::= "int";

Block         ::= "{" {BlockItem} "}";
BlockItem     ::= Decl | Stmt;
Stmt          ::= LVal "=" Exp ";"
                | [Exp] ";"
                | Block
                | "if" "(" Exp ")" Stmt ["else" Stmt]
                | "while" "(" Exp ")" Stmt
                | "break" ";"
                | "continue" ";"
                | "return" [Exp] ";";
Exp           ::= LOrExp;
LVal          ::= IDENT;
PrimaryExp    ::= "(" Exp ")" | LVal | Number;
Number        ::= INT_CONST;
UnaryExp      ::= PrimaryExp | UnaryOp UnaryExp;
UnaryOp       ::= "+" | "-" | "!";
MulExp        ::= UnaryExp | MulExp ("*" | "/" | "%") UnaryExp;
AddExp        ::= MulExp | AddExp ("+" | "-") MulExp;
RelExp        ::= AddExp | RelExp ("<" | ">" | "<=" | ">=") AddExp;
EqExp         ::= RelExp | EqExp ("==" | "!=") RelExp;
LAndExp       ::= EqExp | LAndExp "&&" EqExp;
LOrExp        ::= LAndExp | LOrExp "||" LAndExp;
ConstExp      ::= Exp;

*/

CompUnit
  : FuncDef {
    auto comp_unit = new CompUnitAST($1->line, $1->column);
    comp_unit->func_def = unique_ptr<BaseAST>($1);
    comp_unit->func_def->parent = comp_unit;
    ast = unique_ptr<CompUnitAST>(comp_unit);
  }
  ;

FuncDef
  : FuncType IDENT '(' ')' Block {
    auto ast = new FuncDefAST($2.line, $2.column);
    ast->func_type = unique_ptr<BaseAST>($1);
    ast->func_type->parent = ast;

    ast->ident = *unique_ptr<string>($2.val);

    ast->block = unique_ptr<BaseAST>($5);
    ast->block->parent = ast;
    $$ = ast;
  }
  ;

// 同上, 不再解释
FuncType
  : INT {
    auto ast = new FuncTypeAST($1.line, $1.column);
    ast->type = "int";
    $$ = ast;
  }
  ;

BlockItem
  : {
    auto ast = new BlockAST(yylineno, yycolumn);
    $$ = ast;
  }
  | BlockItem Decl {
    auto ast = new BlockItemAST(BlockItemAST::Decl, AstObject($2));
    ast->parent = $1;
    dynamic_cast<BlockAST*>($1)->stmts.push_back(AstObject(ast));
    $$ = $1;
  }
  | BlockItem Stmt {
    auto ast = new BlockItemAST(BlockItemAST::Stmt, AstObject($2));
    ast->parent = $1;
    dynamic_cast<BlockAST*>($1)->stmts.push_back(AstObject(ast));
    $$ = $1;
  }
  ;

Block
  : '{' BlockItem '}' {
    $$ = $2;
  }
  ;

Stmt
  : OpenStmt | CloseStmt {
    $$ = $1;
  };

OpenStmt
  : IF '(' Exp ')' Stmt {
    $$ = new StmtAST($1.line, $1.column, (StmtAST::IfContainer){AstObject($3), AstObject($5), nullptr});
  }
  | IF '(' Exp ')' CloseStmt ELSE OpenStmt {
    $$ = new StmtAST($1.line, $1.column, (StmtAST::IfContainer){AstObject($3), AstObject($5), AstObject($7)});
  }
  | WHILE '(' Exp ')' OpenStmt {
    $$ = new StmtAST($1.line, $1.column, (StmtAST::WhileContainer){AstObject($3), AstObject($5)});
  }
  ;

CloseStmt
  : SimpleStmt {
    $$ = $1;
  }
  | IF '(' Exp ')' CloseStmt ELSE CloseStmt {
    $$ = new StmtAST($1.line, $1.column, (StmtAST::IfContainer){AstObject($3), AstObject($5), AstObject($7)});
  }
  | WHILE '(' Exp ')' CloseStmt {
    $$ = new StmtAST($1.line, $1.column, (StmtAST::WhileContainer){AstObject($3), AstObject($5)});
  }
  ;

SimpleStmt
  : ';' {
    $$ = new StmtAST(yylineno, yycolumn, StmtAST::Expr, nullptr);
  }
  | Exp ';' {
    $$ = new StmtAST(yylineno, yycolumn, StmtAST::Expr, AstObject($1));
  }
  | RETURN ';' {
    $$ = new StmtAST(yylineno, yycolumn, StmtAST::Return, nullptr);
  }
  | RETURN Exp ';' {
    $$ = new StmtAST(yylineno, yycolumn, StmtAST::Return, AstObject($2));
  }
  | LVal '=' Exp ';' {
    $$ = new StmtAST(yylineno, yycolumn, (StmtAST::AssignContainer){AstObject($1), AstObject($3)});
  }
  | BREAK ';' {
    $$ = new StmtAST($1.line, $1.column, StmtAST::Break, nullptr);
  }
  | CONTINUE ';' {
    $$ = new StmtAST($1.line, $1.column, StmtAST::Continue, nullptr);
  }
  | Block {
    $$ = new StmtAST(yylineno, yycolumn, StmtAST::Block, AstObject($1));
  }
  ;

Exp
  : LOrExp {
    auto ast = new ExpAST(AstObject($1));
    $$ = ast;
  }
  ;

PrimaryExp
  : '(' Exp ')' {
    auto ast = new PrimaryExpAST(PrimaryExpAST::Exp, AstObject($2));
    $$ = ast;
  }
  | Number {
    auto ast = new PrimaryExpAST(PrimaryExpAST::Number, AstObject($1));
    $$ = ast;
  }
  | LVal {
    auto ast = new PrimaryExpAST(PrimaryExpAST::LVal, AstObject($1));
    $$ = ast;
  }
  ;

Number
  : INT_CONST {
    auto ast = new NumberAST($1.val, $1.line, $1.column);
    $$ = ast;
  }
  ;

UnaryExp
  : PrimaryExp {
    auto ast = new UnaryExpAST(AstObject($1));
    $$ = ast;
  }
  | UnaryOp UnaryExp {
    auto ast = new UnaryExpAST((UnaryExpAST::Container) {
      .unary_op  = $1.val, 
      .unary_exp = AstObject($2)
    });
    $$ = ast;
  }
  ;

UnaryOp
  : '+' { $$ = {Operator::add, yylineno, yycolumn}; }
  | '-' { $$ = {Operator::sub, yylineno, yycolumn}; }
  | '!' { $$ = {Operator::no, yylineno, yycolumn}; }
  ;

MulOp
  : '*' { $$ = {Operator::mul, yylineno, yycolumn}; }
  | '/' { $$ = {Operator::div, yylineno, yycolumn}; }
  | '%' { $$ = {Operator::mod, yylineno, yycolumn}; }
  ;

AddOp
  : '+' { $$ = {Operator::add, yylineno, yycolumn}; }
  | '-' { $$ = {Operator::sub, yylineno, yycolumn}; }
  ;

MulExp
  : UnaryExp {
    auto ast = new BinaryExpAST(AstObject($1));
    $$ = ast;
  }
  | MulExp MulOp UnaryExp {
    auto ast = new BinaryExpAST($2.line, $2.column, (BinaryExpAST::Container) {
      .left = AstObject($1),
      .op = $2.val,
      .right = AstObject($3)
    });
    $$ = ast;
  }

AddExp
  : MulExp {
    auto ast = new BinaryExpAST(AstObject($1));
    $$ = ast;
  }
  | AddExp AddOp MulExp {
    auto ast = new BinaryExpAST($2.line, $2.column, (BinaryExpAST::Container) {
      .left = AstObject($1),
      .op = $2.val,
      .right = AstObject($3)
    });
    $$ = ast;
  }
  ;

RelExp
  : AddExp {
    auto ast = new BinaryExpAST(AstObject($1));
    $$ = ast;
  }
  | RelExp REL_OP AddExp {
    auto ast = new BinaryExpAST($2.line, $2.column, (BinaryExpAST::Container) {
      .left = AstObject($1),
      .op = $2.val,
      .right = AstObject($3)
    });
    $$ = ast;
  }
  ;

EqExp
  : RelExp {
    auto ast = new BinaryExpAST(AstObject($1));
    $$ = ast;
  }
  | EqExp EQ_OP RelExp {
    auto ast = new BinaryExpAST($2.line, $2.column, (BinaryExpAST::Container) {
      .left = AstObject($1),
      .op = $2.val,
      .right = AstObject($3)
    });
    $$ = ast;
  }
  ;

LAndExp
  : EqExp {
    auto ast = new BinaryExpAST(AstObject($1));
    $$ = ast;
  }
  | LAndExp LAND EqExp {
    auto ast_left = new BinaryExpAST($1->line, $1->column, (BinaryExpAST::Container) {
      .left = AstObject($1),
      .op = Operator::neq,
      .right = AstObject(new NumberAST(0, $1->line, $1->column))
    });
    auto ast_right = new BinaryExpAST($3->line, $3->column, (BinaryExpAST::Container) {
      .left = AstObject($3),
      .op = Operator::neq,
      .right = AstObject(new NumberAST(0, $3->line, $3->column))
    });
    auto ast = new BinaryExpAST($2.line, $2.column, (BinaryExpAST::Container) {
      .left = AstObject(ast_left),
      .op = Operator::land,
      .right = AstObject(ast_right)
    });
    $$ = ast;
  }
  ;

LOrExp
  : LAndExp {
    auto ast = new BinaryExpAST(AstObject($1));
    $$ = ast;
  }
  | LOrExp LOR LAndExp {
    auto ast_left = new BinaryExpAST($1->line, $1->column, (BinaryExpAST::Container) {
      .left = AstObject($1),
      .op = Operator::neq,
      .right = AstObject(new NumberAST(0, $1->line, $1->column))
    });
    auto ast_right = new BinaryExpAST($3->line, $3->column, (BinaryExpAST::Container) {
      .left = AstObject($3),
      .op = Operator::neq,
      .right = AstObject(new NumberAST(0, $3->line, $3->column))
    });
    auto ast = new BinaryExpAST($2.line, $2.column, (BinaryExpAST::Container) {
      .left = AstObject(ast_left),
      .op = Operator::lor,
      .right = AstObject(ast_right)
    });
    $$ = ast;
  }
  ;

LVal
  : IDENT {
    auto ast = new LValAST($1.line, $1.column);
    ast->ident = *unique_ptr<string>($1.val);
    $$ = ast;
  }
  ;

VarType
  : INT {
    auto ast = new VarTypeAST($1.line, $1.column);
    ast->type = "int";
    $$ = ast;
  }

Decl
  : ConstDecl {
    $$ = $1;
  }
  | VarDecl {
    $$ = $1;
  }
  ;

ConstInitVal
  : Exp {
    $$ = $1;
  }
  ;

ConstDef
  : IDENT '=' ConstInitVal {
    auto ast = new ConstDefAST($1.line, $1.column);
    $3->parent = ast;
    ast->ident = *unique_ptr<string>($1.val);
    ast->init_exp = AstObject($3);
    $$ = ast;
  }
  ;

ConstDefList
  : ConstDef {
    auto ast = new ConstDeclAST($1->line, $1->column);
    $1->parent = ast, ast->const_defs.push_back(AstObject($1));
    $$ = ast;
  }
  | ConstDefList ',' ConstDef {
    $3->parent = $1, dynamic_cast<ConstDeclAST*>($1)->const_defs.push_back(AstObject($3));
    $$ = $1;
  }
  ;

ConstDecl
  : CONST VarType ConstDefList ';' {
    auto const_def_list = dynamic_cast<ConstDeclAST*>($3);
    const_def_list->type = AstObject($2);
    const_def_list->type->parent = $3;
    $$ = $3;
  }
  ;

InitVal
  : Exp {
    $$ = $1;
  }
  ;

VarDef
  : IDENT {
    auto ast = new VarDefAST($1.line, $1.column);
    ast->ident = *unique_ptr<string>($1.val);
    ast->init_exp = nullptr;
    $$ = ast;
  }
  | IDENT '=' InitVal {
    auto ast = new VarDefAST($1.line, $1.column);
    $3->parent = ast;
    ast->ident = *unique_ptr<string>($1.val);
    ast->init_exp = AstObject($3);
    $$ = ast;
  }
  ;

VarDefList
  : VarDef {
    auto ast = new VarDeclAST($1->line, $1->column);
    $1->parent = ast, ast->var_defs.push_back(AstObject($1));
    $$ = ast;
  }
  | VarDefList ',' VarDef {
    $3->parent = $1, dynamic_cast<VarDeclAST*>($1)->var_defs.push_back(AstObject($3));
    $$ = $1;
  }
  ;

VarDecl
  : VarType VarDefList ';' {
    auto def_list = dynamic_cast<VarDeclAST*>($2);
    def_list->type = AstObject($1);
    def_list->type->parent = $2;
    $$ = $2;
  }
  ;

%%

// 定义错误处理函数, 其中第二个参数是错误信息
// parser 如果发生错误 (例如输入的程序出现了语法错误), 就会调用这个函数
void yyerror(unique_ptr<BaseAST> &ast, const char *s) {
  cerr << format("[parse error] {}:{}: {}\n", yylineno, yycolumn, s);
}
