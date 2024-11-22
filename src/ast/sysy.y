%code requires {
  #include <memory>
  #include <string>
  #include "ast/ast.h"
  #include "ir/ir.h"
  // 添加行列信息的头文件
  extern int yylineno;
  extern int yycolumn;
}

%{

#include <iostream>
#include <memory>
#include <string>
#include "ast/ast.h"

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
  ExpAST* exp_val;
  CompUnitAST* comp_unit_val;
  FuncTypeAST* func_type_val;
  FuncDefAST* func_def_val;
  BlockAST* block_val;
  ConstDefAST* const_def_val;
  VarDefAST* var_def_val;
  ConstDeclAST* const_decl_val;
  VarDeclAST* var_decl_val;
  VarTypeAST* var_type_val;
  FuncFParamAST* func_f_param_val;
  FuncFParamsAST* func_f_params_val;
  FuncRParamsAST* func_r_params_val;
}

// lexer 返回的所有 token 种类的声明
%token <pos_val> INT RETURN LAND LOR CONST IF ELSE WHILE BREAK CONTINUE VOID
%token <str_val> IDENT
%token <int_val> INT_CONST
%token <op_val> REL_OP EQ_OP

// 非终结符的类型定义
%type <comp_unit_val> CompUnit
%type <func_def_val> FuncDef
%type <block_val> BlockItem
%type <const_def_val> ConstDef
%type <const_decl_val> ConstDefList
%type <var_def_val> VarDef
%type <var_decl_val> VarDefList
%type <var_type_val> VarType
%type <func_type_val> FuncType
%type <ast_val> Block Stmt OpenStmt CloseStmt SimpleStmt
%type <ast_val> Decl ConstDecl VarDecl 
%type <exp_val> Exp PrimaryExp UnaryExp MulExp AddExp RelExp EqExp LAndExp LOrExp LVal Number ConstInitVal InitVal
%type <op_val> UnaryOp MulOp AddOp
%type <func_f_param_val> FuncFParam
%type <func_f_params_val> FuncFParams
%type <func_r_params_val> FuncRParams

%%

/*

CompUnit      ::= [CompUnit] FuncDef;

Decl          ::= ConstDecl | VarDecl;
ConstDecl     ::= "const" BType ConstDef {"," ConstDef} ";";
BType         ::= "int";
ConstDef      ::= IDENT "=" ConstInitVal;
ConstInitVal  ::= ConstExp;
VarDecl       ::= BType VarDef {"," VarDef} ";";
VarDef        ::= IDENT | IDENT "=" InitVal;
InitVal       ::= Exp;

FuncDef       ::= FuncType IDENT "(" [FuncFParams] ")" Block;
FuncType      ::= "void" | "int";
FuncFParams   ::= FuncFParam {"," FuncFParam};
FuncFParam    ::= BType IDENT;

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
                | IDENT "(" [FuncRParams] ")"
                | ...;
FuncRParams   ::= Exp {"," Exp};
UnaryOp       ::= "+" | "-" | "!";
MulExp        ::= UnaryExp | MulExp ("*" | "/" | "%") UnaryExp;
AddExp        ::= MulExp | AddExp ("+" | "-") MulExp;
RelExp        ::= AddExp | RelExp ("<" | ">" | "<=" | ">=") AddExp;
EqExp         ::= RelExp | EqExp ("==" | "!=") RelExp;
LAndExp       ::= EqExp | LAndExp "&&" EqExp;
LOrExp        ::= LAndExp | LOrExp "||" LAndExp;
ConstExp      ::= Exp;

*/

Wrap
  : CompUnit {
    ast = AstObject($1);
  }
  ;

CompUnit
  : FuncDef {
    auto comp_unit = new CompUnitAST(0, 0);
    comp_unit->add($1);
    $$ = comp_unit;
  }
  | CompUnit FuncDef {
    $1->add($2);
    $$ = $1;
  }
  ;

FuncDef
  : FuncType IDENT '(' ')' Block {
    $$ = new FuncDefAST($2.line, $2.column, $1, *unique_ptr<string>($2.val), nullptr, AstObject($5));
  }
  | FuncType IDENT '(' FuncFParams ')' Block {
    $$ = new FuncDefAST($2.line, $2.column, $1, *unique_ptr<string>($2.val), $4, AstObject($6));
  }
  ;

FuncType
  : INT {
    $$ = new FuncTypeAST($1.line, $1.column, "int");
  }
  | VOID {
    $$ = new FuncTypeAST($1.line, $1.column, "void");
  }
  ;

FuncFParam
  : VarType IDENT {
    $$ = new FuncFParamAST($2.line, $2.column, $1, *unique_ptr<string>($2.val));
  }
  ;

FuncFParams
  : FuncFParam {
    auto ast = new FuncFParamsAST($1->line, $1->column);
    ast->add($1);
    $$ = ast;
  }
  | FuncFParams ',' FuncFParam {
    $1->add($3);
    $$ = $1;
  }
  ;

FuncRParams
  : Exp {
    auto ast = new FuncRParamsAST($1->line, $1->column);
    ast->add($1);
    $$ = ast;
  }
  | FuncRParams ',' Exp {
    $1->add($3);
    $$ = $1;
  }
  ;

BlockItem
  : {
    $$ = new BlockAST(yylineno, yycolumn);
  }
  | BlockItem Decl {
    $1->addItem(AstObject(new BlockItemAST(BlockItemAST::Decl, AstObject($2))));
    $$ = $1;
  }
  | BlockItem Stmt {
    $1->addItem(AstObject(new BlockItemAST(BlockItemAST::Stmt, AstObject($2))));
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
    $$ = new StmtAST($1.line, $1.column, (StmtAST::IfContainer){ExpObject($3), AstObject($5), nullptr});
  }
  | IF '(' Exp ')' CloseStmt ELSE OpenStmt {
    $$ = new StmtAST($1.line, $1.column, (StmtAST::IfContainer){ExpObject($3), AstObject($5), AstObject($7)});
  }
  | WHILE '(' Exp ')' OpenStmt {
    $$ = new StmtAST($1.line, $1.column, (StmtAST::WhileContainer){ExpObject($3), AstObject($5)});
  }
  ;

CloseStmt
  : SimpleStmt {
    $$ = $1;
  }
  | IF '(' Exp ')' CloseStmt ELSE CloseStmt {
    $$ = new StmtAST($1.line, $1.column, (StmtAST::IfContainer){ExpObject($3), AstObject($5), AstObject($7)});
  }
  | WHILE '(' Exp ')' CloseStmt {
    $$ = new StmtAST($1.line, $1.column, (StmtAST::WhileContainer){ExpObject($3), AstObject($5)});
  }
  ;

SimpleStmt
  : ';' {
    $$ = new StmtAST(yylineno, yycolumn, StmtAST::Expr, nullptr);
  }
  | Exp ';' {
    $$ = new StmtAST(yylineno, yycolumn, StmtAST::Expr, ExpObject($1));
  }
  | RETURN ';' {
    $$ = new StmtAST(yylineno, yycolumn, StmtAST::Return, nullptr);
  }
  | RETURN Exp ';' {
    $$ = new StmtAST(yylineno, yycolumn, StmtAST::Return, ExpObject($2));
  }
  | LVal '=' Exp ';' {
    $$ = new StmtAST(yylineno, yycolumn, (StmtAST::AssignContainer){ExpObject($1), ExpObject($3)});
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
    $$ = $1;
  }
  ;

PrimaryExp
  : '(' Exp ')' {
    $$ = new PrimaryExpAST(PrimaryExpAST::Exp, ExpObject($2));
  }
  | Number {
    $$ = new PrimaryExpAST(PrimaryExpAST::Number, ExpObject($1));
  }
  | LVal {
    $$ = new PrimaryExpAST(PrimaryExpAST::LVal, ExpObject($1));
  }
  ;

Number
  : INT_CONST {
    $$ = new NumberAST($1.val, $1.line, $1.column);
  }
  ;

UnaryExp
  : PrimaryExp {
    $$ = new UnaryExpAST(ExpObject($1));
  }
  | UnaryOp UnaryExp {
    $$ = new UnaryExpAST($1.line, $1.column, (UnaryExpAST::ExpContainer) {
      .unary_op  = $1.val, 
      .unary_exp = ExpObject($2)
    });
  }
  | IDENT '(' ')' {
    $$ = new UnaryExpAST($1.line, $1.column, (UnaryExpAST::FuncCallContainer) {
      .func_name = *unique_ptr<string>($1.val),
      .params = nullptr
    });
  }
  | IDENT '(' FuncRParams ')' {
    $$ = new UnaryExpAST($1.line, $1.column, (UnaryExpAST::FuncCallContainer) {
      .func_name = *unique_ptr<string>($1.val),
      .params = std::unique_ptr<FuncRParamsAST>($3)
    });
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
    $$ = new BinaryExpAST(ExpObject($1));
  }
  | MulExp MulOp UnaryExp {
    $$ = new BinaryExpAST($2.line, $2.column, (BinaryExpAST::Container) {
      .lhs = ExpObject($1),
      .op = $2.val,
      .rhs = ExpObject($3)
    });
  }

AddExp
  : MulExp {
    $$ = new BinaryExpAST(ExpObject($1));
  }
  | AddExp AddOp MulExp {
    $$ = new BinaryExpAST($2.line, $2.column, (BinaryExpAST::Container) {
      .lhs = ExpObject($1),
      .op = $2.val,
      .rhs = ExpObject($3)
    });
  }
  ;

RelExp
  : AddExp {
    $$ = new BinaryExpAST(ExpObject($1));
  }
  | RelExp REL_OP AddExp {
    $$ = new BinaryExpAST($2.line, $2.column, (BinaryExpAST::Container) {
      .lhs = ExpObject($1),
      .op = $2.val,
      .rhs = ExpObject($3)
    });
  }
  ;

EqExp
  : RelExp {
    $$ = new BinaryExpAST(ExpObject($1));
  }
  | EqExp EQ_OP RelExp {
    $$ = new BinaryExpAST($2.line, $2.column, (BinaryExpAST::Container) {
      .lhs = ExpObject($1),
      .op = $2.val,
      .rhs = ExpObject($3)
    });
  }
  ;

LAndExp
  : EqExp {
    $$ = new BinaryExpAST(ExpObject($1));
  }
  | LAndExp LAND EqExp {
    auto ast_left = new BinaryExpAST($1->line, $1->column, (BinaryExpAST::Container) {
      .lhs = ExpObject($1),
      .op = Operator::neq,
      .rhs = ExpObject(new NumberAST(0, $1->line, $1->column))
    });
    auto ast_right = new BinaryExpAST($3->line, $3->column, (BinaryExpAST::Container) {
      .lhs = ExpObject($3),
      .op = Operator::neq,
      .rhs = ExpObject(new NumberAST(0, $3->line, $3->column))
    });
    $$ = new BinaryExpAST($2.line, $2.column, (BinaryExpAST::Container) {
      .lhs = ExpObject(ast_left),
      .op = Operator::land,
      .rhs = ExpObject(ast_right)
    });
  }
  ;

LOrExp
  : LAndExp {
    $$ = new BinaryExpAST(ExpObject($1));
  }
  | LOrExp LOR LAndExp {
    auto ast_left = new BinaryExpAST($1->line, $1->column, (BinaryExpAST::Container) {
      .lhs = ExpObject($1),
      .op = Operator::neq,
      .rhs = ExpObject(new NumberAST(0, $1->line, $1->column))
    });
    auto ast_right = new BinaryExpAST($3->line, $3->column, (BinaryExpAST::Container) {
      .lhs = ExpObject($3),
      .op = Operator::neq,
      .rhs = ExpObject(new NumberAST(0, $3->line, $3->column))
    });
    $$ = new BinaryExpAST($2.line, $2.column, (BinaryExpAST::Container) {
      .lhs = ExpObject(ast_left),
      .op = Operator::lor,
      .rhs = ExpObject(ast_right)
    });
  }
  ;

LVal
  : IDENT {
    $$ = new LValAST($1.line, $1.column, *unique_ptr<string>($1.val));
  }
  ;

VarType
  : INT {
    $$ = new VarTypeAST($1.line, $1.column, "int");
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
    $$ = new ConstDefAST($1.line, $1.column, *unique_ptr<string>($1.val), ExpObject($3));
  }
  ;

ConstDefList
  : ConstDef {
    auto ast = new ConstDeclAST($1->line, $1->column);
    ast->add($1);
    $$ = ast;
  }
  | ConstDefList ',' ConstDef {
    $1->add($3);
    $$ = $1;
  }
  ;

ConstDecl
  : CONST VarType ConstDefList ';' {
    $3->setType($2);
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
    $$ = new VarDefAST($1.line, $1.column, *unique_ptr<string>($1.val), nullptr);
  }
  | IDENT '=' InitVal {
    $$ = new VarDefAST($1.line, $1.column, *unique_ptr<string>($1.val), ExpObject($3));
  }
  ;

VarDefList
  : VarDef {
    auto ast = new VarDeclAST($1->line, $1->column);
    ast->add($1);
    $$ = ast;
  }
  | VarDefList ',' VarDef {
    $1->add($3);
    $$ = $1;
  }
  ;

VarDecl
  : VarType VarDefList ';' {
    $2->setType($1);
    $$ = $2;
  }
  ;

%%

// 定义错误处理函数, 其中第二个参数是错误信息
// parser 如果发生错误 (例如输入的程序出现了语法错误), 就会调用这个函数
void yyerror(unique_ptr<BaseAST> &ast, const char *s) {
  cerr << format("[parse error] {}:{}: {}\n", yylineno, yycolumn, s);
}
