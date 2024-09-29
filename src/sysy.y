%code requires {
  #include <memory>
  #include <string>
  #include <ast.hpp>
}

%{

#include <iostream>
#include <memory>
#include <string>
#include <ast.hpp>

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

// yylval 的定义, 我们把它定义成了一个联合体 (union)
// 因为 token 的值有的是字符串指针, 有的是整数
// 之前我们在 lexer 中用到的 str_val 和 int_val 就是在这里被定义的
// 至于为什么要用字符串指针而不直接用 string 或者 unique_ptr<string>?
// 请自行 STFW 在 union 里写一个带析构函数的类会出现什么情况
%union {
  std::string* str_val;
  int int_val;
  Operator op_val;
  BaseAST* ast_val;
}

// lexer 返回的所有 token 种类的声明
// 注意 IDENT 和 INT_CONST 会返回 token 的值, 分别对应 str_val 和 int_val
%token INT RETURN LAND LOR
%token <str_val> IDENT
%token <int_val> INT_CONST
%token <op_val> REL_OP EQ_OP

// 非终结符的类型定义
%type <ast_val> FuncDef FuncType Block Stmt Number
%type <ast_val> Exp PrimaryExp UnaryExp MulExp AddExp RelExp EqExp LAndExp LOrExp
%type <op_val> UnaryOp MulOp AddOp

%%

/*
CompUnit  ::= FuncDef;

FuncDef   ::= FuncType IDENT "(" ")" Block;
FuncType  ::= "int";

Block     ::= "{" Stmt "}";
Number    ::= INT_CONST;
Stmt        ::= "return" Exp ";";

Exp         ::= LOrExp;

PrimaryExp  ::= "(" Exp ")" | Number;
UnaryExp    ::= PrimaryExp | UnaryOp UnaryExp;
UnaryOp     ::= "+" | "-" | "!";

MulExp      ::= UnaryExp | MulExp MulOp UnaryExp;
MulOp       ::= "*" | "/" | "%";
AddExp      ::= MulExp | AddExp AddOp MulExp;
AddOp       ::= "+" | "-";

RelExp      ::= AddExp | RelExp REL_OP AddExp;
EqExp       ::= RelExp | EqExp EQ_OP RelExp;
LAndExp     ::= EqExp | LAndExp LAND EqExp;
LOrExp      ::= LAndExp | LOrExp LOR LAndExp;

*/

CompUnit
  : FuncDef {
    auto comp_unit = make_unique<CompUnitAST>();
    comp_unit->func_def = unique_ptr<BaseAST>($1);
    ast = move(comp_unit);
  }
  ;

FuncDef
  : FuncType IDENT '(' ')' Block {
    auto ast = new FuncDefAST();
    ast->func_type = unique_ptr<BaseAST>($1);
    ast->ident = *unique_ptr<string>($2);
    ast->block = unique_ptr<BaseAST>($5);
    $$ = ast;
  }
  ;

// 同上, 不再解释
FuncType
  : INT {
    auto ast = new FuncTypeAST();
    ast->type = "int";
    $$ = ast;
  }
  ;

Block
  : '{' Stmt '}' {
    auto ast = new BlockAST();
    ast->stmt = unique_ptr<BaseAST>($2);
    $$ = ast;
  }
  ;

Stmt
  : RETURN Exp ';' {
    auto ast = new StmtAST();
    ast->exp = unique_ptr<BaseAST>($2);
    $$ = ast;
  }
  ;

Exp
  : LOrExp {
    auto ast = new ExpAST();
    ast->exp = unique_ptr<BaseAST>($1);
    $$ = ast;
  }
  ;

PrimaryExp
  : '(' Exp ')' {
    auto ast = new PrimaryExpAST();
    ast->type = PrimaryExpAST::Real;
    ast->content = unique_ptr<BaseAST>($2);
    $$ = ast;
  }
  | Number {
    auto ast = new PrimaryExpAST();
    ast->type = PrimaryExpAST::Virtual;
    ast->content = unique_ptr<BaseAST>($1);
    $$ = ast;
  }
  ;

Number
  : INT_CONST {
    auto ast = new NumberAST();
    ast->number = $1;
    $$ = ast;
  }
  ;

UnaryExp
  : PrimaryExp {
    auto ast = new UnaryExpAST();
    ast->type = UnaryExpAST::Virtual;
    ast->content = unique_ptr<BaseAST>($1);
    $$ = ast;
  }
  | UnaryOp UnaryExp {
    auto ast = new UnaryExpAST();
    ast->type = UnaryExpAST::Real;
    ast->content = (UnaryExpAST::Container) {
      .unary_op  = $1, 
      .unary_exp = unique_ptr<BaseAST>($2)
    };
    $$ = ast;
  }
  ;

UnaryOp
  : '+' { $$ = Operator::add; }
  | '-' { $$ = Operator::sub; }
  | '!' { $$ = Operator::no; }
  ;

MulOp
  : '*' { $$ = Operator::mul; }
  | '/' { $$ = Operator::div; }
  | '%' { $$ = Operator::mod; }
  ;

AddOp
  : '+' { $$ = Operator::add; }
  | '-' { $$ = Operator::sub; }
  ;

MulExp
  : UnaryExp {
    auto ast = new BinaryExpAST();
    ast->type = BinaryExpAST::Virtual;
    ast->content = AstObject($1);
    $$ = ast;
  }
  | MulExp MulOp UnaryExp {
    auto ast = new BinaryExpAST();
    ast->type = BinaryExpAST::Real;
    ast->content = (BinaryExpAST::Container) {
      .left = AstObject($1),
      .op = $2,
      .right = AstObject($3)
    };
    $$ = ast;
  }

AddExp
  : MulExp {
    auto ast = new BinaryExpAST();
    ast->type = BinaryExpAST::Virtual;
    ast->content = unique_ptr<BaseAST>($1);
    $$ = ast;
  }
  | AddExp AddOp MulExp {
    auto ast = new BinaryExpAST();
    ast->type = BinaryExpAST::Real;
    ast->content = (BinaryExpAST::Container) {
      .left = AstObject($1),
      .op = $2,
      .right = AstObject($3)
    };
    $$ = ast;
  }
  ;

RelExp
  : AddExp {
    auto ast = new BinaryExpAST();
    ast->type = BinaryExpAST::Virtual;
    ast->content = unique_ptr<BaseAST>($1);
    $$ = ast;
  }
  | RelExp REL_OP AddExp {
    auto ast = new BinaryExpAST();
    ast->type = BinaryExpAST::Real;
    ast->content = (BinaryExpAST::Container) {
      .left = AstObject($1),
      .op = $2,
      .right = AstObject($3)
    };
    $$ = ast;
  }
  ;

EqExp
  : RelExp {
    auto ast = new BinaryExpAST();
    ast->type = BinaryExpAST::Virtual;
    ast->content = unique_ptr<BaseAST>($1);
    $$ = ast;
  }
  | EqExp EQ_OP RelExp {
    auto ast = new BinaryExpAST();
    ast->type = BinaryExpAST::Real;
    ast->content = (BinaryExpAST::Container) {
      .left = AstObject($1),
      .op = $2,
      .right = AstObject($3)
    };
    $$ = ast;
  }
  ;

LAndExp
  : EqExp {
    auto ast = new BinaryExpAST(AstObject($1));
    $$ = ast;
  }
  | LAndExp LAND EqExp {
    auto ast_left = new BinaryExpAST((BinaryExpAST::Container) {
      .left = AstObject($1),
      .op = Operator::neq,
      .right = AstObject(new NumberAST(0))
    });
    auto ast_right = new BinaryExpAST((BinaryExpAST::Container) {
      .left = AstObject($3),
      .op = Operator::neq,
      .right = AstObject(new NumberAST(0))
    });
    auto ast = new BinaryExpAST((BinaryExpAST::Container) {
      .left = AstObject(ast_left),
      .op = Operator::band,
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
    auto ast_left = new BinaryExpAST((BinaryExpAST::Container) {
      .left = AstObject($1),
      .op = Operator::neq,
      .right = AstObject(new NumberAST(0))
    });
    auto ast_right = new BinaryExpAST((BinaryExpAST::Container) {
      .left = AstObject($3),
      .op = Operator::neq,
      .right = AstObject(new NumberAST(0))
    });
    auto ast = new BinaryExpAST((BinaryExpAST::Container) {
      .left = AstObject(ast_left),
      .op = Operator::bor,
      .right = AstObject(ast_right)
    });
    $$ = ast;
  }
  ;


%%

// 定义错误处理函数, 其中第二个参数是错误信息
// parser 如果发生错误 (例如输入的程序出现了语法错误), 就会调用这个函数
void yyerror(unique_ptr<BaseAST> &ast, const char *s) {
  cerr << "error: " << s << endl;
  if (ast == nullptr) cerr << "nullptr" << endl;
  else cerr << "ast: " << ast->toString() << endl;
  cerr << "done" << endl;
}
