%option noyywrap
%option nounput
%option noinput

%{

#include <cstdlib>
#include <string>
#include "ast/ast.h"
#include "ir/ir.h"

// 因为 Flex 会用到 Bison 中关于 token 的定义
// 所以需要 include Bison 生成的头文件
#include "sysy.tab.hpp"

using namespace std;

int yycolumn = 1;
extern int yylineno;

void update_column() {
    yycolumn += yyleng;
}

void update_line() {
    yylineno++;
    yycolumn = 1;
}

%}

/* 空白符和注释 */
WhiteSpace    [ \t\r]*
LineComment   "//".*
ParaComment   "/"\*([^*]|\*+[^"/"])*\*"/"

/* 标识符 */
Identifier    [a-zA-Z_][a-zA-Z0-9_]*

/* 整数字面量 */
Decimal       [1-9][0-9]*
Octal         0[0-7]*
Hexadecimal   0[xX][0-9a-fA-F]+

%%

{WhiteSpace}    { update_column(); /* 忽略, 不做任何操作 */ }
{LineComment}   { update_column(); /* 忽略, 不做任何操作 */ }
{ParaComment}   { update_column(); /* 忽略, 不做任何操作 */ }

\n              { update_line(); /* 更新行号 */ }

"int"           { yylval.pos_val = {yylineno, yycolumn}; update_column(); return INT; }
"return"        { yylval.pos_val = {yylineno, yycolumn}; update_column(); return RETURN; }
"const"         { yylval.pos_val = {yylineno, yycolumn}; update_column(); return CONST; }
"if"            { yylval.pos_val = {yylineno, yycolumn}; update_column(); return IF; }
"else"          { yylval.pos_val = {yylineno, yycolumn}; update_column(); return ELSE; }
"while"         { yylval.pos_val = {yylineno, yycolumn}; update_column(); return WHILE; }
"break"         { yylval.pos_val = {yylineno, yycolumn}; update_column(); return BREAK; }
"continue"      { yylval.pos_val = {yylineno, yycolumn}; update_column(); return CONTINUE; }
"void"          { yylval.pos_val = {yylineno, yycolumn}; update_column(); return VOID; }

"<"             { yylval.op_val = {Operator::lt, yylineno, yycolumn}; update_column(); return REL_OP; }
">"             { yylval.op_val = {Operator::gt, yylineno, yycolumn}; update_column(); return REL_OP; }
"<="            { yylval.op_val = {Operator::leq, yylineno, yycolumn}; update_column(); return REL_OP; }
">="            { yylval.op_val = {Operator::geq, yylineno, yycolumn}; update_column(); return REL_OP; }

"=="            { yylval.op_val = {Operator::eq, yylineno, yycolumn}; update_column(); return EQ_OP; }
"!="            { yylval.op_val = {Operator::neq, yylineno, yycolumn}; update_column(); return EQ_OP; }

"&&"            { yylval.pos_val = {yylineno, yycolumn}; update_column(); return LAND; }
"||"            { yylval.pos_val = {yylineno, yycolumn}; update_column(); return LOR; }

{Identifier}    { yylval.str_val = {new string(yytext), yylineno, yycolumn}; update_column(); return IDENT; }

{Decimal}       { yylval.int_val = {strtol(yytext, nullptr, 0), yylineno, yycolumn}; update_column(); return INT_CONST; }
{Octal}         { yylval.int_val = {strtol(yytext, nullptr, 0), yylineno, yycolumn}; update_column(); return INT_CONST; }
{Hexadecimal}   { yylval.int_val = {strtol(yytext, nullptr, 0), yylineno, yycolumn}; update_column(); return INT_CONST; }

.               { update_column(); return yytext[0]; }

%%
