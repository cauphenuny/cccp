%option noyywrap
%option nounput
%option noinput

%{

#include <cstdlib>
#include <string>
#include "ast.hpp"
#include "riscv.hpp"
#include "bf.hpp"

// 因为 Flex 会用到 Bison 中关于 token 的定义
// 所以需要 include Bison 生成的头文件
#include "sysy.tab.hpp"

using namespace std;

%}

/* 空白符和注释 */
WhiteSpace    [ \t\n\r]*
LineComment   "//".*
ParaComment   "/"\*([^*]|\*+[^"/"])*\*"/"

/* 标识符 */
Identifier    [a-zA-Z_][a-zA-Z0-9_]*

/* 整数字面量 */
Decimal       [1-9][0-9]*
Octal         0[0-7]*
Hexadecimal   0[xX][0-9a-fA-F]+

%%

{WhiteSpace}    { /* 忽略, 不做任何操作 */ }
{LineComment}   { /* 忽略, 不做任何操作 */ }
{ParaComment}   { /* 忽略, 不做任何操作 */ }

"int"           { return INT; }
"return"        { return RETURN; }
"const"         { return CONST; }

"<"             { yylval.op_val = Operator::lt; return REL_OP; }
">"             { yylval.op_val = Operator::gt; return REL_OP; }
"<="            { yylval.op_val = Operator::leq; return REL_OP; }
">="            { yylval.op_val = Operator::geq; return REL_OP; }

"=="            { yylval.op_val = Operator::eq; return EQ_OP; }
"!="            { yylval.op_val = Operator::neq; return EQ_OP; }

"&&"            { return LAND; }
"||"            { return LOR; }

{Identifier}    { yylval.str_val = new string(yytext); return IDENT; }

{Decimal}       { yylval.int_val = strtol(yytext, nullptr, 0); return INT_CONST; }
{Octal}         { yylval.int_val = strtol(yytext, nullptr, 0); return INT_CONST; }
{Hexadecimal}   { yylval.int_val = strtol(yytext, nullptr, 0); return INT_CONST; }

.               { return yytext[0]; }

%%
