#include "ast.hpp"

#include <cassert>
#include <cstdio>
#include <fstream>
#include <iostream>
#include <memory>
#include <string>

using namespace std;

extern FILE* yyin;
extern int yyparse(unique_ptr<BaseAST>& ast);

int main(int argc, const char* argv[])
{
    assert(argc == 5);
    const string mode = argv[1];
    const char* input = argv[2];
    const char* output = argv[4];
    // const string mode = "-debug";
    // const char* input = "/root/compiler/test/lv3-3.c";
    // const char* output = "/root/compiler/test/lv3-3.o";

    yyin = fopen(input, "r");
    assert(yyin);

    unique_ptr<BaseAST> ast;
    auto ret = yyparse(ast);
    assert(!ret);

    // cerr << "parsed\n";
    std::fstream fs(output, std::ios::out);
    auto& out = fs;
    auto& debug = cerr;
    auto ir = ast->toIr();
    if (mode == "-riscv")
        out << ir->toAssembly() << endl;
    else if (mode == "-koopa") {
        // debug << ast->toString() << endl;
        out << ir->toString() << endl;
        // debug << ir->toAssembly() << endl;
    } else if (mode == "-debug") {
        debug << ast->toString() << endl;
        debug << ir->toString() << endl;
        debug << ir->toAssembly() << endl;
    } else
        cerr << "usage: " + string(argv[0]) + " [-riscv|-koopa] input -o output"
             << endl;
    return 0;
}
