#include "ast.hpp"

#include <cassert>
#include <cstdio>
#include <fstream>
#include <iostream>
#include <memory>
#include <stdexcept>
#include <string>

using namespace std;

extern FILE* yyin;
extern int yyparse(unique_ptr<BaseAST>& ast);

void cat(const char* file) {
    ifstream fs(file);
    char str[1024];
    while (fs.getline(str, 1024)) {
        cerr << str << endl;
    }
}

int main(int argc, const char* argv[])
{
    assert(argc == 5);
    const string mode = argv[1];
    const char* input = argv[2];
    const char* output = argv[4];

    // const string mode = "-debug3";
    // const char* input = "test/lv4-2.c";
    // const char* output = "test/lv4-2.o";

    cat(input);

    yyin = fopen(input, "r");
    assert(yyin);
    cerr << "parsing\n";

    unique_ptr<BaseAST> ast;
    try {
        auto ret = yyparse(ast);
        assert(!ret);
    } catch (std::runtime_error e) {
        cerr << "parse error: " << e.what() << endl;
        return 1;
    }
    cerr << "parsed\n";

    IrObject ir;
    try {
        ir = ast->toIr();
    } catch (std::runtime_error e) {
        cerr << "convert error: " << e.what() << endl;
        return 2;
    }

    std::fstream fs(output, std::ios::out);
    auto& out = fs;
    auto& debug = cerr;

    if (mode == "-koopa") {
        out << ir->toString() << endl;
    } else if (mode == "-riscv") {
        out << ir->toAssembly() << endl;
    } else if (mode == "-brfk") {
        out << ir->toBrainfuck() << endl;
    } else if (mode == "-debug1") {
        debug << ast->toString() << endl;
    } else if (mode == "-debug2") {
        debug << ast->toString() << endl;
        debug << ir->toString() << endl;
    } else if (mode == "-debug3") {
        debug << ast->toString() << endl;
        debug << ir->toString() << endl;
        debug << ir->toAssembly() << endl;
        // debug << ir->toBrainfuck() << endl;
    } else {
        cerr << "usage: " + string(argv[0]) + " [-riscv|-koopa|-brfk] input -o output"
             << endl;
    }
    return 0;
}
