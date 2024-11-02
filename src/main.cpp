#include "ast.hpp"
#include "bf.hpp"

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

int usage(std::string name) {
    cerr << "usage: " + name + " -option1 [-option2, ...] input [-o output]" << endl;
    cerr << "options: -ast | -koopa | -riscv | -brain" << endl;
    return 1;
}

int main(int argc, const char* argv[]) {
    map<std::string, bool> options;
    const char* input = nullptr;
    const char* output = nullptr;
    for (int i = 1; i < argc; i++) {
        if (argv[i][0] == '-') {
            options[string(argv[i] + 1)] = 1;
        } else {
            if (input == nullptr) {
                input = argv[i];
            } else if (output == nullptr) {
                output = argv[i];
            } else {
                return usage(argv[0]);
            }
        }
    }
    if (input == nullptr) return usage(argv[0]);

    yyin = fopen(input, "r");
    assert(yyin);

    unique_ptr<BaseAST> ast;
    try {
        auto ret = yyparse(ast);
        assert(!ret);
    } catch (std::runtime_error e) {
        cerr << "parse error: " << e.what() << endl;
        return 1;
    }

    IrObject ir;
    try {
        ir = ast->toIr();
    } catch (std::runtime_error e) {
        cerr << "convert error: " << e.what() << endl;
        return 2;
    }

    FILE* file;
    if (output) {
        file = fopen(output, "w");
    } else {
        file = stdout;
    }
    if (options["ast"]) {
        fprintf(file, "%s\n", ast->toString().c_str());
    }
    if (options["koopa"]) {
        fprintf(file, "%s\n", ir->toString().c_str());
    }
    if (options["riscv"]) {
        fprintf(file, "%s\n", ir->toAssembly().c_str());
    }
    if (options["brain"]) {
        if (options["z"]) {
            fprintf(file, "%s\n", bfCompress(ir->toBrainfuck()).c_str());
        } else {
            fprintf(file, "%s\n", ir->toBrainfuck().c_str());
        }
    }
    return 0;
}
