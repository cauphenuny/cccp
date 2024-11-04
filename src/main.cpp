#include "ast.hpp"
#include "bf.hpp"

#include <cassert>
#include <cstdio>
#include <fstream>
#include <iostream>
#include <memory>
#include <stdexcept>
#include <string>
#include <vector>

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
    cerr << "options: -ast | -ir | -koopa | -riscv | -brain" << endl;
    return 1;
}

int main(int argc, const char* argv[]) {
    vector<std::string> options;
    const char* input = nullptr;
    const char* output = nullptr;
    for (int i = 1; i < argc; i++) {
        if (argv[i][0] == '-') {
            string opt = argv[i] + 1;
            if (opt == "o") {
                if (i + 1 < argc) {
                    output = argv[++i];
                } else {
                    return usage(argv[0]);
                }
            } else
                options.push_back(string(argv[i] + 1));
        } else {
            if (input == nullptr) {
                input = argv[i];
            } else {
                return usage(argv[0]);
            }
        }
    }
    if (input == nullptr || options.empty()) return usage(argv[0]);

    yyin = fopen(input, "r");
    if (!yyin) {
        cerr << "no such file: " << input << endl;
        return 1;
    }

    unique_ptr<BaseAST> ast;
    auto ret = yyparse(ast);
    if (ret) return ret;

    FILE* file;
    if (output) {
        file = fopen(output, "w");
    } else {
        file = stdout;
    }
    IrObject ir;
    try {
        try {
            ir = ast->toIr();
        } catch (const std::logic_error& e) {
            cerr << "[AST error] " << e.what() << endl;
            return 2;
        }
        for (const auto& opt : options) {
            if (opt == "ast") {
                fprintf(file, "%s\n", ast->toString().c_str());
            } else if (opt == "ir") {
                fprintf(file, "%s\n", ir->toString().c_str());
            } else if (opt == "koopa") {
                try {
                    fprintf(file, "%s\n", ir->print().c_str());
                } catch (const std::logic_error& e) {
                    cerr << "[IR error] " << e.what() << endl;
                    return 3;
                }
            } else if (opt == "riscv") {
                try {
                    fprintf(file, "%s\n", ir->printRiscV().c_str());
                } catch (const std::logic_error& e) {
                    cerr << "[ASM error] " << e.what() << endl;
                    return 4;
                }
            } else if (opt == "brain" || opt == "brainz") {
                std::string bf;
                try {
                    bf = ir->printBf();
                } catch (const std::logic_error& e) {
                    cerr << "[BF error] " << e.what() << endl;
                    return 5;
                }
                if (opt.back() == 'z') {
                    fprintf(file, "%s\n", bfCompress(bf).c_str());
                } else {
                    fprintf(file, "%s\n", bf.c_str());
                }
            }
        }
    } catch (const std::runtime_error& e) {
        cerr << "[compiler error] " << e.what() << endl;
        return 6;
    }
    return 0;
}
