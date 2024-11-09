#include "ast/ast.hpp"
#include "ir/ir.hpp"
#include "util.hpp"

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

int usage(string_view name) {
    cerr << "usage: " << name << " -option1 [-option2, ...] [input_file] [-o output_file]" << endl;
    cerr << "options: -ast | -ir | -koopa | -riscv | -brain" << endl;
    return 1;
}

int main(int argc, const char* argv[]) {
    vector<string> options;
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
                options.emplace_back(argv[i] + 1);
        } else {
            if (input == nullptr) {
                input = argv[i];
            } else {
                return usage(argv[0]);
            }
        }
    }
    if (options.empty()) return usage(argv[0]);
    if (input == nullptr)
        yyin = stdin;
    else
        yyin = fopen(input, "r");

    if (!yyin) {
        cerr << "no such file: " << input << endl;
        return 1;
    }
    FILE* file;
    if (output) {
        file = fopen(output, "w");
    } else {
        file = stdout;
    }

    try {
        unique_ptr<BaseAST> ast;
        if (const auto ret = yyparse(ast)) return ret;
        debugLog("parsed");
        IrObject ir;
        auto generate_ir = [&ir, &ast] {
            if (ir) return;
            try {
                ir = ast->toIR();
            } catch (const logic_error& e) {
                cerr << RED "[AST error]\n" RESET << e.what() << endl;
                exit(2);
            }
        };
        for (const auto& opt : options) {
            if (opt == "ast") {
                fprintf(file, "%s\n", ast->toString().c_str());
            } else if (opt == "ir") {
                generate_ir();
                fprintf(file, "%s\n", ir->toString().c_str());
            } else if (opt == "koopa") {
                generate_ir();
                try {
                    fprintf(file, "%s\n", ir->print().c_str());
                } catch (const logic_error& e) {
                    cerr << RED "[IR error]\n" RESET << e.what() << endl;
                    return 3;
                }
            } else if (opt == "riscv") {
                generate_ir();
                try {
                    fprintf(file, "%s\n", ir->printRiscV().c_str());
                } catch (const logic_error& e) {
                    cerr << RED "[ASM error]\n" RESET << e.what() << endl;
                    return 4;
                }
            } else if (opt == "brain" || opt == "brainz") {
                generate_ir();
                try {
                    string bf;
                    if (opt.back() == 'z') {
                        bf = ir->printBf(true);
                    } else {
                        bf = ir->printBf();
                    }
                    fprintf(file, "%s\n", bf.c_str());
                } catch (const logic_error& e) {
                    cerr << RED "[BF error]\n" RESET << e.what() << endl;
                    return 5;
                }
            }
        }
    } catch (const runtime_error& e) {
        cerr << RED "[compiler error]\n" RESET << e.what() << endl;
        return 6;
    }
    return 0;
}
