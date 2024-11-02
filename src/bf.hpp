#ifndef BF_HPP
#define BF_HPP
#include "ir.hpp"
#include "util.hpp"

#include <map>
#include <string>

const int MAXN = 20000;

struct Tape {
    unsigned current_pos{0};
    bool used[MAXN]{0};
};
struct BfContext {
    Tape tape;
    std::map<std::string, int> symbol_table;
    unsigned ret;
};

inline std::string ValueIR::toBrainfuck(void* context) const {
    std::string str;
    assert(context != nullptr);
    auto& ctx = *(BfContext*)(context);
    switch (type) {
        case Return:
            str += params[0]->toBrainfuck(context);
            if (ctx.tape.current_pos < ctx.ret)
                for (int i = 0; i < ctx.ret - ctx.tape.current_pos; i++) str += ">";
            else if (ctx.tape.current_pos > ctx.ret)
                for (int i = 0; i < ctx.tape.current_pos - ctx.ret; i++) str += "<";
            ctx.tape.current_pos = ctx.ret;
            break;
        case Integer: {
            int new_pos = ctx.tape.current_pos;
            while (ctx.tape.used[new_pos] || ctx.tape.used[new_pos + 1]) new_pos++;
            ctx.tape.used[new_pos] = 1;
            int value = std::stoi(content);
            int factor1, factor2;
            for (int i = 1; i * i <= value; i++) {
                if (value % i == 0) {
                    factor1 = i;
                    factor2 = value / i;
                }
            }
            str = ">";
            for (int i = 0; i < factor1; ++i) str += '+';
            str += "[<";
            for (int i = 0; i < factor2; ++i) str += '+';
            str += ">-]<";
            ctx.ret = new_pos;
            break;
        }
        case Binary: break;
        default: eprintf("not implemented value type %d!", type); break;
    }
    return str;
}

inline std::string BasicBlockIR::toBrainfuck(void*) const {
    std::string str;
    BfContext context;
    for (const auto& inst : insts) {
        str += inst->toBrainfuck(&context);
    }
    return str;
}

inline std::string FunctionIR::toBrainfuck(void*) const {
    std::string blocks_str;
    for (const auto& block : blocks) {
        blocks_str += block->toBrainfuck();
    }
    return blocks_str;
}

inline std::string ProgramIR::toBrainfuck(void*) const {
    std::string str;
    for (const auto& func : funcs) {
        str += func->toBrainfuck();
    }
    return str;
}

#endif