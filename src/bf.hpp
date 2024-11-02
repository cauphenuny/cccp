#ifndef BF_HPP
#define BF_HPP
#include "ir.hpp"
#include "util.hpp"

#include <cstdio>
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

inline std::string repeat(std::string str, unsigned times) {
    std::string ret_str;
    for (int i = 0; i < times; i++) ret_str += str;
    return ret_str;
}

/// @brief move cell pointer to the given position
inline std::string bfMove(Tape& tape, unsigned pos) {
    if (pos == tape.current_pos) return "";
    char comment[64];
    std::snprintf(comment, sizeof(comment), "move from $%d to $%d\n", tape.current_pos, pos);
    std::string str;
    if (tape.current_pos < pos)
        str = repeat(">", pos - tape.current_pos);
    else if (tape.current_pos > pos)
        str = repeat("<", tape.current_pos - pos);
    tape.current_pos = pos;
    return addIndent(comment + str);
}

/// @brief move cell from src to dest
inline std::string bfMoveCell(Tape& tape, unsigned src, unsigned dest) {
    char comment[64];
    std::snprintf(comment, sizeof(comment), "move($%d): $%d\n", src, dest);
    std::string str;
    str = bfMove(tape, src);
    str += "[\n";
    str += bfMove(tape, dest);
    str += "+\n";
    str += bfMove(tape, src);
    str += "-]\n";
    return addIndent(comment + str);
}

inline std::string bfClear(Tape& tape, unsigned pos, unsigned size = 1) {
    char comment[64];
    std::snprintf(comment, sizeof(comment), "clear($%d; %d)\n", pos, size);
    std::string str;
    for (int i = 0; i < size; i++) str += bfMove(tape, pos + i) + "[-]\n";
    return addIndent(comment + str);
}

inline std::string bfAlloc(Tape& tape, unsigned& pos, unsigned size = 1) {
    pos = tape.current_pos;
    while (1) {
        if (std::all_of(tape.used + pos, tape.used + pos + size, [](bool used) { return !used; })) {
            break;
        }
        pos++;
    }
    for (int i = 0; i < size; i++) tape.used[pos + i] = true;
    return bfClear(tape, pos, size);
}

inline void bfFree(Tape& tape, unsigned pos, unsigned size = 1) {
    for (int i = 0; i < size; i++) tape.used[pos + i] = false;
}

inline std::string bfCopyCell(Tape& tape, unsigned src, std::vector<unsigned> dest, int sign) {
    char comment[64];
    std::snprintf(comment, sizeof(comment), "copy($%d): ", src);
    for (auto d : dest) {
        std::snprintf(comment + strlen(comment), sizeof(comment) - strlen(comment), "$%d ", d);
    }
    comment[strlen(comment)] = '\n';
    std::string str;
    unsigned tmp;
    str += bfAlloc(tape, tmp);
    dest.push_back(tmp);
    std::string ch = sign == 1 ? "+" : "-";
    str += bfMove(tape, src);
    str += "[\n";
    for (auto d : dest) {
        str += bfMove(tape, d);
        str += ch + "\n";
    }
    str += bfMove(tape, src);
    str += "-]\n";
    str += bfMoveCell(tape, tmp, src);
    return addIndent(comment + str);
}

/// @brief copy cell from src to dest
inline std::string bfCopyCell(Tape& tape, unsigned src, unsigned dest, int sign = 1, int len = 1) {
    std::vector<unsigned> dests(len);
    for (int i = 0; i < len; i++) {
        dests[i] = dest + i;
    }
    return bfCopyCell(tape, src, dests, sign);
}

inline std::string bfAdd(Tape& tape, unsigned param1, unsigned param2, unsigned ret) {
    char comment[64];
    std::snprintf(comment, sizeof(comment), "add($%d; $%d): $%d\n", param1, param2, ret);
    std::string str;
    str += bfCopyCell(tape, param1, ret, 1);
    str += bfCopyCell(tape, param2, ret, 1);
    return addIndent(comment + str);
}

inline std::string bfSub(Tape& tape, unsigned param1, unsigned param2, unsigned ret) {
    char comment[64];
    std::snprintf(comment, sizeof(comment), "sub($%d; $%d): $%d\n", param1, param2, ret);
    std::string str = bfCopyCell(tape, param1, ret, 1);
    str += bfCopyCell(tape, param2, ret, -1);
    return addIndent(comment + str);
}

inline std::string bfInteger(Tape& tape, unsigned& pos, int value) {
    char comment[64];
    std::string str;
    if (!value || value == 1 || value == -1) {
        str += bfAlloc(tape, pos);
        std::snprintf(comment, sizeof(comment), "int(%d): $%d\n", value, pos);
        if (value) str += bfMove(tape, pos) + (value > 0 ? "+" : "-");
    } else {
        str += bfAlloc(tape, pos, 2);
        std::snprintf(comment, sizeof(comment), "int(%d): $%d\n", (uint8_t)value, pos);
        std::string ch = value > 0 ? "+" : "-";
        value = value > 0 ? value : -value;
        int factor1, factor2;
        for (int i = 1; i * i <= value; i++) {
            if (value % i == 0) {
                factor1 = i;
                factor2 = value / i;
            }
        }
        str += bfMove(tape, pos);
        str += ">" + repeat(ch, factor1);
        str += "[<" + repeat(ch, factor2);
        str += ">-]<";
        bfFree(tape, pos + 1);
    }
    return addIndent(comment + str);
}

using BfBinaryFunction = std::function<std::string(Tape&, unsigned, unsigned, unsigned)>;

inline const std::map<Operator, BfBinaryFunction> bf_function_map = {
    {Operator::add, bfAdd},
    {Operator::sub, bfSub},
};

inline BfBinaryFunction getBfFunction(Operator op) {
    for (const auto& pair : bf_function_map) {
        if (pair.first == op) {
            return pair.second;
        }
    }
    eprintf("unknown operator %d!", op);
    return nullptr;
}

inline std::string ValueIR::toBrainfuck(void* context) const {
    std::string str;
    assert(context != nullptr);
    auto& ctx = *(BfContext*)(context);
    switch (type) {
        case Return:
            str += params[0]->toBrainfuck(context);
            str += bfMove(ctx.tape, ctx.ret);
            break;
        case Integer: {
            str = bfInteger(ctx.tape, ctx.ret, std::stoi(content));
            break;
        }
        case Binary: {
            str += params[0]->toBrainfuck(context);
            unsigned op1 = ctx.ret;
            str += params[1]->toBrainfuck(context);
            unsigned op2 = ctx.ret;
            str += bfAlloc(ctx.tape, ctx.ret);
            str += getBfFunction(toOperator(content))(ctx.tape, op1, op2, ctx.ret);
            break;
        }
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

inline std::string compress(std::string s) {
    std::string ret;
    for (auto i : s) {
        switch (i) {
            case '[':
            case ']':
            case '-':
            case '+':
            case '>':
            case '<':
            case ',':
            case '.': ret += i;
        }
    }
    return ret;
}

inline std::string FunctionIR::toBrainfuck(void*) const {
    std::string blocks_str = name + "():\n";
    for (const auto& block : blocks) {
        blocks_str += block->toBrainfuck();
    }
    // std::cerr << compress(blocks_str) << std::endl;
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