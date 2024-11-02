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

inline void bfFree(Tape& tape, unsigned pos, unsigned size = 1) {
    for (int i = 0; i < size; i++)
        assert(tape.used[pos + i] && "free unallocated cell!"), tape.used[pos + i] = false;
}

/// @brief move cell pointer to the given position
inline std::string bfMove(Tape& tape, unsigned pos) {
    if (pos == tape.current_pos) return "";
    char comment[64];
    std::snprintf(comment, sizeof(comment), "; goto $%d\n", pos);
    std::string str;
    if (tape.current_pos < pos)
        str = repeat(">", pos - tape.current_pos);
    else if (tape.current_pos > pos)
        str = repeat("<", tape.current_pos - pos);
    tape.current_pos = pos;
    return str + "\t" + comment;
}

/// @brief do {func} for @{pos} times, take the ownership of cell ${pos}
inline std::string bfFor(Tape& tape, unsigned pos, std::function<std::string()> func) {
    assert(tape.used[pos] && "bad access");
    char comment[64];
    std::snprintf(comment, sizeof(comment), "; for($%d)\n", pos);
    std::string str;
    str += bfMove(tape, pos);
    str += "[\n";
    str += func();
    str += bfMove(tape, pos);
    str += "-]\n";
    bfFree(tape, pos);
    return addIndent(comment + str);
}

/// @brief clear cell ${pos:size}
inline std::string bfClear(Tape& tape, unsigned pos, unsigned size = 1) {
    char comment[64];
    std::snprintf(comment, sizeof(comment), "; clear($%d %d)\n", pos, size);
    std::string str;
    for (int i = 0; i < size; i++)
        assert(tape.used[pos + i] && "bad access"), str += bfMove(tape, pos + i) + "[-]\n";
    return addIndent(comment + str);
}

/// @brief allocate a segment cell, length: {size}, returned first cell: ${pos}
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

/// @brief move cell from src to dest, take the ownership of ${src}
inline std::string bfMoveCell(Tape& tape, unsigned src, unsigned dest, int sign = 1) {
    assert(tape.used[src] && "bad access");
    assert(tape.used[dest] && "bad access");
    if (src == dest) return "";
    char comment[64];
    std::snprintf(comment, sizeof(comment), "; move($%d): $%d\n", src, dest);
    std::string str, ch = sign > 0 ? "+" : "-";
    str = bfMove(tape, src);
    str += "[\n";
    str += bfMove(tape, dest);
    str += ch + "\n";
    str += bfMove(tape, src);
    str += "-]\n";
    bfFree(tape, src);
    return addIndent(comment + str);
}

inline std::string bfCopyCell(Tape& tape, unsigned src, std::vector<unsigned> dest, int sign) {
    char comment[64];
    std::snprintf(comment, sizeof(comment), "; copy($%d): ", src);
    for (auto d : dest) {
        std::snprintf(comment + strlen(comment), sizeof(comment) - strlen(comment), "$%d ", d);
        assert(tape.used[d] && "bad access");
    }
    comment[strlen(comment)] = '\n';
    std::string str, ch = sign == 1 ? "+" : "-";
    unsigned tmp;
    str += bfAlloc(tape, tmp);
    dest.push_back(tmp);
    str += bfFor(tape, src, [&tape, &dest, ch, src] {
        std::string s;
        for (auto d : dest) {
            s += bfMove(tape, d);
            s += ch + "\n";
        }
        s += bfMove(tape, src);
        return s;
    });
    tape.used[src] = true;
    str += bfMoveCell(tape, tmp, src);
    return addIndent(comment + str);
}

inline std::string bfCopyCell(Tape& tape, unsigned src, unsigned dest, int sign = 1, int len = 1) {
    std::vector<unsigned> dests(len);
    for (int i = 0; i < len; i++) {
        dests[i] = dest + i;
    }
    return bfCopyCell(tape, src, dests, sign);
}

inline std::string bfAdd(Tape& tape, unsigned x, unsigned y, unsigned ret) {
    assert(tape.used[x] && tape.used[y] && tape.used[ret] && "bad access");
    char comment[64];
    std::snprintf(comment, sizeof(comment), "; add($%d $%d): $%d\n", x, y, ret);
    std::string str;
    str += bfMoveCell(tape, x, ret, 1);
    str += bfMoveCell(tape, y, ret, 1);
    return addIndent(comment + str);
}

inline std::string bfSub(Tape& tape, unsigned x, unsigned y, unsigned ret) {
    assert(tape.used[x] && tape.used[y] && tape.used[ret] && "bad access");
    char comment[64];
    std::snprintf(comment, sizeof(comment), "; sub($%d $%d): $%d\n", x, y, ret);
    std::string str = bfMoveCell(tape, x, ret, 1);
    str += bfMoveCell(tape, y, ret, -1);
    return addIndent(comment + str);
}

inline std::string bfMul(Tape& tape, unsigned x, unsigned y, unsigned ret) {
    assert(tape.used[x] && tape.used[y] && tape.used[ret] && "bad access");
    char comment[64];
    std::snprintf(comment, sizeof(comment), "; mul($%d $%d): $%d\n", x, y, ret);
    std::string str;
    str += bfFor(tape, x, [&] { return bfCopyCell(tape, y, ret, 1); });
    bfFree(tape, y);
    return addIndent(comment + str);
}

inline std::string bfDiv(Tape& tape, unsigned x, unsigned y, unsigned ret) {
    assert(tape.used[x] && tape.used[y] && tape.used[ret] && "bad access");
    char comment[64];
    std::snprintf(comment, sizeof(comment), "; div($%d $%d): $%d\n", x, y, ret);
    std::string str;
    return addIndent(comment + str);
}

inline std::string bfBool(Tape& tape, unsigned x, unsigned ret) {
    assert(tape.used[x] && tape.used[ret] && "bad access");
    char comment[64];
    std::snprintf(comment, sizeof(comment), "; bool($%d): $%d\n", x, ret);
    std::string str;
    str += bfMove(tape, x);
    str += "[\n" + bfClear(tape, x) + bfMove(tape, ret) + "+" + bfMove(tape, x) + "\n]\n";
    return addIndent(comment + str);
}

inline std::string bfOr(Tape& tape, unsigned x, unsigned y, unsigned ret) {
    assert(tape.used[x] && tape.used[y] && tape.used[ret] && "bad access");
    char comment[64];
    std::snprintf(comment, sizeof(comment), "; or($%d $%d): $%d\n", x, y, ret);
    std::string str;
    str += bfMove(tape, x);
    str += "[-\n" + bfClear(tape, y) + bfMove(tape, y) + "+" + bfMove(tape, x) + "]\n";
    str += bfMoveCell(tape, y, ret);
    return addIndent(comment + str);
}

inline std::string bfEqual(Tape& tape, unsigned x, unsigned y, unsigned ret) {
    assert(tape.used[x] && tape.used[y] && tape.used[ret] && "bad access");
    char comment[64];
    std::snprintf(comment, sizeof(comment), "; equal($%d $%d): $%d\n", x, y, ret);
    std::string str;
    str += bfMove(tape, x);
    str += "[-\n" + bfMove(tape, y) + "-\n" + bfMove(tape, x) + "]+\n" + bfMove(tape, y);
    str += "[\n" + bfMove(tape, x) + "-\n" + bfClear(tape, y) + "]\n";
    str += bfMoveCell(tape, x, ret);
    bfFree(tape, y);
    return addIndent(comment + str);
}

inline std::string bfNot(Tape& tape, unsigned x, unsigned ret) {
    assert(tape.used[x] && tape.used[ret] && "bad access");
    char comment[64];
    std::snprintf(comment, sizeof(comment), "; not($%d): $%d\n", x, ret);
    unsigned tmp;
    std::string str;
    str += bfAlloc(tape, tmp) + bfMove(tape, x) + "-\n[\n" + bfMove(tape, tmp) + "-\n" +
           bfMove(tape, x) + "-]";
    str += bfMove(tape, tmp) + "[" + bfMove(tape, x) + "+" + bfMove(tape, tmp) + "-]";
    str += bfMoveCell(tape, x, ret);
    bfFree(tape, tmp);
    return addIndent(comment + str);
}

inline std::string bfNeq(Tape& tape, unsigned x, unsigned y, unsigned ret) {
    assert(tape.used[x] && tape.used[y] && tape.used[ret] && "bad access");
    char comment[64];
    std::snprintf(comment, sizeof(comment), "; equal($%d $%d): $%d\n", x, y, ret);
    std::string str;
    unsigned tmp;
    str += bfAlloc(tape, tmp);
    str += bfEqual(tape, x, y, tmp);
    str += bfNot(tape, tmp, ret);
    return addIndent(comment + str);
}

inline std::string bfInteger(Tape& tape, unsigned& pos, int value) {
    char comment[64];
    std::string str;
    if (!value || value == 1 || value == -1) {
        str += bfAlloc(tape, pos);
        std::snprintf(comment, sizeof(comment), "; int(%d): $%d\n", value, pos);
        if (value) str += bfMove(tape, pos) + (value > 0 ? "+" : "-");
    } else {
        str += bfAlloc(tape, pos, 2);
        std::snprintf(comment, sizeof(comment), "; int(%d): $%d\n", (uint8_t)value, pos);
        std::string ch = value > 0 ? "+" : "-";
        value = value > 0 ? value : -value;
        int factor1, factor2;
        for (int i = 1; i * i <= value; i++) {
            if (value % i == 0) {
                factor1 = i;
                factor2 = value / i;
            }
        }
        str += bfMove(tape, pos + 1) + repeat(ch, factor1) + "\n";
        str += bfFor(tape, pos + 1,
                       [&]() { return addIndent(bfMove(tape, pos) + repeat(ch, factor2)); });
    }
    return addIndent(comment + str);
}

using BfBinaryFunction = std::function<std::string(Tape&, unsigned, unsigned, unsigned)>;

inline const std::map<Operator, BfBinaryFunction> bf_function_map = {
    {Operator::add, bfAdd}, {Operator::sub, bfSub},  {Operator::mul, bfMul},
    {Operator::bor, bfOr},  {Operator::eq, bfEqual}, {Operator::neq, bfNeq}};

inline BfBinaryFunction getBfFunction(Operator op) {
    for (const auto& pair : bf_function_map) {
        if (pair.first == op) {
            return pair.second;
        }
    }
    eprintf("unknown operator %s!", toIrOperatorName(op));
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
        case Alloc:
            str += bfAlloc(ctx.tape, ctx.ret);
            ctx.symbol_table[content] = ctx.ret;
            break;
        case Store: {
            str += params[0]->toBrainfuck(context);
            unsigned exp_pos = ctx.ret;
            unsigned var_pos = ctx.symbol_table[content];
            str += bfClear(ctx.tape, var_pos);
            str += bfMoveCell(ctx.tape, exp_pos, var_pos);
            break;
        }
        case Variable:
            str += bfAlloc(ctx.tape, ctx.ret);
            str += bfCopyCell(ctx.tape, ctx.symbol_table[content], ctx.ret);
            break;
        case Type: break;
        default: eprintf("not implemented value type %d!", type); break;
    }
    return str;
}

inline std::string MultiValueIR::toBrainfuck(void* context) const {
    std::string str;
    for (auto& value : values) {
        str += value->toBrainfuck(context);
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

inline std::string bfCompress(std::string s) {
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
    std::string blocks_str = "; " + name + "()\n";
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