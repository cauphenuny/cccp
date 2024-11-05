#ifndef BF_HPP
#define BF_HPP
#include "ir.hpp"
#include "util.hpp"

#include <cstdarg>
#include <cstdio>
#include <map>
#include <string>

const int MAXN = 20000;

struct BaseIR::BfContext {
    struct Tape {
        unsigned current_pos{0};
        bool used[MAXN]{0};
    };
    Tape tape;
    std::map<std::string, int> symbol_table;
    unsigned ret;
};

using Tape = BaseIR::BfContext::Tape;

inline std::string repeat(std::string str, unsigned times) {
    std::string ret_str;
    for (unsigned i = 0; i < times; i++) ret_str += str;
    return ret_str;
}

inline void bfFree(Tape& tape, unsigned pos, unsigned size = 1) {
    for (unsigned i = 0; i < size; i++)
        assert(tape.used[pos + i] && "free unallocated cell!"), tape.used[pos + i] = false;
}

inline std::string strPrintf(const char* format, ...) {
    char buffer[64];
    va_list args;
    va_start(args, format);
    std::vsnprintf(buffer, sizeof(buffer), format, args);
    va_end(args);
    return std::string(buffer);
}

/// @brief move cell pointer to the given position
inline std::string bfMove(Tape& tape, unsigned pos) {
    if (pos == tape.current_pos) return "";
    const std::string comment = strPrintf("; goto #%d\n", pos);
    std::string str;
    if (tape.current_pos < pos)
        str = repeat(">", pos - tape.current_pos);
    else if (tape.current_pos > pos)
        str = repeat("<", tape.current_pos - pos);
    tape.current_pos = pos;
    return str + "    \t" + comment;
}

/// @brief do {func} for @{pos} times, take the ownership of cell ${pos}
inline std::string bfFor(Tape& tape, unsigned pos, std::function<std::string()> func) {
    assert(tape.used[pos] && "bad access");
    const std::string comment = strPrintf("; do for $%d times\n", pos);
    std::string str;
    str += bfMove(tape, pos);
    str += "[\n";
    str += addIndent(func() + bfMove(tape, pos));
    str += "-]\n";
    bfFree(tape, pos);
    return comment + addIndent(str);
}

/// @brief clear cell ${pos:size}
inline std::string bfClear(Tape& tape, unsigned pos, unsigned size = 1) {
    const std::string comment = strPrintf("; clear(#%d %d)\n", pos, size);
    std::string str;
    for (unsigned i = 0; i < size; i++)
        assert(tape.used[pos + i] && "bad access"), str += bfMove(tape, pos + i) + "[-]\n";
    return comment + addIndent(str);
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
    for (unsigned i = 0; i < size; i++) tape.used[pos + i] = true;
    return bfClear(tape, pos, size);
}

/// @brief move cell from src to dest, take the ownership of ${src}
inline std::string bfMoveCell(Tape& tape, unsigned src, unsigned dest, int sign = 1) {
    assert(tape.used[src] && "bad access");
    assert(tape.used[dest] && "bad access");
    if (src == dest) return "";
    const std::string comment = strPrintf("; move(#%d): #%d\n", src, dest);
    std::string str, ch = sign > 0 ? "+" : "-";
    str += bfFor(tape, src, [&] { return bfMove(tape, dest) + ch + "\n"; });
    return comment + addIndent(str);
}

inline std::string bfCopyCell(Tape& tape, unsigned src, std::vector<unsigned> dest, int sign) {
    std::string comment = strPrintf("; copy(#%d): ", src);
    for (auto d : dest) {
        comment += strPrintf("#%d ", d);
        assert(tape.used[d] && "bad access");
    }
    comment += "\n";
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
    return comment + addIndent(str);
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
    const std::string comment = strPrintf("; add($%d $%d): #%d\n", x, y, ret);
    std::string str;
    str += bfMoveCell(tape, x, ret, 1);
    str += bfMoveCell(tape, y, ret, 1);
    return comment + addIndent(str);
}

inline std::string bfSub(Tape& tape, unsigned x, unsigned y, unsigned ret) {
    assert(tape.used[x] && tape.used[y] && tape.used[ret] && "bad access");
    const std::string comment = strPrintf("; sub($%d $%d): #%d\n", x, y, ret);
    std::string str = bfMoveCell(tape, x, ret, 1);
    str += bfMoveCell(tape, y, ret, -1);
    return comment + addIndent(str);
}

inline std::string bfMul(Tape& tape, unsigned x, unsigned y, unsigned ret) {
    assert(tape.used[x] && tape.used[y] && tape.used[ret] && "bad access");
    const std::string comment = strPrintf("; mul($%d $%d): #%d\n", x, y, ret);
    std::string str;
    str += bfFor(tape, x, [&] { return bfCopyCell(tape, y, ret, 1); });
    bfFree(tape, y);
    return comment + addIndent(str);
}

inline std::string bfDiv(Tape& tape, unsigned x, unsigned y, unsigned ret) {
    assert(tape.used[x] && tape.used[y] && tape.used[ret] && "bad access");
    const std::string comment = strPrintf("; div($%d $%d): #%d\n", x, y, ret);
    const std::string str;
    return comment + addIndent(str);
}

inline std::string bfBool(Tape& tape, unsigned x, unsigned ret) {
    assert(tape.used[x] && tape.used[ret] && "bad access");
    const std::string comment = strPrintf("; bool($%d): #%d\n", x, ret);
    std::string str;
    str += bfMove(tape, x);
    str += "[\n" + bfClear(tape, x) + bfMove(tape, ret) + "+\n" + bfMove(tape, x) + "]\n";
    return comment + addIndent(str);
}

inline std::string bfOr(Tape& tape, unsigned x, unsigned y, unsigned ret) {
    assert(tape.used[x] && tape.used[y] && tape.used[ret] && "bad access");
    const std::string comment = strPrintf("; or($%d $%d): #%d\n", x, y, ret);
    std::string str;
    str += bfMove(tape, x);
    str += "[-\n" + bfClear(tape, y) + bfMove(tape, y) + "+\n" + bfMove(tape, x) + "]\n";
    str += bfMoveCell(tape, y, ret);
    return comment + addIndent(str);
}

inline std::string bfEqual(Tape& tape, unsigned x, unsigned y, unsigned ret) {
    assert(tape.used[x] && tape.used[y] && tape.used[ret] && "bad access");
    const std::string comment = strPrintf("; equal($%d $%d): #%d\n", x, y, ret);
    std::string str;
    str += bfMove(tape, x);
    str += "[-\n" + bfMove(tape, y) + "-\n" + bfMove(tape, x) + "]+\n" + bfMove(tape, y);
    str += "[\n" + bfMove(tape, x) + "-\n" + bfClear(tape, y) + "]\n";
    str += bfMoveCell(tape, x, ret);
    bfFree(tape, y);
    return comment + addIndent(str);
}

inline std::string bfNot(Tape& tape, unsigned x, unsigned ret) {
    assert(tape.used[x] && tape.used[ret] && "bad access");
    const std::string comment = strPrintf("; not($%d): #%d\n", x, ret);
    unsigned tmp;
    std::string str;
    str += bfAlloc(tape, tmp) + bfMove(tape, x) + "-\n[\n" + bfMove(tape, tmp) + "-\n" +
           bfMove(tape, x) + "-]\n";
    str += bfMove(tape, tmp) + "[\n" + bfMove(tape, x) + "+" + bfMove(tape, tmp) + "-]\n";
    str += bfMoveCell(tape, x, ret);
    bfFree(tape, tmp);
    return comment + addIndent(str);
}

inline std::string bfNeq(Tape& tape, unsigned x, unsigned y, unsigned ret) {
    assert(tape.used[x] && tape.used[y] && tape.used[ret] && "bad access");
    const std::string comment = strPrintf("; equal($%d $%d): #%d\n", x, y, ret);
    std::string str;
    unsigned tmp;
    str += bfAlloc(tape, tmp);
    str += bfEqual(tape, x, y, tmp);
    str += bfNot(tape, tmp, ret);
    return comment + addIndent(str);
}

inline std::string bfInteger(Tape& tape, unsigned& pos, int value) {
    std::string comment;
    std::string str;
    if (!value || value == 1 || value == -1) {
        str += bfAlloc(tape, pos);
        comment = strPrintf("; int(%d): #%d\n", value, pos);
        if (value) str += bfMove(tape, pos) + (value > 0 ? "+" : "-");
    } else {
        str += bfAlloc(tape, pos, 2);
        comment = strPrintf("; int(%d): #%d\n", (uint8_t)value, pos);
        const std::string ch = value > 0 ? "+" : "-";
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
                       [&]() { return bfMove(tape, pos) + repeat(ch, factor2) + "\n"; });
    }
    return comment + addIndent(str);
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
    throw runtimeError("unknown operator {}!", toIrOperatorName(op));
}

inline std::string ValueIR::printBf(BfContext* context) const {
    std::string str;
    assert(context != nullptr);
    auto& ctx = *(BfContext*)(context);
    switch (inst) {
        case Inst::Return:
            str += params[0]->printBf(context);
            str += bfMove(ctx.tape, ctx.ret);
            break;
        case Inst::Integer: {
            str = bfInteger(ctx.tape, ctx.ret, std::stoi(content));
            break;
        }
        case Inst::Binary: {
            str += params[0]->printBf(context);
            const unsigned op1 = ctx.ret;
            str += params[1]->printBf(context);
            const unsigned op2 = ctx.ret;
            str += bfAlloc(ctx.tape, ctx.ret);
            str += getBfFunction(toOperator(content))(ctx.tape, op1, op2, ctx.ret);
            break;
        }
        case Inst::Alloc:
            str += bfAlloc(ctx.tape, ctx.ret);
            ctx.symbol_table[content] = ctx.ret;
            break;
        case Inst::Store: {
            str += params[0]->printBf(context);
            const unsigned exp_pos = ctx.ret;
            const unsigned var_pos = ctx.symbol_table[content];
            str += bfClear(ctx.tape, var_pos);
            str += bfMoveCell(ctx.tape, exp_pos, var_pos);
            break;
        }
        case Inst::Load:
            str += bfAlloc(ctx.tape, ctx.ret);
            str += bfCopyCell(ctx.tape, ctx.symbol_table[content], ctx.ret);
            break;
        default: throw runtimeError("not implemented value type {}!", serialize(inst)); break;
    }
    return str;
}

inline std::string MultiValueIR::printBf(BfContext* context) const {
    std::string str;
    for (auto& value : values) {
        str += value->printBf(context);
    }
    return str;
}

inline std::string BasicBlockIR::printBf(BfContext*) const {
    std::string str;
    BfContext context;
    for (const auto& inst : insts) {
        str += inst->printBf(&context);
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

inline std::string FunctionIR::printBf(BfContext*) const {
    std::string blocks_str;
    for (const auto& block : blocks) {
        blocks_str += block->printBf();
    }
    // std::cerr << compress(blocks_str) << std::endl;
    return "; " + name + "()\n" + addIndent(blocks_str);
}

inline std::string ProgramIR::printBf(BfContext*) const {
    std::string str;
    for (const auto& func : funcs) {
        str += func->printBf();
    }
    return str;
}

#endif
