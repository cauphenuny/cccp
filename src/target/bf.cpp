#include "ir/ir.hpp"
#include "util.hpp"

#include <map>
#include <string>

const int MAXN = 65536;

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

static std::string repeat(std::string str, unsigned times) {
    std::string ret_str;
    for (unsigned i = 0; i < times; i++) ret_str += str;
    return ret_str;
}

std::string bfCompress(std::string s) {
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

static void bfFree(Tape& tape, unsigned pos, unsigned size = 1) {
    for (unsigned i = 0; i < size; i++)
        assert(tape.used[pos + i] && "free unallocated cell!"), tape.used[pos + i] = false;
}

/// @brief move cell pointer to the given position
static std::string bfJump(Tape& tape, unsigned pos) {
    if (pos == tape.current_pos) return "";
    const std::string comment = std::format("; goto #{}\n", pos);
    std::string str;
    if (tape.current_pos < pos)
        str = repeat(">", pos - tape.current_pos);
    else if (tape.current_pos > pos)
        str = repeat("<", tape.current_pos - pos);
    tape.current_pos = pos;
    return str + "    \t" + comment;
}

/// @brief do {func} for @{pos} times, take the ownership of cell ${pos}
static std::string bfFor(Tape& tape, unsigned pos, std::function<std::string()> func) {
    assert(tape.used[pos] && "bad access");
    const std::string comment = std::format("; do for ${} times\n", pos);
    std::string str;
    str += bfJump(tape, pos);
    str += "[\n";
    str += addIndent(func() + bfJump(tape, pos));
    str += "-]\n";
    bfFree(tape, pos);
    return comment + addIndent(str);
}

// static std::string bfIf(Tape& tape, unsigned pos, std::function<std::string()> func) {
//     assert(tape.used[pos] && "bad access");
//     const std::string comment = std::format("; if ${}\n", pos);
//     std::string str;
//     str += bfJump(tape, pos);
//     str += "[\n";
//     str += addIndent(func());
//     str += bfJump(tape, pos);
//     str += "]\n";
//     bfFree(tape, pos);
//     return comment + addIndent(str);
// }

/// @brief clear cell ${pos:size}
static std::string bfClear(Tape& tape, unsigned pos, unsigned size = 1) {
    const std::string comment = std::format("; clear(#{} {})\n", pos, size);
    std::string str;
    for (unsigned i = 0; i < size; i++)
        assert(tape.used[pos + i] && "bad access"), str += bfJump(tape, pos + i) + "[-]\n";
    return comment + addIndent(str);
}

/// @brief allocate a segment cell, length: {size}, returned first cell: ${pos}
static std::string bfAlloc(Tape& tape, unsigned& pos, unsigned size = 1) {
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
static std::string bfMove(Tape& tape, unsigned src, unsigned dest, int sign = 1) {
    assert(tape.used[src] && "bad access");
    assert(tape.used[dest] && "bad access");
    if (src == dest) return "";
    const std::string comment = std::format("; move(#{} to #{})\n", src, dest);
    std::string str, ch = sign > 0 ? "+" : "-";
    str += bfFor(tape, src, [&] { return bfJump(tape, dest) + ch + "\n"; });
    return comment + addIndent(str);
}

static std::string bfCopy(Tape& tape, unsigned src, std::vector<unsigned> dest, int sign) {
    std::string comment = std::format("; copy(#{} to ", src);
    for (auto d : dest) {
        comment += std::format("#{} ", d);
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
            s += bfJump(tape, d);
            s += ch + "\n";
        }
        s += bfJump(tape, src);
        return s;
    });
    tape.used[src] = true;
    str += bfMove(tape, tmp, src);
    return comment + addIndent(str);
}

static std::string bfCopy(Tape& tape, unsigned src, unsigned dest, int sign = 1, int len = 1) {
    std::vector<unsigned> dests(len);
    for (int i = 0; i < len; i++) {
        dests[i] = dest + i;
    }
    return bfCopy(tape, src, dests, sign);
}

static std::string bfInc(Tape& tape, int pos, int val) {
    assert(tape.used[pos] && "bad access");
    std::string str, ch, name;
    if (val > 0)
        ch = "+", name = "inc";
    else
        ch = "-", val = -val, name = "dec";
    const std::string comment = std::format("; {}(#{} by {})\n", name, pos, val);
    str += bfJump(tape, pos);
    str += repeat(ch, val);
    return comment + addIndent(str);
}

static std::string bfAdd(Tape& tape, unsigned x, unsigned y, unsigned ret) {
    assert(tape.used[x] && tape.used[y] && tape.used[ret] && "bad access");
    const std::string comment = std::format("; add(${} ${}) to #{}\n", x, y, ret);
    std::string str;
    str += bfMove(tape, x, ret, 1);
    str += bfMove(tape, y, ret, 1);
    return comment + addIndent(str);
}

static std::string bfSub(Tape& tape, unsigned x, unsigned y, unsigned ret) {
    assert(tape.used[x] && tape.used[y] && tape.used[ret] && "bad access");
    const std::string comment = std::format("; sub(${} ${}) to #{}\n", x, y, ret);
    std::string str = bfMove(tape, x, ret, 1);
    str += bfMove(tape, y, ret, -1);
    return comment + addIndent(str);
}

static std::string bfMul(Tape& tape, unsigned x, unsigned y, unsigned ret) {
    assert(tape.used[x] && tape.used[y] && tape.used[ret] && "bad access");
    const std::string comment = std::format("; mul(${} ${}) to #{}\n", x, y, ret);
    std::string str;
    str += bfFor(tape, x, [&] { return bfCopy(tape, y, ret, 1); });
    bfFree(tape, y);
    return comment + addIndent(str);
}

static std::string bfDiv(Tape& tape, unsigned x, unsigned y, unsigned ret) {
    assert(tape.used[x] && tape.used[y] && tape.used[ret] && "bad access");
    const std::string comment = std::format("; div(${} ${}) to #{}\n", x, y, ret);
    debugLog("{}", comment);
    std::string str;
    unsigned tmp;
    auto j = [&tape](unsigned p) { return bfJump(tape, p); };
    auto inc = [&tape](unsigned p) { return bfInc(tape, p, 1); };
    auto dec = [&tape](unsigned p) { return bfInc(tape, p, -1); };
    str += bfAlloc(tape, tmp, 3);
    unsigned tmp0 = tmp + 1, tmp1 = tmp, x0 = tmp + 2;
    /*
    x[
     temp1+[
      y[x-[temp1+x0]temp1-temp0+y-]
      temp0[y+temp0-]q+temp1
     ]
    ]
    x[y[temp0+x+y-]temp0[y+temp0-]q-x0]
    */
    str += j(x) + "[\n" + inc(tmp1) + "[\n";
    str += j(y) + "[\n" + dec(x) + "[\n" + inc(tmp1) + j(x0) + "]\n";
    str += dec(tmp1) + inc(tmp0) + dec(y) + "]\n";
    str += j(tmp0) + "[\n" + inc(y) + dec(tmp0) + "]\n";
    str += inc(ret) + j(tmp1) + "]]\n";
    str += j(x) + "[\n" + j(y) + "[\n" + inc(tmp0) + inc(x) + dec(y) + "]\n";
    str += j(tmp0) + "[\n" + inc(y) + dec(tmp0) + "]\n" + dec(ret) + j(x0) + "]";
    bfFree(tape, tmp, 3);
    bfFree(tape, x), bfFree(tape, y);
    return comment + addIndent(str);
}

// static std::string bfBool(Tape& tape, unsigned x, unsigned ret) {
//     assert(tape.used[x] && tape.used[ret] && "bad access");
//     const std::string comment = std::format("; bool(${}) to #{}\n", x, ret);
//     std::string str;
//     str += bfJump(tape, x);
//     str += "[\n" + bfClear(tape, x) + bfJump(tape, ret) + "+\n" + bfJump(tape, x) + "]\n";
//     return comment + addIndent(str);
// }

static std::string bfOr(Tape& tape, unsigned x, unsigned y, unsigned ret) {
    assert(tape.used[x] && tape.used[y] && tape.used[ret] && "bad access");
    const std::string comment = std::format("; or(${} ${}) to #{}\n", x, y, ret);
    std::string str;
    str += bfJump(tape, x);
    str += "[-\n" + bfClear(tape, y) + bfJump(tape, y) + "+\n" + bfJump(tape, x) + "]\n";
    str += bfMove(tape, y, ret);
    return comment + addIndent(str);
}

static std::string bfEqual(Tape& tape, unsigned x, unsigned y, unsigned ret) {
    assert(tape.used[x] && tape.used[y] && tape.used[ret] && "bad access");
    const std::string comment = std::format("; eq(${} ${}) to #{}\n", x, y, ret);
    std::string str;
    str += bfJump(tape, x);
    str += "[-\n" + bfJump(tape, y) + "-\n" + bfJump(tape, x) + "]+\n" + bfJump(tape, y);
    str += "[\n" + bfJump(tape, x) + "-\n" + bfClear(tape, y) + "]\n";
    str += bfMove(tape, x, ret);
    bfFree(tape, y);
    return comment + addIndent(str);
}

static std::string bfNot(Tape& tape, unsigned x, unsigned ret) {
    assert(tape.used[x] && tape.used[ret] && "bad access");
    const std::string comment = std::format("; not(${}) to #{}\n", x, ret);
    unsigned tmp;
    std::string str;
    str += bfAlloc(tape, tmp) + bfJump(tape, x) + "-\n[\n" + bfJump(tape, tmp) + "-\n" +
           bfJump(tape, x) + "-]\n";
    str += bfJump(tape, tmp) + "[\n" + bfJump(tape, x) + "+" + bfJump(tape, tmp) + "-]\n";
    str += bfMove(tape, x, ret);
    bfFree(tape, tmp);
    return comment + addIndent(str);
}

static std::string bfNeq(Tape& tape, unsigned x, unsigned y, unsigned ret) {
    assert(tape.used[x] && tape.used[y] && tape.used[ret] && "bad access");
    const std::string comment = std::format("; neq(${} ${}) to #{}\n", x, y, ret);
    std::string str;
    unsigned tmp;
    str += bfAlloc(tape, tmp);
    str += bfEqual(tape, x, y, tmp);
    str += bfNot(tape, tmp, ret);
    return comment + addIndent(str);
}

static std::string bfInteger(Tape& tape, unsigned& pos, int value) {
    std::string comment;
    std::string str;
    if (!value || value == 1 || value == -1) {
        str += bfAlloc(tape, pos);
        comment = std::format("; int({}) to #{}\n", value, pos);
        if (value) str += bfJump(tape, pos) + (value > 0 ? "+" : "-");
    } else {
        str += bfAlloc(tape, pos, 2);
        comment = std::format("; int({}) to #{}\n", (uint8_t)value, pos);
        const std::string ch = value > 0 ? "+" : "-";
        value = value > 0 ? value : -value;
        int factor1, factor2;
        for (int i = 1; i * i <= value; i++) {
            if (value % i == 0) {
                factor1 = i;
                factor2 = value / i;
            }
        }
        str += bfJump(tape, pos + 1) + repeat(ch, factor1) + "\n";
        str +=
            bfFor(tape, pos + 1, [&]() { return bfJump(tape, pos) + repeat(ch, factor2) + "\n"; });
    }
    return comment + addIndent(str);
}

using BfBinaryFunction = std::function<std::string(Tape&, unsigned, unsigned, unsigned)>;

static const std::map<Operator, BfBinaryFunction> bf_function_map = {
    {Operator::add, bfAdd},  {Operator::sub, bfSub}, {Operator::mul, bfMul}, {Operator::bor, bfOr},
    {Operator::eq, bfEqual}, {Operator::neq, bfNeq}, {Operator::div, bfDiv}};

static BfBinaryFunction getBfFunction(Operator oper) {
    for (const auto& [op, func] : bf_function_map) {
        if (oper == op) {
            return func;
        }
    }
    std::vector<Operator> ops;
    for (const auto& [op, _] : bf_function_map) {
        ops.push_back(op);
    }
    throw runtimeError("unimplemented operator {}! (now supports {})", toIrOperatorName(oper), ops);
}

std::string ValueIR::printBf(bool compress, std::shared_ptr<BfContext> context) const {
    std::string str;
    assert(context != nullptr);
    auto& ctx = *context;
    switch (inst) {
        case Inst::Return:
            str += params[0]->printBf(compress, context);
            str += bfJump(ctx.tape, ctx.ret);
            break;
        case Inst::Integer:
            str += bfInteger(ctx.tape, ctx.ret, std::stoi(content));
            break;
        case Inst::Binary: {
            str += params[0]->printBf(compress, context);
            const unsigned op1 = ctx.ret;
            str += params[1]->printBf(compress, context);
            const unsigned op2 = ctx.ret;
            str += bfAlloc(ctx.tape, ctx.ret);
            str += getBfFunction(toOperator(content))(ctx.tape, op1, op2, ctx.ret);
            break;
        }
        case Inst::Alloc:
            str += bfAlloc(ctx.tape, ctx.ret);
            str += std::format("; alloc #{} for @{}\n", ctx.ret, content);
            ctx.symbol_table[content] = ctx.ret;
            break;
        case Inst::Store: {
            str += params[0]->printBf(compress, context);
            const unsigned exp_pos = ctx.ret;
            const unsigned var_pos = ctx.symbol_table[content];
            str += bfClear(ctx.tape, var_pos);
            str += bfMove(ctx.tape, exp_pos, var_pos);
            break;
        }
        case Inst::Load:
            str += bfAlloc(ctx.tape, ctx.ret);
            str += std::format("; load @{} to #{}\n", content, ctx.ret);
            str += bfCopy(ctx.tape, ctx.symbol_table[content], ctx.ret);
            break;
        default: throw runtimeError("not implemented value type {}!", inst); break;
    }
    return str;
}

std::string MultiValueIR::printBf(bool compress, std::shared_ptr<BfContext> context) const {
    std::string str;
    for (auto& value : values) {
        str += value->printBf(compress, context);
    }
    return str;
}

std::string FunctionIR::printBf(bool compress, std::shared_ptr<BfContext> context) const {
    std::string blocks_str;
    // for (const auto& block : blocks) {
    blocks_str += blocks->printBf(compress, context);
    // }
    // std::cerr << compress(blocks_str) << std::endl;
    return "; " + name + "()\n" + addIndent(blocks_str);
}

std::string ProgramIR::printBf(bool compress, std::shared_ptr<BfContext> context) const {
    std::string str;
    for (const auto& func : funcs) {
        str += func->printBf(compress, context);
    }
    if (!compress)
        return str;
    else
        return bfCompress(str);
}
