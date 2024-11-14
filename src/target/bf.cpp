#include "ir/ir.h"
#include "util.hpp"

#include <map>
#include <string>
#include <vector>

const int MAXN = 65536;

struct BaseIR::BfContext {
    struct Tape {
        unsigned current_pos{0};
        bool cur_used[MAXN]{0};
        bool non_zero[MAXN]{0};
    };
    Tape tape;
    std::map<std::string, unsigned> symbol_table;
    unsigned pc, global_ret;
    std::map<std::string, unsigned> labels;
    int getBlockPC(const std::string& block_name) {
        if (!labels.contains(block_name)) {
            int pc = labels.size() + 1;  // reserve pc=0 for skip
            debugLog("alloc pc @{} for block `{}`", pc, block_name.length() ? block_name : "entry");
            labels[block_name] = pc;
        }
        return labels[block_name];
    }
    std::function<std::string()> label_end = []() { return ""; };
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
    for (unsigned i = 0; i < size; i++) {
        // assert(tape.cur_used[pos + i] && "double free!");
        if (!tape.cur_used[pos + i]) debugLog("warning: double freed cell #{}", pos + i);
        tape.cur_used[pos + i] = false;
    }
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
    return str + "  \t" + comment;
}

/// @brief do {func} for @{pos} times, take the ownership of cell ${pos}
static std::string bfFor(Tape& tape, unsigned pos, std::function<std::string()> func,
                         bool free_after_move = true) {
    assert(tape.cur_used[pos] && "bad access");
    const std::string comment = std::format("; do for ${} times\n", pos);
    std::string str;
    str += bfJump(tape, pos);
    str += "[\n";
    str += addIndent(func());
    str += bfJump(tape, pos) + "-]\n";
    if (free_after_move) bfFree(tape, pos);
    // tape.non_zero[pos] = false;
    return comment + addIndent(str);
}

/// @brief clear cell ${pos:size}
static std::string bfClear(Tape& tape, unsigned pos, unsigned size = 1) {
    const std::string comment = std::format("; clear(#{} {})\n", pos, size);
    std::string str;
    for (unsigned i = 0; i < size; i++) {
        if (!tape.non_zero[pos + i]) continue;
        assert(tape.cur_used[pos + i] && "bad access"), str += bfJump(tape, pos + i) + "[-]\n";
    }
    if (str.length())
        return comment + addIndent(str);
    else
        return "";
}

/// @brief allocate a segment cell, length: {size}, returned first cell: ${pos}
static std::string bfAlloc(Tape& tape, unsigned& pos, unsigned size = 1) {
    pos = tape.current_pos;
    while (1) {
        if (std::all_of(tape.cur_used + pos, tape.cur_used + pos + size,
                        [](bool used) { return !used; })) {
            break;
        }
        pos++;
    }
    for (unsigned i = 0; i < size; i++) tape.cur_used[pos + i] = true;
    std::string str = bfClear(tape, pos, size);
    for (unsigned i = 0; i < size; i++) tape.non_zero[pos + i] = true;
    // return std::format("; alloc #{} ~ #{}\n", pos, pos + size - 1) + addIndent(str);
    return str;
}

static std::string bfIf(Tape& tape, unsigned pos, std::function<std::string()> func) {
    assert(tape.cur_used[pos] && "bad access");
    const std::string comment = std::format("; if ${}\n", pos);
    std::string str;
    str += bfJump(tape, pos);
    str += "[[-]\n";
    str += addIndent(func());
    str += bfJump(tape, pos);
    str += "]\n";
    bfFree(tape, pos);
    return comment + addIndent(str);
}

static std::string bfIfElse(Tape& tape, unsigned pos, std::function<std::string()> then_func,
                            std::function<std::string()> else_func) {
    assert(tape.cur_used[pos] && "bad access");
    const std::string comment1 = std::format("; if ${}\n", pos), comment2 = std::format("; else\n");
    std::string str1, str2;
    unsigned tmp;
    str1 += bfAlloc(tape, tmp);
    str1 += bfJump(tape, tmp) + "+\n";
    str1 += bfJump(tape, pos);
    str1 += "[\n[-]\n";
    str1 += bfJump(tape, tmp) + "-\n";
    str1 += addIndent(then_func());
    str1 += bfJump(tape, pos);
    str1 += "]\n";
    str2 += bfJump(tape, tmp);
    str2 += "[-\n";
    str2 += addIndent(else_func());
    str2 += bfJump(tape, tmp);
    str2 += "]\n";
    bfFree(tape, tmp);
    bfFree(tape, pos);
    return comment1 + addIndent(str1) + comment2 + addIndent(str2);
}

/// @brief move cell from src to dest, take the ownership of ${src}
static std::string bfMove(Tape& tape, unsigned src, unsigned dest, int sign = 1,
                          bool free_after_move = true) {
    assert(tape.cur_used[src] && "bad access");
    assert(tape.cur_used[dest] && "bad access");
    if (src == dest) return "";
    const std::string comment = std::format("; move(#{}) to #{}\n", src, dest);
    std::string str, ch = sign > 0 ? "+" : "-";
    str += bfFor(tape, src, [&] { return bfJump(tape, dest) + ch + "\n"; }, free_after_move);
    return comment + addIndent(str);
}

static std::string bfCopy(Tape& tape, unsigned src, std::vector<unsigned> dest, int sign) {
    std::string comment = std::format("; copy(#{}) to ", src);
    for (auto d : dest) {
        comment += std::format("#{} ", d);
        assert(tape.cur_used[d] && "bad access");
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
    tape.cur_used[src] = true;
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
    assert(tape.cur_used[pos] && "bad access");
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
    assert(tape.cur_used[x] && tape.cur_used[y] && tape.cur_used[ret] && "bad access");
    const std::string comment = std::format("; add(${} ${}) to #{}\n", x, y, ret);
    std::string str;
    str += bfMove(tape, x, ret, 1);
    str += bfMove(tape, y, ret, 1);
    return comment + addIndent(str);
}

static std::string bfSub(Tape& tape, unsigned x, unsigned y, unsigned ret) {
    assert(tape.cur_used[x] && tape.cur_used[y] && tape.cur_used[ret] && "bad access");
    const std::string comment = std::format("; sub(${} ${}) to #{}\n", x, y, ret);
    std::string str = bfMove(tape, x, ret, 1);
    str += bfMove(tape, y, ret, -1);
    return comment + addIndent(str);
}

static std::string bfMul(Tape& tape, unsigned x, unsigned y, unsigned ret) {
    assert(tape.cur_used[x] && tape.cur_used[y] && tape.cur_used[ret] && "bad access");
    const std::string comment = std::format("; mul(${} ${}) to #{}\n", x, y, ret);
    std::string str;
    str += bfFor(tape, x, [&] { return bfCopy(tape, y, ret, 1); });
    bfFree(tape, y);
    return comment + addIndent(str);
}

static std::string bfDiv(Tape& tape, unsigned x, unsigned y, unsigned ret) {
    assert(tape.cur_used[x] && tape.cur_used[y] && tape.cur_used[ret] && "bad access");
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
    assert(tape.cur_used[x] && tape.cur_used[y] && tape.cur_used[ret] && "bad access");
    const std::string comment = std::format("; or(${} ${}) to #{}\n", x, y, ret);
    std::string str;
    str += bfJump(tape, x);
    str += "[-\n" + bfClear(tape, y) + bfJump(tape, y) + "+\n" + bfJump(tape, x) + "]\n";
    str += bfMove(tape, y, ret);
    return comment + addIndent(str);
}

static std::string bfEqual(Tape& tape, unsigned x, unsigned y, unsigned ret) {
    assert(tape.cur_used[x] && tape.cur_used[y] && tape.cur_used[ret] && "bad access");
    const std::string comment = std::format("; eq(${} ${}) to #{}\n", x, y, ret);
    std::string str;
    str += bfJump(tape, x);
    str += "[-\n" + bfJump(tape, y) + "-\n" + bfJump(tape, x) + "]+\n" + bfJump(tape, y);
    str += "[\n" + bfJump(tape, x) + "-\n" + bfClear(tape, y) + "]\n";
    str += bfMove(tape, x, ret);
    bfFree(tape, y);
    return comment + addIndent(str);
}

static std::string bfBitNot(Tape& tape, unsigned x, unsigned ret) {
    assert(tape.cur_used[x] && tape.cur_used[ret] && "bad access");
    const std::string comment = std::format("; bit not(${}) to #{}\n", x, ret);
    unsigned tmp;
    std::string str;
    str += bfAlloc(tape, tmp) + bfJump(tape, x) + "-\n[\n" + bfJump(tape, tmp) + "-\n" +
           bfJump(tape, x) + "-]\n";
    str += bfJump(tape, tmp) + "[\n" + bfJump(tape, x) + "+" + bfJump(tape, tmp) + "-]\n";
    str += bfMove(tape, x, ret);
    bfFree(tape, tmp);
    return comment + addIndent(str);
}

static std::string bfLogicNot(Tape& tape, unsigned x, unsigned ret) {
    assert(tape.cur_used[x] && tape.cur_used[ret] && "bad access");
    const std::string comment = std::format("; not(${}) to #{}\n", x, ret);
    std::string str;
    str += bfInc(tape, ret, 1);
    str += bfIf(tape, x, [&] { return bfInc(tape, ret, -1); });
    return comment + addIndent(str);
}

static std::string bfNeq(Tape& tape, unsigned x, unsigned y, unsigned ret) {
    assert(tape.cur_used[x] && tape.cur_used[y] && tape.cur_used[ret] && "bad access");
    const std::string comment = std::format("; neq(${} ${}) to #{}\n", x, y, ret);
    std::string str;
    unsigned tmp;
    str += bfAlloc(tape, tmp);
    str += bfEqual(tape, x, y, tmp);
    str += bfLogicNot(tape, tmp, ret);
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
    runtimeError("unimplemented operator {}! (now supports {})", toIrOperatorName(oper), ops);
}

std::string ValueIR::printBf(bool compress, std::shared_ptr<BfContext> context) const {
    std::string str, comment;
    assert(context != nullptr);
    auto& ctx = *context;
    switch (inst) {
        case Inst::String: ctx.ret = ctx.getBlockPC(content); break;
        case Inst::Return:
            str += params[0]->printBf(compress, context);
            comment = std::format("; return ${}\n", ctx.ret);
            str += bfClear(ctx.tape, ctx.global_ret);
            str += bfMove(ctx.tape, ctx.ret, ctx.global_ret);
            str += bfClear(ctx.tape, ctx.pc);
            break;
        case Inst::Integer: str += bfInteger(ctx.tape, ctx.ret, std::stoi(content)); break;
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
            if (!ctx.symbol_table.contains(content))
                runtimeError("unallocated variable {}", content);
            // str += bfAlloc(ctx.tape, ctx.ret);
            // str += std::format("; alloc #{} for {}\n", ctx.ret, content);
            // debugLog("alloc #{} for variable {}", ctx.ret, content);
            // ctx.symbol_table[content] = ctx.ret;
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
            str += std::format("; load {} to #{}\n", content, ctx.ret);
            str += bfCopy(ctx.tape, ctx.symbol_table[content], ctx.ret);
            break;
        case Inst::Jump: {
            unsigned target = ctx.getBlockPC(content);
            comment = std::format("; jump {}(pc: {}):\n", content, target);
            str += bfJump(ctx.tape, ctx.pc);
            str += "[-]" + repeat("+", target) + "\n";
            break;
        }
        case Inst::Branch: {
            str += params[0]->printBf(compress, context);
            unsigned exp = ctx.ret;
            params[1]->printBf(compress, context);
            unsigned block_pc1 = ctx.ret;
            params[2]->printBf(compress, context);
            unsigned block_pc2 = ctx.ret;
            str += bfIfElse(
                ctx.tape, exp,
                [&] { return bfJump(ctx.tape, ctx.pc) + "[-]" + repeat("+", block_pc1) + "\n"; },
                [&] { return bfJump(ctx.tape, ctx.pc) + "[-]" + repeat("+", block_pc2) + "\n"; });
            break;
        }
        case Inst::Label: {
            str += ctx.label_end();
            int pc = ctx.getBlockPC(content);
            comment =
                std::format("; block {}(pc: {}):\n", content.length() ? content : "entry", pc);
            unsigned tmp, tmp2;
            str += bfJump(ctx.tape, ctx.pc);
            str += bfAlloc(ctx.tape, tmp, 2);
            str += bfCopy(ctx.tape, ctx.pc, tmp);
            str += bfInteger(ctx.tape, tmp2, pc);
            str += bfEqual(ctx.tape, tmp, tmp2, tmp + 1);
            str += bfJump(ctx.tape, tmp + 1);
            str += "[-\n";
            ctx.label_end = [tmp, this, context]() {
                bfFree(context->tape, tmp + 1);
                return std::format("; end block {}: jump to entry #{}\n",
                                   this->content.length() ? this->content : "entry", tmp + 1) +
                       bfJump(context->tape, tmp + 1) + "]\n";
            };
            break;
        }
        default: runtimeError("unimplemented value type `{}`", inst); break;
    }
    if (comment.length()) str = comment + addIndent(str);
    if (inst != Inst::Label) return addIndent(str);
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
    std::string str;
    context = std::make_shared<BfContext>();

    auto alloc = [&](const std::string& name) {
        unsigned pos;
        str += bfAlloc(context->tape, pos);
        context->symbol_table[name] = pos;
        debugLog("alloc cell #{} for variable `{}`", pos, name);
    };
    alloc("$RET"), alloc("$PC");
    context->global_ret = context->symbol_table["$RET"];
    context->pc = context->symbol_table["$PC"];
    std::function<void(BaseIR*)> traverse = [&](BaseIR* base) {
        if (MultiValueIR* multi = dynamic_cast<MultiValueIR*>(base)) {
            for (auto& value : multi->values) {
                traverse(value.get());
            }
        } else if (ValueIR* value = dynamic_cast<ValueIR*>(base)) {
            if (value->inst == Inst::Alloc) {
                alloc(value->content);
            }
            for (auto& param : value->params) {
                traverse(param.get());
            }
        }
    };
    traverse(blocks.get());

    str += bfInc(context->tape, context->pc, context->getBlockPC(""));
    // str += bfInc(context->tape, context->global_ret, 3);
    str += bfJump(context->tape, context->pc);
    str += "[  \t; pc\n";

    str += addIndent(blocks->printBf(compress, context));
    str += addIndent(context->label_end());

    str += bfJump(context->tape, context->pc);
    str += "]  \t; pc\n";
    str += bfJump(context->tape, context->global_ret);

    std::vector<unsigned> pos;
    for (unsigned i = 0; i < MAXN; i++) {
        if (context->tape.cur_used[i]) {
            int flag = 1;
            for (auto& [name, p] : context->symbol_table) {
                if (p == i) flag = 0;
            }
            if (flag) pos.push_back(i);
        }
    }
    if (pos.size()) {
        debugLog("warning: memory leaked at {}", pos);
    }

    return "; " + name + "()\n" + addIndent(str);
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
