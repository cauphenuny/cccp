#include "ir/ir.h"
#include "util.hpp"

#include <cmath>
#include <format>
#include <functional>
#include <map>
#include <string>
#include <utility>
#include <vector>

const int MAXN = 65536;

struct Tape {
    struct Var;
    unsigned current_pos{0};
    unsigned max_pos{0};
    bool cur_used[MAXN]{0};
    bool non_zero[MAXN]{0};
    std::string record_str;
    unsigned recurse_counter{0};
    struct RecurseGuard {
        Tape* tape;
        RecurseGuard(Tape* t) : tape(t) {
            ++tape->recurse_counter;
        }
        ~RecurseGuard() {
            --tape->recurse_counter;
        }
    };
    [[nodiscard]] RecurseGuard comment(const std::string& fmt, const auto&... params);
    void record(const std::string& str);
    void free(unsigned pos, unsigned size = 1);
    void clear(unsigned pos);
    auto alloc() -> Var;
    auto claim(unsigned pos) -> Var;
    void jump(unsigned pos);
    void move(Var src, Var& dest, int sign = 1);
    void copy(Var& src, std::vector<std::reference_wrapper<Var>> dest, int sign = 1);
    auto clone(Var& src) -> Var;
    void inc(Var& pos, int val);
    void add(Var x, Var y, Var& ret);
    void sub(Var x, Var y, Var& ret);
    void mul(Var x, Var y, Var& ret);
    void div(Var x, Var y, Var& ret);
    void mod(Var x, Var y, Var& ret);
    void logicOr(Var x, Var y, Var& ret);
    void logicNot(Var x, Var& ret);
    void eq(Var x, Var y, Var& ret);
    void lt(Var x, Var y, Var& ret);
    void neq(Var x, Var y, Var& ret);
    void construct_int(Var& pos, int value);
    auto create_int(int value) -> Var;
    void loop(Var&, std::function<void()>);
    void cond(Var, std::function<void()>);
    void cond(Var, std::function<void()>, std::function<void()>);
    struct Var {
        Tape* tape;
        unsigned pos;
        bool available{false};
        std::string debugInfo() const {
            return serializeClass("Var", tape, pos, available);
        }
        Var() : tape(nullptr), pos(0) {}
        Var(Tape* tape) : tape(tape), available(true) {
            for (unsigned i = 0; i < MAXN; i++) {
                if (tape->cur_used[i]) continue;
                pos = i;
                break;
            }
            tape->max_pos = std::max(tape->max_pos, pos);
            tape->cur_used[pos] = true, tape->clear(pos);
            tape->non_zero[pos] = true;
        }
        Var(Tape* tape, unsigned preset_pos) : tape(tape), pos(preset_pos), available(true) {
            if (tape->cur_used[pos]) runtimeError("double allocated cell #{}", pos);
            tape->max_pos = std::max(tape->max_pos, preset_pos);
            tape->cur_used[pos] = true;
            tape->non_zero[pos] = true;
            // debugLog("inplace allocated at {}", pos);
        }
        Var(Var&& v) : tape(v.tape), pos(v.pos), available(v.available) {
            // debugLog("move construct {} => {}", (void*)&v, (void*)this);
            v.available = false;
            if (!available) runtimeError("constructed by unavailable variable");
        }
        Var(const Var& v) = delete;
        Var& operator=(Var&& v) {
            // debugLog("move assign {} => {}", (void*)&v, (void*)this);
            this->~Var();
            if (this != &v) {
                tape = v.tape, pos = v.pos, available = v.available;
                v.available = false;
            }
            if (!available) runtimeError("assigned by unavailable variable");
            return *this;
        }
        Var& operator=(const Var& v) = delete;
        operator unsigned() const {
            if (!available) runtimeError("get value from unavailable variable");
            return pos;
        }
        std::string toString() const {
            return std::to_string(pos);
        }
        ~Var() {
            // debugLog("destruct {}", (void*)this);
            if (available) {
                tape->free(pos);
            }
            available = false;
        }
        Var& operator++();
        void operator++(int);
        Var& operator--();
        void operator--(int);
        Var& operator=(int);
        friend Var operator!(Var exp);
    };
};

using Var = Tape::Var;

struct BaseIR::BfContext {
    BfContext() = default;
    BfContext(const BfContext&) = delete;
    Tape tape;
    std::map<std::string, Var> symbol_table;

    Var pc, global_ret;
    std::variant<int, Var> ret;

    std::map<std::string, unsigned> labels;
    int getBlockPC(const std::string& block_name) {
        if (!labels.contains(block_name)) {
            int pc = labels.size() + 1;  // reserve pc=0 for exit
            debugLog("alloc pc={} for block `{}`", pc, block_name.length() ? block_name : "entry");
            labels[block_name] = pc;
        }
        return labels[block_name];
    }
    std::function<void()> label_end = [] {};
};

std::string repeat(std::string str, unsigned times) {
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

void Tape::record(const std::string& str) {
    record_str += addIndent(str, recurse_counter);
}

Tape::RecurseGuard Tape::comment(const std::string& fmt, const auto&... params) {
    record(std::vformat("; " + fmt, std::make_format_args<std::format_context>(params...)));
    return RecurseGuard(this);
}

void Tape::free(unsigned pos, unsigned size) {
    for (unsigned i = 0; i < size; i++) {
        if (!cur_used[pos + i]) debugLog("warning: double freed cell #{}", pos + i);
        cur_used[pos + i] = false;
    }
}

void Tape::jump(unsigned pos) {
    if (pos == current_pos) return;
    if (current_pos < pos) {
        record(repeat(">", pos - current_pos));
    } else {
        record(repeat("<", current_pos - pos));
    }
    current_pos = pos;
}

void Tape::clear(unsigned pos) {
    if (!non_zero[pos]) return;
    auto _ = comment("clear #{}", pos);
    assert(cur_used[pos] && "bad access");
    jump(pos);
    record("[-]\n");
}

Var Tape::alloc() {
    return Var(this);
}

Var Tape::claim(unsigned pos) {
    return Var(this, pos);
}

void Tape::loop(Var& cond, std::function<void()> then) {
    assert(cur_used[cond] && "bad access");
    auto _ = comment("loop #{}\n", cond);
    jump(cond), record("[\n");
    then();
    jump(cond), record("]\n");
}

void Tape::cond(Var cond, std::function<void()> then) {
    assert(cur_used[cond] && "bad access");
    auto _ = comment("if ${}\n", cond);
    jump(cond);
    record("[[-]\n");
    then();
    jump(cond);
    record("]\n");
}

void Tape::cond(Var cond, std::function<void()> then, std::function<void()> otherwise) {
    assert(cur_used[cond] && "bad access");
    auto _ = comment("if_else ${}\n", cond);
    Var tmp = alloc();
    tmp++;
    jump(cond), record("[\n[-]\n");
    tmp--, then();
    jump(cond), record("]");
    jump(tmp), record("[-\n");
    otherwise();
    jump(tmp), record("]");
}

void Tape::move(Var src, Var& dest, int sign) {
    // debugLog("tape.move {} => {}", src, dest);
    assert(cur_used[src]);
    assert(cur_used[dest]);
    if (src.pos == dest.pos) return;
    auto _ = comment("move #{} to #{}", src, dest);
    std::string op = sign > 0 ? "+" : "-";
    loop(src, [&] { jump(dest), record(op), --src; });
}

void Tape::copy(Var& src, std::vector<std::reference_wrapper<Var>> dest, int sign) {
    std::string comment_str = std::format("copy #{} to ", src);
    for (auto d : dest) {
        comment_str += std::format("#{} ", d.get());
        assert(cur_used[d.get()] && "bad access");
    }
    auto _ = comment(comment_str);
    Var tmp = alloc();
    dest.push_back(tmp);
    std::string ch = sign == 1 ? "+" : "-";
    loop(src, [this, &dest, &src, ch] {
        for (auto d : dest) {
            jump(d.get()), record(ch);
        }
        --src;
    });
    move(std::move(tmp), src);
}

Var Tape::clone(Var& src) {
    Var pos = alloc();
    copy(src, {pos});
    return pos;
}

void Tape::inc(Var& pos, int val) {
    assert(cur_used[pos] && "bad access");
    std::string ch, name;
    if (val > 0)
        ch = "+", name = "inc";
    else
        ch = "-", val = -val, name = "dec";
    auto _ = comment("{} #{} by {}", name, pos, val);
    jump(pos);
    record(repeat(ch, val));
}

void Tape::add(Var x, Var y, Var& ret) {
    assert(cur_used[x] && cur_used[y] && cur_used[ret] && "bad access");
    auto _ = comment("add (${} ${}) to #{}", x, y, ret);
    move(std::move(x), ret);
    move(std::move(y), ret);
}

void Tape::sub(Var x, Var y, Var& ret) {
    assert(cur_used[x] && cur_used[y] && cur_used[ret] && "bad access");
    auto _ = comment("sub (${} ${}) to #{}", x, y, ret);
    move(std::move(x), ret);
    move(std::move(y), ret, -1);
}

void Tape::mul(Var x, Var y, Var& ret) {
    assert(cur_used[x] && cur_used[y] && cur_used[ret] && "bad access");
    auto _ = comment("mul (${} ${}) to #{}", x, y, ret);
    loop(x, [this, &x, &y, &ret] { copy(y, {ret}), --x; });
}

void Tape::div(Var x, Var y, Var& ret) {
    assert(cur_used[x] && cur_used[y] && cur_used[ret] && "bad access");
    auto _ = comment("div (${} ${}) to #{}", x, y, ret);
    loop(x, [&] {
        auto tmp = clone(y);
        loop(tmp, [&] {
            x--;
            cond(!clone(x), [&] {
                tmp--;
                cond(clone(tmp), [&] {
                    ret--;
                    clear(tmp);
                });
                tmp++;
            });
            tmp--;
        });
        ret++;
    });
    /*
    while (x) {
        for (tmp = y; tmp; tmp--) {
            x--;
            if (!x) {
                tmp--;
                if (tmp) {
                    ret--;
                    tmp = 0;
                }
                tmp++;
            }
        }
        ret++;
    }
   */
}

void Tape::mod(Var x, Var y, Var& ret) {
    assert(cur_used[x] && cur_used[y] && cur_used[ret] && "bad access");
    auto _ = comment("mod (${} ${}) to #{}", x, y, ret);
    Var tmp0 = alloc(), tmp1 = alloc();
    div(clone(x), clone(y), tmp0);
    mul(std::move(tmp0), std::move(y), tmp1);
    sub(std::move(x), std::move(tmp1), ret);
}

void Tape::logicOr(Var x, Var y, Var& ret) {
    assert(cur_used[x] && cur_used[y] && cur_used[ret] && "bad access");
    auto _ = comment("or (${} ${}) to #{}", x, y, ret);
    jump(x), record("[-");
    clear(y), jump(y), record("+");
    jump(x), record("]");
    move(std::move(y), ret);
}

void Tape::logicNot(Var x, Var& ret) {
    assert(cur_used[x] && cur_used[ret] && "bad access");
    auto _ = comment("not (${}) to #{}", x, ret);
    ret++, cond(std::move(x), [&] { ret--; });
}

void Tape::eq(Var x, Var y, Var& ret) {
    assert(cur_used[x] && cur_used[y] && cur_used[ret] && "bad access");
    auto _ = comment("eq (${} ${}) to #{}", x, y, ret);
    jump(x), record("[-");
    y--, jump(x), record("]+"), jump(y);
    record("["), x--, clear(y), record("]");
    move(std::move(x), ret);
}

void Tape::lt(Var x, Var y, Var& ret) {
    assert(cur_used[x] && cur_used[y] && cur_used[ret] && "bad access");
    auto _ = comment("lt (${} ${}) to #{}", x, y, ret);
    cond(
        clone(y),
        [&] {
            Var tmp = alloc();
            mod(clone(x), std::move(y), tmp);
            eq(std::move(x), std::move(tmp), ret);
        },
        [&] { ret = 0; });  // FIXME: bug when use with `while` statement
}

/*
std::string bfBitNot(Tape& tape, unsigned x, unsigned ret) {
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
*/

Var& Var::operator--() {
    tape->inc(*this, -1);
    return *this;
}

void Var::operator--(int) {
    tape->inc(*this, -1);
}

Var& Var::operator++() {
    tape->inc(*this, 1);
    return *this;
}

void Var::operator++(int) {
    tape->inc(*this, 1);
}

Var operator+(Var lhs, Var rhs) {
    Var ret = lhs.tape->alloc();
    lhs.tape->add(std::move(lhs), std::move(rhs), ret);
    return ret;
}

Var operator-(Var lhs, Var rhs) {
    Var ret = lhs.tape->alloc();
    lhs.tape->sub(std::move(lhs), std::move(rhs), ret);
    return ret;
}

Var operator*(Var lhs, Var rhs) {
    Var ret = lhs.tape->alloc();
    lhs.tape->mul(std::move(lhs), std::move(rhs), ret);
    return ret;
}

Var operator!=(Var lhs, Var rhs) {
    Var ret = lhs.tape->alloc(), tmp = lhs.tape->alloc();
    lhs.tape->eq(std::move(lhs), std::move(rhs), tmp);
    lhs.tape->logicNot(std::move(tmp), ret);
    return ret;
}

Var operator==(Var lhs, Var rhs) {
    Var ret = lhs.tape->alloc();
    lhs.tape->eq(std::move(lhs), std::move(rhs), ret);
    return ret;
}

Var operator!(Var var) {
    Var ret = var.tape->alloc();
    var.tape->logicNot(std::move(var), ret);
    return ret;
}

Var operator||(Var lhs, Var rhs) {
    Var ret = lhs.tape->alloc();
    lhs.tape->logicOr(std::move(lhs), std::move(rhs), ret);
    return ret;
}

Var operator&&(Var lhs, Var rhs) {
    return !(!std::move(lhs) || !std::move(rhs));
}

Var operator/(Var lhs, Var rhs) {
    Var ret = lhs.tape->alloc();
    lhs.tape->div(std::move(lhs), std::move(rhs), ret);
    return ret;
}

Var operator%(Var lhs, Var rhs) {
    Var ret = lhs.tape->alloc();
    lhs.tape->mod(std::move(lhs), std::move(rhs), ret);
    return ret;
}

Var operator<(Var lhs, Var rhs) {
    Var ret = lhs.tape->alloc();
    lhs.tape->lt(std::move(lhs), std::move(rhs), ret);
    return ret;
}

Var operator>(Var lhs, Var rhs) {
    return std::move(rhs) < std::move(lhs);
}

Var operator<=(Var lhs, Var rhs) {
    return !(std::move(rhs) < std::move(lhs));
}

Var operator>=(Var lhs, Var rhs) {
    return !(std::move(lhs) < std::move(rhs));
}

void Tape::construct_int(Var& pos, int value) {
    auto _ = comment("int {} to #{}", (uint8_t)value, pos);
    if (!value) return;
    jump(pos);
    record(repeat(value > 0 ? "+" : "-", std::abs(value)));
}

Var Tape::create_int(int value) {
    Var pos = alloc();
    construct_int(pos, value);
    return pos;
}

Var& Var::operator=(int value) {
    tape->clear(pos);
    tape->construct_int(*this, value);
    return *this;
}

std::string ValueIR::printBf(std::shared_ptr<BfContext> context) const {
    std::string str;
    assert(context != nullptr);
    auto& ctx = *context;
    auto& tape = ctx.tape;
    std::vector<decltype(BfContext::ret)> ret;
    for (auto& param : params) {
        param->printBf(context);
        ret.push_back(std::move(ctx.ret));
    }
    auto get = [&ret](size_t index) { return std::move(std::get<Var>(ret[index])); };
    auto get_int = [&ret](size_t index) { return std::get<int>(ret[index]); };
    switch (inst) {
        case Inst::String: ctx.ret = ctx.getBlockPC(content); break;
        case Inst::Return: {
            if (ret.size()) {
                Var pos = get(0);
                auto _ = tape.comment("return ${}", pos);
                tape.clear(ctx.global_ret);
                tape.move(std::move(pos), ctx.global_ret);
                tape.clear(ctx.pc);
            } else {
                auto _ = tape.comment("return");
                tape.clear(ctx.pc);
            }
            break;
        }
        case Inst::Integer: {
            ctx.ret = tape.create_int(std::stoi(content));
            break;
        }
        case Inst::Binary: {
            switch (toOperator(content)) {
                case Operator::eq: ctx.ret = get(0) == get(1); break;
                case Operator::lt: ctx.ret = get(0) < get(1); break;
                case Operator::gt: ctx.ret = get(0) > get(1); break;
                case Operator::leq: ctx.ret = get(0) <= get(1); break;
                case Operator::geq: ctx.ret = get(0) >= get(1); break;
                case Operator::add: ctx.ret = get(0) + get(1); break;
                case Operator::sub: ctx.ret = get(0) - get(1); break;
                case Operator::mul: ctx.ret = get(0) * get(1); break;
                case Operator::neq: ctx.ret = get(0) != get(1); break;
                case Operator::div: ctx.ret = get(0) / get(1); break;
                case Operator::mod: ctx.ret = get(0) % get(1); break;
                case Operator::bor: ctx.ret = get(0) || get(1); break;
                case Operator::band: ctx.ret = get(0) && get(1); break;
                default: runtimeError("unimplemented operator `{}`", content);
            }
            break;
        }
        case Inst::Alloc:
            if (!ctx.symbol_table.contains(content))
                runtimeError("unallocated variable {}", content);
            break;
        case Inst::Store: {
            Var exp = get(0), &var = ctx.symbol_table[content];
            auto _ = tape.comment("store ${} to {}(#{})", exp, content, var);
            tape.clear(var);
            tape.move(std::move(exp), var);
            break;
        }
        case Inst::Load: {
            Var pos = tape.alloc(), &var = ctx.symbol_table[content];
            auto _ = tape.comment("load {}(#{}) to #{}", content, var, pos);
            tape.copy(var, {pos});
            ctx.ret = std::move(pos);
            break;
        }
        case Inst::Jump: {
            unsigned target = ctx.getBlockPC(content);
            auto _ = tape.comment("jump {}", target);
            ctx.pc = target;
            break;
        }
        case Inst::Branch: {
            Var exp = get(0);
            unsigned block_pc1 = get_int(1), block_pc2 = get_int(2);
            auto _ = tape.comment("jump ${} ? {} : {}", exp, block_pc1, block_pc2);
            tape.cond(
                std::move(exp),  //
                [&] { ctx.pc = block_pc1; }, [&] { ctx.pc = block_pc2; });
            break;
        }
        case Inst::Label: {
            ctx.label_end(), ctx.label_end = []() {};
            int pc = ctx.getBlockPC(content);
            tape.record(
                std::format("; BLOCK {}(PC: {}):", content.length() ? content : "entry", pc));
            auto recurse_guard = std::make_shared<Tape::RecurseGuard>(&tape);
            auto tmp =
                std::make_shared<Var>(tape.alloc());  // std::function only supports copy,
                                                      // and std::move_only_function requires c++23
            tape.eq(tape.clone(ctx.pc), tape.create_int(pc), *tmp);
            tape.jump(*tmp), tape.record("[-");
            ctx.label_end = [this, tmp, context, recurse_guard]() {
                auto _ = context->tape.comment("ENDBLOCK {}",
                                               this->content.length() ? this->content : "entry");
                context->tape.jump(*tmp), context->tape.record("]");
            };
            break;
        }
        default: runtimeError("unimplemented value type `{}`", inst); break;
    }
    return "";  // string stored in context
}

std::string MultiValueIR::printBf(std::shared_ptr<BfContext> context) const {
    std::string str;
    for (auto& value : values) {
        str += value->printBf(context);
    }
    return str;
}

std::string FunctionIR::printBf(std::shared_ptr<BfContext> context) const {
    context = std::make_shared<BfContext>();
    auto& tape = context->tape;

    auto _ = tape.comment("FUNC {}:", name);
    context->global_ret = tape.alloc(), context->pc = tape.alloc();
    debugLog("alloc cell #{} for RET, #{} for PC", context->global_ret, context->pc);
    std::function<void(BaseIR*)> traverse = [&](BaseIR* base) {
        if (MultiValueIR* multi = dynamic_cast<MultiValueIR*>(base)) {
            for (auto& value : multi->values) {
                traverse(value.get());
            }
        } else if (ValueIR* value = dynamic_cast<ValueIR*>(base)) {
            if (value->inst == Inst::Alloc) {
                context->symbol_table[value->content] = tape.alloc();
            }
            for (auto& param : value->params) {
                traverse(param.get());
            }
        }
    };
    traverse(blocks.get());

    tape.inc(context->pc, context->getBlockPC(""));
    tape.jump(context->pc), tape.record("[");

    blocks->printBf(context);
    context->label_end(),
        context->label_end = [] {};  // reset for calling destructor of previous Var

    tape.jump(context->pc), tape.record("]");
    tape.jump(context->global_ret);

    std::vector<unsigned> pos;
    for (unsigned i = 0; i < MAXN; i++) {
        if (i == context->global_ret.pos || i == context->pc.pos) continue;
        if (context->tape.cur_used[i]) {
            int flag = 1;
            for (auto& [name, p] : context->symbol_table) {
                if (p.pos == i) flag = 0;
            }
            if (flag) pos.push_back(i);
        }
    }
    if (pos.size()) {
        debugLog("warning: memory leaked at {}", pos);
    }
    debugLog("stack size: {}", context->tape.max_pos);

    return context->tape.record_str;
}

std::string ProgramIR::printBf(std::shared_ptr<BfContext> context) const {
    std::string str;
    for (const auto& func : funcs) {
        str += func->printBf(context);
    }
    return str;
}
