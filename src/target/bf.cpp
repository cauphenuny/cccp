#include "ir/ir.h"
#include "util.hpp"

#include <functional>
#include <map>
#include <string>
#include <vector>

const int MAXN = 65536;

struct Tape {
    struct Var;
    unsigned current_pos{0};
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
    void record(const std::string& str);
    void free(unsigned pos, unsigned size = 1);
    void clear(unsigned pos, unsigned size = 1);
    auto alloc() -> Var;
    auto alloc(unsigned pos) -> Var;
    void jump(unsigned pos);
    void move(Var src, Var& dest, int sign = 1);
    void copy(Var& src, std::vector<std::reference_wrapper<Var>> dest, int sign = 1);
    auto clone(Var& src) -> Var;
    void inc(Var& pos, int val);
    void add(Var x, Var y, Var& ret);
    void sub(Var x, Var y, Var& ret);
    void mul(Var x, Var y, Var& ret);
    void logicOr(Var x, Var y, Var& ret);
    void logicNot(Var x, Var& ret);
    void equal(Var x, Var y, Var& ret);
    void neq(Var x, Var y, Var& ret);
    void construct_int(Var& pos, int value);
    auto create_int(int value) -> Var;
    void exec(Var pos, std::function<void()> func);
    void cond(Var pos, std::function<void()> func);
    void cond(Var pos, std::function<void()> then_func, std::function<void()> else_func);
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
            tape->cur_used[pos] = true, tape->clear(pos);
            tape->non_zero[pos] = true;
        }
        Var(Tape* tape, int preset_pos) : tape(tape), pos(preset_pos), available(true) {
            if (tape->cur_used[pos]) runtimeError("double allocated cell #{}", pos);
            tape->cur_used[pos] = true, tape->clear(pos);
            tape->non_zero[pos] = true;
            tape->clear(pos);
            debugLog("inplace allocated at {}", pos);
        }
        Var(Var&& v) : tape(v.tape), pos(v.pos), available(v.available) {
            debugLog("move construct {} => {}", (void*)&v, (void*)this);
            v.available = false;
            if (!available) runtimeError("constructed by unavailable variable");
        }
        Var(const Var& v) = delete;
        Var& operator=(Var&& v) {
            debugLog("move assign {} => {}", (void*)&v, (void*)this);
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
            debugLog("destruct {}", (void*)this);
            if (available) {
                tape->free(pos);
            }
            available = false;
        }
        Var& operator++();
        Var& operator--();
        friend Var operator+(Var lhs, Var rhs);
        friend Var operator-(Var lhs, Var rhs);
        friend Var operator*(Var lhs, Var rhs);
        friend Var operator||(Var lhs, Var rhs);
        friend Var operator!(Var exp);
        friend Var operator==(Var lhs, Var rhs);
        friend Var operator!=(Var lhs, Var rhs);
    };
};

using Var = Tape::Var;

struct BaseIR::BfContext {
    BfContext() = default;
    BfContext(const BfContext&) = delete;
    Tape tape;
    std::map<std::string, unsigned> symbol_table_legacy;
    std::map<std::string, Var> symbol_table;

    unsigned pc_legacy, global_ret_legacy;
    unsigned ret_legacy;
    Var pc, global_ret;
    std::variant<int, Var> ret;

    std::map<std::string, unsigned> labels;
    int getBlockPC(const std::string& block_name) {
        if (!labels.contains(block_name)) {
            int pc = labels.size() + 1;  // reserve pc @0 for exit
            debugLog("alloc pc @{} for block `{}`", pc, block_name.length() ? block_name : "entry");
            labels[block_name] = pc;
        }
        return labels[block_name];
    }
    std::function<std::string()> label_end_legacy = []() { return ""; };
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

void Tape::exec(Var pos, std::function<void()> func) {
    assert(cur_used[pos] && "bad access");
    debugLog("exec get {}, content: {}", (void*)&pos, pos.debugInfo());
    record(std::format("; do for ${} times\n", pos));
    RecurseGuard _(this);
    jump(pos), record("[\n");
    func();
    jump(pos), record("-]\n");
    debugLog("content: {}", pos.debugInfo());
}

void Tape::clear(unsigned pos, unsigned size) {
    record(std::format("; clear(#{} {})\n", pos, size));
    RecurseGuard _(this);
    for (unsigned i = 0; i < size; i++) {
        if (!non_zero[pos + i]) continue;
        assert(cur_used[pos + i] && "bad access");
        jump(pos + i);
        record("[-]\n");
    }
}

Var Tape::alloc() {
    return Var(this);
}

Var Tape::alloc(unsigned pos) {
    return Var(this, pos);
}

void Tape::cond(Var pos, std::function<void()> func) {
    assert(cur_used[pos] && "bad access");
    record(std::format("; if ${}\n", pos));
    RecurseGuard _(this);
    jump(pos);
    record("[[-]\n");
    func();
    jump(pos);
    record("]\n");
}

void Tape::cond(Var pos, std::function<void()> then_func, std::function<void()> else_func) {
    assert(cur_used[pos] && "bad access");
    record(std::format("; if_else ${}\n", pos));
    RecurseGuard _(this);
    Var tmp = alloc();
    ++tmp;
    jump(pos), record("[\n[-]\n");
    --tmp, then_func();
    jump(pos), record("]");
    jump(tmp), record("[-\n");
    else_func();
    jump(tmp), record("]");
}

void Tape::move(Var src, Var& dest, int sign) {
    debugLog("tape.move {} => {}", src, dest);
    assert(cur_used[src]);
    assert(cur_used[dest]);
    if (src.pos == dest.pos) return;
    record(std::format("; move(#{}) to #{}\n", src, dest));
    RecurseGuard _(this);
    std::string op = sign > 0 ? "+" : "-";
    exec(std::move(src), [&] { jump(dest), record(op); });
}

void Tape::copy(Var& src, std::vector<std::reference_wrapper<Var>> dest, int sign) {
    std::string comment = std::format("; copy(#{}) to ", src);
    for (auto d : dest) {
        comment += std::format("#{} ", d.get());
        assert(cur_used[d.get()] && "bad access");
    }
    record(comment);
    RecurseGuard _(this);
    Var tmp = alloc();
    dest.push_back(tmp);
    std::string ch = sign == 1 ? "+" : "-";
    debugLog("exec start");
    unsigned pos = src.pos;
    exec(std::move(src), [this, &dest, ch] {
        for (auto d : dest) {
            jump(d.get()), record(ch);
        }
    });
    debugLog("exec end, src: {}, content: {}", (void*)&src, src.debugInfo());
    assert(!src.available);
    src = alloc(pos);
    debugLog("inplace allocated {}, pos {}", src.pos, pos);
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
    record(std::format("; {}(#{} by {})\n", name, pos, val));
    RecurseGuard _(this);
    jump(pos);
    record(repeat(ch, val));
}

void Tape::add(Var x, Var y, Var& ret) {
    assert(cur_used[x] && cur_used[y] && cur_used[ret] && "bad access");
    record(std::format("; add(${} ${}) to #{}\n", x, y, ret));
    RecurseGuard _(this);
    move(std::move(x), ret);
    move(std::move(y), ret);
}

void Tape::sub(Var x, Var y, Var& ret) {
    assert(cur_used[x] && cur_used[y] && cur_used[ret] && "bad access");
    record(std::format("; sub(${} ${}) to #{}\n", x, y, ret));
    RecurseGuard _(this);
    move(std::move(x), ret);
    move(std::move(y), ret, -1);
}

void Tape::mul(Var x, Var y, Var& ret) {
    assert(cur_used[x] && cur_used[y] && cur_used[ret] && "bad access");
    record(std::format("; mul(${} ${}) to #{}\n", x, y, ret));
    RecurseGuard _(this);
    exec(std::move(x), [this, &y, &ret] { copy(y, {ret}); });
}

void Tape::logicOr(Var x, Var y, Var& ret) {
    assert(cur_used[x] && cur_used[y] && cur_used[ret] && "bad access");
    record(std::format("; or(${} ${}) to #{}\n", x, y, ret));
    RecurseGuard _(this);
    jump(x), record("[-");
    clear(y), jump(y), record("+");
    jump(x), record("]");
    move(std::move(y), ret);
}

void Tape::equal(Var x, Var y, Var& ret) {
    assert(cur_used[x] && cur_used[y] && cur_used[ret] && "bad access");
    record(std::format("; eq(${} ${}) to #{}\n", x, y, ret));
    RecurseGuard _(this);
    jump(x), record("[-");
    --y, jump(x), record("]+"), jump(y);
    record("["), --x, clear(y), record("]");
    move(std::move(x), ret);
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

void Tape::logicNot(Var x, Var& ret) {
    assert(cur_used[x] && cur_used[ret] && "bad access");
    record(std::format("; not(${}) to #{}\n", x, ret));
    RecurseGuard _(this);
    ++ret, cond(std::move(x), [&] { --ret; });
}

void Tape::neq(Var x, Var y, Var& ret) {
    assert(cur_used[x] && cur_used[y] && cur_used[ret] && "bad access");
    record(std::format("; neq(${} ${}) to #{}\n", x, y, ret));
    RecurseGuard _(this);
    Var tmp = alloc();
    equal(std::move(x), std::move(y), tmp);
    logicNot(std::move(tmp), ret);
}

Var& Var::operator--() {
    tape->inc(*this, -1);
    return *this;
}

Var& Var::operator++() {
    tape->inc(*this, 1);
    return *this;
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
    Var ret = lhs.tape->alloc();
    lhs.tape->neq(std::move(lhs), std::move(rhs), ret);
    return ret;
}

Var operator==(Var lhs, Var rhs) {
    Var ret = lhs.tape->alloc();
    lhs.tape->equal(std::move(lhs), std::move(rhs), ret);
    return ret;
}

Var operator||(Var lhs, Var rhs) {
    Var ret = lhs.tape->alloc();
    lhs.tape->logicOr(std::move(lhs), std::move(rhs), ret);
    return ret;
}

void Tape::construct_int(Var& pos, int value) {
    record(std::format("; int({}) to #{}\n", (uint8_t)value, pos));
    RecurseGuard _(this);
    clear(pos), jump(pos);
    record(repeat(value > 0 ? "+" : "-", std::abs(value)));
}

Var Tape::create_int(int value) {
    Var pos = alloc();
    construct_int(pos, value);
    return pos;
}

std::string ValueIR::printBf(bool compress, std::shared_ptr<BfContext> context) const {
    std::string str, comment;
    assert(context != nullptr);
    auto& ctx = *context;
    auto& tape = ctx.tape;
    std::vector<decltype(BfContext::ret)> ret;
    for (auto& param : params) {
        param->printBf(compress, context);
        ret.push_back(std::move(ctx.ret));
    }
    auto get_var = [&ret](size_t index) { return std::move(std::get<Var>(ret[index])); };
    auto get_int = [&ret](size_t index) { return std::get<int>(ret[index]); };
    switch (inst) {
        case Inst::String: ctx.ret = ctx.getBlockPC(content); break;
        case Inst::Return: {
            if (ret.size()) {
                Var pos = get_var(0);
                comment = std::format("; return ${}\n", pos);
                tape.clear(ctx.global_ret);
                tape.move(std::move(pos), ctx.global_ret);
                tape.clear(ctx.pc);
            } else {
                comment = std::format("; return\n");
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
                case Operator::add: ctx.ret = get_var(0) + get_var(1); break;
                case Operator::sub: ctx.ret = get_var(0) - get_var(1); break;
                case Operator::mul: ctx.ret = get_var(0) * get_var(1); break;
                case Operator::bor: ctx.ret = get_var(0) || get_var(1); break;
                case Operator::eq: ctx.ret = get_var(0) == get_var(1); break;
                case Operator::neq: ctx.ret = get_var(0) != get_var(1); break;
                default: runtimeError("unimplemented operator `{}`", content);
            }
            break;
        }
        case Inst::Alloc:
            if (!ctx.symbol_table.contains(content))
                runtimeError("unallocated variable {}", content);
            break;
        case Inst::Store: {
            Var exp = get_var(0), &var = ctx.symbol_table[content];
            tape.clear(var);
            tape.move(std::move(exp), var);
            break;
        }
        case Inst::Load: {
            Var pos = tape.alloc();
            tape.record(std::format("; load {} to #{}\n", content, pos));
            tape.copy(ctx.symbol_table[content], {pos});
            ctx.ret = std::move(pos);
            break;
        }
        case Inst::Jump: {
            unsigned target = ctx.getBlockPC(content);
            tape.record(std::format("; jump {}({}):", content, target));
            tape.construct_int(ctx.pc, target);
            break;
        }
        case Inst::Branch: {
            Var exp = get_var(0);
            unsigned block_pc1 = get_int(1), block_pc2 = get_int(2);
            tape.record(std::format("; branch: ${} ? {} : {}", exp, block_pc1, block_pc2));
            tape.cond(
                std::move(exp),  //
                [&] { tape.construct_int(ctx.pc, block_pc1); },
                [&] { tape.construct_int(ctx.pc, block_pc2); });
            break;
        }
        case Inst::Label: {
            ctx.label_end();
            int pc = ctx.getBlockPC(content);
            tape.record(
                std::format("; block {}(pc: {}):", content.length() ? content : "entry", pc));
            auto tmp = std::make_shared<Var>(tape.alloc());
            tape.equal(tape.clone(ctx.pc), tape.create_int(pc), *tmp);
            tape.jump(*tmp), tape.record("[-");
            ctx.label_end = [this, tmp, context]() {
                context->tape.record(std::format("; end block {}: jump to entry #{}",
                                                 this->content.length() ? this->content : "entry",
                                                 *tmp));
                context->tape.jump(*tmp), context->tape.record("]");
            };
            break;
        }
        default: runtimeError("unimplemented value type `{}`", inst); break;
    }
    return "";  // string stored in context
}

std::string MultiValueIR::printBf(bool compress, std::shared_ptr<BfContext> context) const {
    std::string str;
    for (auto& value : values) {
        str += value->printBf(compress, context);
    }
    return str;
}

std::string FunctionIR::printBf(bool compress, std::shared_ptr<BfContext> context) const {
    context = std::make_shared<BfContext>();
    auto& tape = context->tape;

    tape.record(std::format("; {}:", name));
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

    blocks->printBf(compress, context);
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

    return context->tape.record_str;
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
