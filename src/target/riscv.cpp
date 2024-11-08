#include "ir/ir.hpp"
#include "util.hpp"

#include <cmath>
#include <format>
#include <string>

class BaseIR::RiscvContext {
public:
    static constexpr int REG_SIZE = 7;
    class Register {
    public:
        RiscvContext* ctx = nullptr;
        int idx = -1;
        Register() = default;
        Register(RiscvContext* context, int index) : ctx(context), idx(index) {}
        ~Register() {
            if (ctx) ctx->freeRegister(*this);
            ctx = nullptr;
        }
        std::string toString() const {
            if (idx < 0 || idx > REG_SIZE) throw runtimeError("invalid register %{}", idx);
            return "t" + std::to_string(idx);
        }
    };
    using RegisterPtr = std::unique_ptr<Register>;
    using Int = int;
    using Stack = unsigned;  // need to be different from Int for pattern-matching
    using Reg = RegisterPtr;
    using Str = std::string;
    std::variant<Int, Stack, Reg, Str> ret;
    std::map<std::string, int> symbol_table;
    RegisterPtr newRegister() {
        int idx = -1;
        for (int i = 0; i < REG_SIZE; i++)
            if (!_reg_used[i]) {
                idx = i;
                break;
            }
        if (idx == -1) throw runtimeError("no available register");
        _reg_used[idx] = true;
        return std::make_unique<Register>(this, idx);
    }
    void freeRegister(const Register& reg) {
        debugLog("free {}", reg);
        if (reg.idx < 0 || reg.idx > REG_SIZE) throw runtimeError("invalid register %{}", reg.idx);
        _reg_used[reg.idx] = false;
    }
    int alloc(int size) {
        if (_stack_used + size > _stack_limit) throw runtimeError("stack overflow");
        int cur = _stack_used;
        _stack_used += size;
        return cur;
    }
    void putVariable(const std::string& name) {
        int pos = alloc(4);
        symbol_table[name] = pos;
    }
    int getVariable(std::string name) {
        if (!symbol_table.contains(name)) throw runtimeError("undefined variable {}", name);
        debugLog("got {}: {}", name, symbol_table[name]);
        return symbol_table[name];
    }
    explicit RiscvContext(int limit) : _stack_limit(limit) {}
    std::string allocStack() {
        if (!_stack_limit) return "";
        std::string str;
        if (_stack_limit < (1 << 12)) {
            str += std::format("  addi\tsp, sp, -{}\n", _stack_limit);
        } else {
            _size_reg = newRegister();
            str += std::format("  li\t{}, {}\n", _size_reg, _stack_limit);
            str += std::format("  add\tsp, sp, {}\n", _size_reg);
        }
        return str;
    }
    std::string freeStack() {
        if (!_stack_limit) return "";
        if (_stack_limit < (1 << 12)) {
            return std::format("  addi\tsp, sp, {}\n", _stack_limit);
        } else {
            return std::format("  sub\tsp, sp, {}\n", _size_reg);
        }
    }

private:
    bool _reg_used[REG_SIZE] = {false};
    int _stack_used = 0;
    const int _stack_limit;
    RegisterPtr _size_reg;
};

std::string ValueIR::printRiscV(std::shared_ptr<RiscvContext> context) const {
    debugLog("{}", serializeClass("ValueIR", inst, content, params));
#ifdef DEBUG
    auto format = [&](const char* fmt, const auto&... args) {
        std::string str = std::vformat(fmt, std::make_format_args(args...));
        debugLog(BLUE "{}" RESET, str);
        return str;
    };
#else
    using std::format;
#endif
    using RegisterPtr = std::unique_ptr<RiscvContext::Register>;
    assert(context != nullptr);
    auto ctx = context;
    std::string str;
    auto write_to_reg = Visitor{
        [&](RiscvContext::Stack stack) -> RegisterPtr {
            auto reg = ctx->newRegister();
            str += format("  lw\t{}, {}(sp)\n", reg, stack);
            return reg;
        },
        [&](RiscvContext::Reg&& reg) -> RegisterPtr { return reg; },
        [&](RiscvContext::Int val) -> RegisterPtr {
            auto reg = ctx->newRegister();
            str += format("  li\t{}, {}\n", reg, val);
            return reg;
        },
        [&](RiscvContext::Str&& str) -> RegisterPtr {
            throw runtimeError("can not write string \"{}\" to register", str);
        },
    };
    auto return_to_stack = [&](RegisterPtr&& reg, int pos) {
        str += format("  sw\t{}, {}(sp)\n", reg, pos);
        ctx->ret = (RiscvContext::Stack)pos;
    };
    std::vector<decltype(RiscvContext::ret)> rets;
    for (const auto& param : params) {
        str += param->printRiscV(context);
        rets.push_back(std::move(ctx->ret));
    }
    switch (inst) {
        case Inst::Label: str += format("\n{}:\n", content); break;
        case Inst::String: ctx->ret = (RiscvContext::Str)(content); break;
        case Inst::Jump: str += format("  j {}\n", content); break;
        case Inst::Branch: {
            auto reg = std::visit(write_to_reg, std::move(rets[0]));
            str += format("  bnez\t{}, {}\n", reg, std::get<RiscvContext::Str>(rets[1]));
            str += format("  j {}\n", std::get<RiscvContext::Str>(rets[2]));
            break;
        }
        case Inst::Return: {
            Match{std::move(rets[0])}(  //
                [&](RiscvContext::Int val) { str += format("  li a0, {}\n", val); },
                [&](RiscvContext::Stack stack) { str += format("  lw a0, {}(sp)\n", stack); },
                [&](RiscvContext::Reg&& reg) { str += format("  mv a0, {}\n", reg->toString()); },
                [&](RiscvContext::Str&& str) {
                    throw runtimeError("can not return string \"{}\"", str);
                });
            str += ctx->freeStack();
            str += "  ret\t\n";
            break;
        }
        case Inst::Binary: {
            auto left = std::visit(write_to_reg, std::move(rets[0]));
            auto right = std::visit(write_to_reg, std::move(rets[1]));
            auto res = ctx->newRegister();
            switch (toOperator(content)) {
                case Operator::gt: str += format("  sgt\t{}, {}, {}\n", res, left, right); break;
                case Operator::lt: str += format("  slt\t{}, {}, {}\n", res, left, right); break;
                case Operator::geq:
                    str += format("  slt\t{}, {}, {}\n", res, left, right);
                    str += format("  seqz\t{0}, {0}\n", res);
                    break;
                case Operator::leq:
                    str += format("  sgt\t{}, {}, {}\n", res, left, right);
                    str += format("  seqz\t{0}, {0}\n", res);
                    break;
                case Operator::neq:
                    str += format("  xor\t{}, {}, {}\n", res, left, right);
                    str += format("  seqz\t{0}, {0}\n", res);
                    str += format("  seqz\t{0}, {0}\n", res);
                    break;
                case Operator::eq:
                    str += format("  xor\t{}, {}, {}\n", res, left, right);
                    str += format("  seqz\t{0}, {0}\n", res);
                    break;
                case Operator::mod: str += format("  rem\t{}, {}, {}\n", res, left, right); break;
                case Operator::sub:
                case Operator::add:
                case Operator::mul:
                case Operator::div:
                case Operator::band:
                case Operator::bor:
                    str += format("  {}\t{}, {}, {}\n", content, res, left, right);
                    break;
                default: throw runtimeError("not implemented binary operator {}!", content);
            }
            return_to_stack(std::move(res), ctx->alloc(4));
            break;
        }
        case Inst::Load: ctx->ret = (RiscvContext::Stack)(ctx->getVariable(content)); break;
        case Inst::Alloc: ctx->putVariable(content); break;
        case Inst::Store: {
            auto reg = std::visit(write_to_reg, std::move(rets[0]));
            return_to_stack(std::move(reg), ctx->getVariable(content));
            break;
        }
        case Inst::Integer: ctx->ret = (RiscvContext::Int)(std::stoi(content)); break;
        default: throw runtimeError("not implemented value type {}!", inst);
    }
    return str;
}

std::string MultiValueIR::printRiscV(std::shared_ptr<RiscvContext> context) const {
    std::string str;
    for (const auto& value : values) {
        str += value->printRiscV(context);
    }
    return str;
}

std::string FunctionIR::printRiscV(std::shared_ptr<RiscvContext> context) const {
    std::string str = name + ":\n";
    int size = stackSize();
    debugLog("stack size: {}", size);
    size = (int)(std::ceil(size / 16.0) * 16);
    auto ctx = std::make_shared<RiscvContext>(size);
    RiscvContext::RegisterPtr size_reg;
    str += ctx->allocStack();
    str += blocks->printRiscV(ctx);
    return str;
}

std::string ProgramIR::printRiscV(std::shared_ptr<RiscvContext> context) const {
    std::string str;
    str += "  .text\n";
    for (const auto& obj : funcs) {
        if (const auto func = dynamic_cast<FunctionIR*>(obj.get())) {
            str += "  .globl " + func->name + "\n";
        }
    }
    for (const auto& func : funcs) {
        str += func->printRiscV(context);
    }
    return str;
}