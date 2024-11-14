#include "ir.h"
#include "util.hpp"
#include <format>

std::string toString(const Operator& oper) {
    for (const auto& [op, raw] : raw_map)
        if (op == oper) return raw;
    runtimeError("unknown operator: {}", (int)oper);
}

const char* toIrOperatorName(Operator oper) {
    for (const auto& [op, name] : name_map)
        if (op == oper) return name;
    runtimeError("unknown operator: {}", oper);
}

Operator toOperator(const std::string& str) {
    for (const auto& [op, name] : name_map)
        if (name == str) return op;
    runtimeError("unknown operator: {}", str);
}

// clang-format off
std::string toString(Inst t) {
    switch (t) {
        case Inst::Unknown: return "Unknown"; case Inst::ZeroInit: return "ZeroInit"; case Inst::FuncArgRef: return "FuncArgRef"; 
        case Inst::GlobalAlloc: return "GlobalAlloc"; case Inst::Alloc: return "Alloc"; case Inst::Load: return "Load";
        case Inst::Store: return "Store"; case Inst::GetPtr: return "GetPtr"; case Inst::GetElemPtr: return "GetElemPtr"; 
        case Inst::Binary: return "Binary"; case Inst::Branch: return "Branch"; case Inst::Jump: return "Jump";
        case Inst::Call: return "Call"; case Inst::Return: return "Return"; case Inst::Integer: return "Integer";
        case Inst::String: return "String"; case Inst::Label: return "Label";
        default: return "Invalid";
    }
}
// clang-format on

struct BaseIR::IrContext {
    std::string ret;
    int cnt = 0;
};

BaseIR::BaseIR() : type(std::make_unique<Type>()) {}

BaseIR::~BaseIR() = default;

int BaseIR::stackSize() const {
    runtimeError("can not calculate stack size for {}", typeid(*this).name());
}

BaseIR::Type::Type(Tag t) : tag(t) {}

std::string BaseIR::Type::toString() const {
    switch (tag) {
        case Tag::Unit: return "unit";
        case Tag::Int32: return "i32";
        case Tag::Array: {
            auto& [base, size] = std::get<ArrayData>(data);
            return std::format("{}[{}]", base, size);
        }
        case Tag::Pointer: {
            auto& [base] = std::get<PointerData>(data);
            return std::format("{}*", base);
        }
        case Tag::Function: {
            auto& [ret, args] = std::get<FunctionData>(data);
            std::string args_str;
            for (const auto& arg : args) {
                if (!args_str.empty()) args_str += ", ";
                args_str += arg->toString();
            }
            return std::format("{}({})", ret, args_str);
        }
        default: runtimeError("invalid type tag {}", (int)tag);
    }
}

ValueIR::ValueIR(Inst inst) : inst(inst) { setType(); }

ValueIR::ValueIR(const std::string& content) : inst(Inst::String), content(content) {}

ValueIR::ValueIR(Inst inst, const std::string& content) : inst(inst), content(content) { setType(); }

std::string ValueIR::toString() const {
    return serializeClass("ValueIR", inst, content, params);
}

std::string ValueIR::print(std::shared_ptr<IrContext> ctx) const {
    assert(ctx != nullptr);
    std::string str;
    std::vector<std::string> ret;
    for (const auto& param : params) {
        str += param->print(ctx);
        ret.push_back(ctx->ret);
    }
    using std::format;
    switch (inst) {
        case Inst::Label:
            if (content.length())
                str += format("\n%{}:\n", content);
            else 
                str += format("%{}:\n", "entry");
            break;
        case Inst::String:
        case Inst::Integer: ctx->ret = content; break;
        case Inst::Load:
            ctx->ret = format("%{}", ctx->cnt++);
            str += format("  {} = load {}\n", ctx->ret, content);
            break;
        case Inst::Alloc: str += format("  {} = alloc {}\n", content, type); break;
        case Inst::Store: str += format("  store {}, {}\n", ret[0], content); break;
        case Inst::Binary: {
            ctx->ret = format("%{}", ctx->cnt++);
            str += format("  {} = {} {}, {}\n", ctx->ret, content, ret[0], ret[1]);
            break;
        }
        case Inst::Return: str += format("  ret {}\n", ret[0]); break;
        case Inst::Jump: str += format("  jump %{}\n", content); break;
        case Inst::Branch: str += format("  br {}, %{}, %{}\n", ret[0], ret[1], ret[2]); break;
        default: runtimeError("unimplemented value type `{}`", inst);
    }
    return str;
}

int ValueIR::stackSize() const {
    int size;
    switch (inst) {
        case Inst::Binary:
        case Inst::Alloc: size = 4; break;
        default: size = 0;
    }
    for (const auto& param : params) size += param->stackSize();
    return size;
}

void ValueIR::setType() {
    switch (inst) {
        case Inst::Integer:
        case Inst::Load:
        case Inst::Binary: type = std::make_unique<Type>(Type::Tag::Int32); break;
        default: break;
    }
}

std::string MultiValueIR::toString() const { return serializeClass("MultipleIR", values); }

std::string MultiValueIR::print(std::shared_ptr<IrContext> ctx) const {
    std::string str;
    for (const auto& value : values) {
        str += value->print(ctx);
    }
    return str;
}

void MultiValueIR::add(IrObject&& value) {
    if (!value) return;
    if (auto* value_ir = dynamic_cast<ValueIR*>(value.get())) {
        if (value_ir->inst == Inst::Label) terminated = false;
        if (terminated) return;
        values.push_back(std::move(value));
        if (value_ir->inst == Inst::Return || value_ir->inst == Inst::Branch ||
            value_ir->inst == Inst::Jump) {
            terminated = true;
        }
    } else if (auto* multiple_ir = dynamic_cast<MultiValueIR*>(value.get())) {
        for (auto& val : multiple_ir->values) {
            add(std::move(val));
        }
    } else {
        runtimeError("invalid value type {}", typeid(value).name());
    }
}

int MultiValueIR::stackSize() const {
    int size = 0;
    for (const auto& value : values) {
        size += value->stackSize();
    }
    return size;
}

FunctionIR::FunctionIR(const std::string& name) : name(name) {
    type = std::make_unique<Type>(Type::Tag::Int32);
}

std::string FunctionIR::toString() const { return serializeClass("FunctionIR", name, blocks); }

std::string FunctionIR::print(std::shared_ptr<IrContext>) const {
    std::shared_ptr<IrContext> ctx = std::make_shared<IrContext>();
    ctx->ret = "";
    ctx->cnt = 0;
    std::string blocks_str = blocks->print(ctx);
    return std::format("fun @{}(): {} {{\n{}}}\n", name, type, blocks_str);
}

int FunctionIR::stackSize() const { return blocks->stackSize(); }

std::string ProgramIR::toString() const {
    return serializeClass("ProgramIR", global_vars, funcs);
}

std::string ProgramIR::print(std::shared_ptr<IrContext>) const {
    std::string str;
    // for (const auto& var : global_vars) {
    //     // TODO:
    // }
    for (const auto& func : funcs) {
        str += func->print();
    }
    return str;
}