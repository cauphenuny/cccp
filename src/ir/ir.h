#ifndef IR_HPP
#define IR_HPP

#include <cassert>
#include <variant>
#include <memory>
#include <functional>
#include <list>
#include <string>
#include <vector>

enum class Operator : unsigned {
    no,    //!
    add,   // +
    sub,   // -
    mul,   // *
    div,   // /
    mod,   // %
    eq,    // ==
    neq,   // !=
    leq,   // <=
    geq,   // >=
    lt,    // <
    gt,    // >
    band,  // &
    bor,   // |
    land,  // &&
    lor,   // ||
};

template <typename T>
std::array<std::function<T(T, T)>, 16> function_map = {
    std::not_equal_to<T>(),   // no,    // !
    std::plus<T>(),           // add,   // +
    std::minus<T>(),          // sub,   // -
    std::multiplies<T>(),     // mul,   // *
    std::divides<T>(),        // div,   // /
    std::modulus<T>(),        // mod,   // %
    std::equal_to<T>(),       // eq,    // ==
    std::not_equal_to<T>(),   // neq,   // !=
    std::less_equal<T>(),     // leq,   // <=
    std::greater_equal<T>(),  // geq,   // >=
    std::less<T>(),           // lt,    // <
    std::greater<T>(),        // gt,    // >
    std::bit_and<T>(),        // band,  // &
    std::bit_or<T>(),         // bor,   // |
    std::logical_and<T>(),    // land,  // &&
    std::logical_or<T>()      // lor,   // ||
};
/*
    note: for unary expression, the first parameter should be 0, and the second parameter is the
given parameter
    example: for a unary expression `!a`, the function should be `function_map[(size_t)no](0, a)`
*/

template <typename T> std::function<T(T, T)> getFunction(Operator op) {
    return function_map<T>[(size_t)op];
}

// clang-format off
inline std::array<std::pair<Operator, const char*>, 16> raw_map = {{
    {Operator::no, "!"},{Operator::eq, "=="}, {Operator::neq, "!="},
    {Operator::leq, "<="}, {Operator::geq, ">="}, {Operator::lt, "<"}, {Operator::gt, ">"},
    {Operator::add, "+"}, {Operator::sub, "-"}, {Operator::mul, "*"}, {Operator::div, "/"}, {Operator::mod, "%"},
    {Operator::band, "&"}, {Operator::bor, "|"}, {Operator::land, "&&"}, {Operator::lor, "||"}
}};

inline std::array<std::pair<Operator, const char*>, 13> name_map = {{
    {Operator::neq, "ne"}, {Operator::eq, "eq"},
    {Operator::leq, "le"}, {Operator::geq, "ge"}, {Operator::lt, "lt"}, {Operator::gt, "gt"},
    {Operator::add, "add"}, {Operator::sub, "sub"}, {Operator::mul, "mul"}, {Operator::div, "div"}, {Operator::mod, "mod"},
    {Operator::band, "and"}, {Operator::bor, "or"}
}};

std::string toString(const Operator& oper);
const char* toIrOperatorName(Operator oper);
Operator toOperator(const std::string& str);

enum class Inst {
    Unknown, ZeroInit, FuncArgRef, GlobalAlloc, Alloc, Load, Store,
    GetPtr, GetElemPtr, Binary, Branch, Jump, Call, Return,
    // non-instrument values
    Integer, String, Label, 
};

std::string toString(Inst t);

// clang-format on

class BaseIR {
public:
    class Type;
    class RiscvContext;
    struct IrContext;
    struct BfContext;
    std::unique_ptr<Type> type;
    BaseIR();
    virtual ~BaseIR();
    virtual std::string toString() const = 0;
    virtual std::string print(std::shared_ptr<IrContext> ctx = nullptr) const = 0;
    virtual std::string printRiscV(std::shared_ptr<RiscvContext> ctx = nullptr) const = 0;
    virtual std::string printBf(bool compress = false,
                                std::shared_ptr<BfContext> ctx = nullptr) const = 0;
    virtual int stackSize() const;
};

using IrObject = std::unique_ptr<BaseIR>;

class BaseIR::Type {
public:
    enum class Tag { Unit, Int32, Array, Pointer, Function };
    struct ArrayData {
        std::unique_ptr<Type> base;
        int size;
    };
    struct PointerData {
        std::unique_ptr<Type> base;
    };
    struct FunctionData {
        std::unique_ptr<Type> ret;
        std::vector<std::unique_ptr<Type>> args;
    };
    Tag tag;
    std::variant<std::monostate, ArrayData, PointerData, FunctionData> data;

    explicit Type(Tag t = Tag::Int32);
    std::string toString() const;
};

class ValueIR : public BaseIR {
public:
    Inst inst;
    std::string content;
    std::string comment;
    std::vector<IrObject> params;
    ValueIR() = delete;
    explicit ValueIR(Inst inst);
    explicit ValueIR(const std::string& content);
    ValueIR(Inst inst, const std::string& content);
    ValueIR(Inst inst, const std::string& content, auto&&... params);
    std::string toString() const override;
    std::string print(std::shared_ptr<IrContext> ctx) const override;
    std::string printRiscV(std::shared_ptr<RiscvContext> ctx) const override;
    std::string printBf(bool compress, std::shared_ptr<BfContext> ctx) const override;
    int stackSize() const override;

private:
    void setType();
};

ValueIR::ValueIR(Inst inst, const std::string& content, auto&&... params) {
    this->inst = inst;
    this->content = content;
    (this->params.emplace_back(std::move(params)), ...);
    setType();
}

class MultiValueIR : public BaseIR {
public:
    bool terminated = false;
    std::list<IrObject> values;

    explicit MultiValueIR(auto&&... params);

    std::string toString() const override;

    void add(IrObject&& value);
    void add(Inst inst, auto&&... params);

    std::string print(std::shared_ptr<IrContext> ctx) const override;
    std::string printRiscV(std::shared_ptr<RiscvContext> ctx) const override;
    std::string printBf(bool compress, std::shared_ptr<BfContext> ctx) const override;

    int stackSize() const override;
};

MultiValueIR::MultiValueIR(auto&&... params) {
    (this->add(std::move(params)), ...);
}

void MultiValueIR::add(Inst inst, auto&&... params) {
    add(std::make_unique<ValueIR>(inst, std::forward<decltype(params)>(params)...));
}

class FunctionIR : public BaseIR {
public:
    std::string name;
    IrObject blocks;
    explicit FunctionIR(const std::string& name);

    std::string toString() const override;
    std::string print(std::shared_ptr<IrContext>) const override;
    std::string printRiscV(std::shared_ptr<RiscvContext> ctx) const override;
    std::string printBf(bool compress, std::shared_ptr<BfContext> ctx) const override;
    int stackSize() const override;
};

class ProgramIR : public BaseIR {
public:
    std::vector<IrObject> global_vars;  // global variables
    std::vector<IrObject> funcs;
    std::string toString() const override;
    std::string print(std::shared_ptr<IrContext>) const override;
    std::string printRiscV(std::shared_ptr<RiscvContext> ctx) const override;
    std::string printBf(bool compress, std::shared_ptr<BfContext> ctx) const override;
};

#endif