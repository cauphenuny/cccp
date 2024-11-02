#ifndef UTIL_HPP
#define UTIL_HPP
#include <string>
#include <sstream>
#include <cstdio>

#define eprintf(fmt, ...) fprintf(stderr, "%s:%d:%s: " fmt "\n", __FILE__, __LINE__, __func__, ## __VA_ARGS__), exit(1)

inline std::string addIndent(const std::string& str, int indent = 1)
{
    std::string indent_str;
    for (int i = 0; i < indent; i++) indent_str += "  ";
    bool indent_flag = 1;
    std::string result_str;
    for (auto i : str) {
        if (indent_flag) result_str += indent_str, indent_flag = 0;
        result_str += i;
        if (i == '\n') {
            indent_flag = 1;
        }
    }
    if (result_str.back() != '\n') result_str += '\n';
    return result_str;
}
template <typename T> std::string serialize(const std::vector<T>& vec) {
    std::string str;
    for (auto& val : vec) {
        str += serialize(val) + ",\n";
    }
    if (!str.empty()) {
        str.pop_back(), str.pop_back();
    }
    return "vector {\n" + addIndent(str) + "}";
}

template <typename T> std::string serialize(const T& val) {
    if constexpr (requires { std::string(val); }) {
        return std::string(val);
    } else if constexpr (requires { std::to_string(val); }) {
        return std::to_string(val);
    } else if constexpr (requires { val->toString(); }) {
        return val->toString();
    } else {
        static_assert(false, "can not convert to string");
    }
}

std::string serializeVar(const char* names, const auto& var, const auto&... rest) {
    std::ostringstream oss;
    const char* comma = strchr(names, ',');
    while (names[0] == ' ') names++;
    if (comma != nullptr) {
        oss.write(names, comma - names) << ": " << serialize(var) << "," << "\n";
        if constexpr (sizeof...(rest)) oss << serializeVar(comma + 1, rest...);
    } else {
        oss.write(names, strlen(names)) << ": " << serialize(var) << "\n";
    }
    return oss.str();
}

#define serializeClass(name, ...) \
    name " {\n" + addIndent(serializeVar(#__VA_ARGS__, __VA_ARGS__)) + "}"

#endif