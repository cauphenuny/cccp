#ifndef UTIL_HPP
#define UTIL_HPP
#include <cstring>
#include <filesystem>
#include <map>
#include <format>
#include <sstream>
#include <string>
#include <source_location>
#include <string_view>
#include <vector>

inline std::string addIndent(std::string_view str, int indent = 1) {
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

inline std::string compressStr(std::string_view str) {
    std::string ret;
    char prev = ' ';
    for (auto i : str) {
        if (!isspace(i))
            ret += i;
        else if (!isspace(prev))
            ret += ' ';
        prev = i;
    }
    return ret;
}

inline std::string tryCompressStr(std::string_view str) {
    if (str.length() < 80)
        return compressStr(str);
    else
        return std::string(str);
}

std::string serialize(const auto& val);

template <std::input_iterator T> std::string toString(const T& begin, const T& end) {
    std::string str;
    for (T it = begin; it != end; it++) {
        str += serialize(*it) + ",\n";
    }
    if (!str.empty()) {
        str.pop_back(), str.pop_back();
    }
    return tryCompressStr(str);
}

template <typename T1, typename T2> std::string toString(const std::pair<T1, T2>& p) {
    return std::format("{}: {}", serialize(p.first), serialize(p.second));
}

template <typename T> std::string toString(const std::vector<T>& vec) {
    return tryCompressStr("[\n" + addIndent(toString(vec.begin(), vec.end())) + "]");
}

template <typename K, typename V> std::string toString(const std::map<K, V>& m) {
    return tryCompressStr("[\n" + addIndent(toString(m.begin(), m.end())) + "]");
}

std::string serialize(const auto& val) {
    if constexpr (requires { std::string(val); }) {
        return "\"" + std::string(val) + "\"";
    } else if constexpr (requires { toString(val); }) {
        return toString(val);
    } else if constexpr (requires { val.toString(); }) {
        return val.toString();
    } else if constexpr (requires { val->toString(); }) {
        if (val) return val->toString();
        else return "nullptr";
    } else if constexpr (requires { std::to_string(val); }) {
        return std::to_string(val);
    } else {
        static_assert(false, "can not convert to string");
    }
}

template <typename T>
concept has_custom_tostring = requires(T t) {
    { t.toString() } -> std::convertible_to<std::string>;
} || requires(T t) {
    { toString(t) } -> std::convertible_to<std::string>;
};
template <has_custom_tostring T> struct std::formatter<T> : std::formatter<std::string> {
    auto format(const T& t, std::format_context& ctx) const {
        return std::formatter<std::string>::format(serialize(t), ctx);
    }
};

template <typename T> struct std::formatter<std::unique_ptr<T>> : std::formatter<std::string> {
    auto format(const std::unique_ptr<T>& ptr, std::format_context& ctx) const {
        if (ptr) {
            return std::formatter<std::string>::format(serialize(ptr), ctx);
        } else {
            return std::formatter<std::string>::format("nullptr", ctx);
        }
    }
};

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
    tryCompressStr(name " {\n" + addIndent(serializeVar(#__VA_ARGS__, __VA_ARGS__)) + "}")

inline std::string getLocation(std::source_location location = std::source_location::current()) {
    return std::format("{}:{} `{}`", std::filesystem::path(location.file_name()).filename().string(),
                  location.line(), location.function_name());
}

#define RED      "\033[0;31m"
#define L_RED    "\033[1;31m"
#define GREEN    "\033[0;32m"
#define L_GREEN  "\033[1;32m"
#define YELLOW   "\033[0;33m"
#define L_YELLOW "\033[1;33m"
#define BLUE     "\033[0;34m"
#define L_BLUE   "\033[1;34m"
#define PURPLE   "\033[0;35m"
#define L_PURPLE "\033[1;35m"
#define CYAN     "\033[0;36m"
#define L_CYAN   "\033[1;36m"
#define DARK     "\033[2m"
#define RESET    "\033[0m"

#define addLocation(...) \
    "{}{}:{}\n{}", DARK, getLocation(), RESET, addIndent(std::format(__VA_ARGS__), 2)

#define runtimeError(...) std::runtime_error(std::format(addLocation(__VA_ARGS__)))

#define compileError(...) std::logic_error(std::format(addLocation(__VA_ARGS__)))

#ifdef DEBUG
#    define debugLog(...) std::cerr << std::format(addLocation(__VA_ARGS__))
#else
#    define debugLog(...) (void)0
#endif

#endif
