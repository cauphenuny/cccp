#ifndef UTIL_HPP
#define UTIL_HPP
#include <string>

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
    return result_str;
}

#endif