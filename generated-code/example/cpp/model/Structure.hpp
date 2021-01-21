#ifndef __MODEL_STRUCTURE_HPP__
#define __MODEL_STRUCTURE_HPP__

#include "../Stream.hpp"
#include <string>

class Structure {
public:
    std::string text;
    float floatNumber;
    double doubleNumber;

    Structure();

    Structure(std::string text, float floatNumber, double doubleNumber);

    static Structure readFrom(InputStream& stream);

    void writeTo(OutputStream& stream) const;
};

#endif