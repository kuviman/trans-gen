#ifndef __MODEL_STRUCTURE_HPP__
#define __MODEL_STRUCTURE_HPP__

#include "../Stream.hpp"
#include "Enumeration.hpp"
#include "OneOf.hpp"
#include <memory>
#include <stdexcept>
#include <string>
#include <unordered_map>
#include <vector>

class Structure {
public:
    std::shared_ptr<OneOf> oneOfOne;
    std::shared_ptr<OneOf> oneOfTwo;
    std::unordered_map<Enumeration, int> hashMap;
    std::string text;
    float floatNumber;
    double doubleNumber;

    Structure();

    Structure(std::shared_ptr<OneOf> oneOfOne, std::shared_ptr<OneOf> oneOfTwo, std::unordered_map<Enumeration, int> hashMap, std::string text, float floatNumber, double doubleNumber);

    static Structure readFrom(InputStream& stream);

    void writeTo(OutputStream& stream) const;
};

#endif