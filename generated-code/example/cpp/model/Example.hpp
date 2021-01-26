#ifndef __MODEL_EXAMPLE_HPP__
#define __MODEL_EXAMPLE_HPP__

#include "../Stream.hpp"
#include "Enumeration.hpp"
#include "OneOf.hpp"
#include "Structure.hpp"
#include <memory>
#include <sstream>
#include <stdexcept>
#include <string>
#include <unordered_map>
#include <vector>

// Example
class Example {
public:
    // OneOf
    std::shared_ptr<OneOf> oneOf;
    // Dictionary
    std::unordered_map<Enumeration, int> hashMap;
    // Optional int
    std::shared_ptr<int> optionalInt;
    // Optional boolean
    std::shared_ptr<bool> optionalBool;
    // Optional OneOf
    std::shared_ptr<std::shared_ptr<OneOf>> optionalOneOf;
    // Optional struct
    std::shared_ptr<Structure> optionalStruct;
    // Optional enum
    std::shared_ptr<Enumeration> optionalEnum;

    Example();

    Example(std::shared_ptr<OneOf> oneOf, std::unordered_map<Enumeration, int> hashMap, std::shared_ptr<int> optionalInt, std::shared_ptr<bool> optionalBool, std::shared_ptr<std::shared_ptr<OneOf>> optionalOneOf, std::shared_ptr<Structure> optionalStruct, std::shared_ptr<Enumeration> optionalEnum);

    // Read Example from input stream
    static Example readFrom(InputStream& stream);

    // Write Example to output stream
    void writeTo(OutputStream& stream) const;

    // Get string representation of Example
    std::string toString() const;
};

#endif