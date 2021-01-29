#ifndef __MODEL_EXAMPLE_HPP__
#define __MODEL_EXAMPLE_HPP__

#include "../Stream.hpp"
#include "Enumeration.hpp"
#include "OneOf.hpp"
#include "Structure.hpp"
#include <memory>
#include <optional>
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
    std::optional<int> optionalInt;
    // Optional boolean
    std::optional<bool> optionalBool;
    // Optional OneOf
    std::optional<std::shared_ptr<OneOf>> optionalOneOf;
    // Optional struct
    std::optional<Structure> optionalStruct;
    // Optional enum
    std::optional<Enumeration> optionalEnum;

    Example();

    Example(std::shared_ptr<OneOf> oneOf, std::unordered_map<Enumeration, int> hashMap, std::optional<int> optionalInt, std::optional<bool> optionalBool, std::optional<std::shared_ptr<OneOf>> optionalOneOf, std::optional<Structure> optionalStruct, std::optional<Enumeration> optionalEnum);

    // Read Example from input stream
    static Example readFrom(InputStream& stream);

    // Write Example to output stream
    void writeTo(OutputStream& stream) const;

    // Get string representation of Example
    std::string toString() const;
};

#endif