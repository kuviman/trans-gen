#ifndef __MODEL_EXAMPLE_HPP__
#define __MODEL_EXAMPLE_HPP__

#include "../Stream.hpp"
#include "Enumeration.hpp"
#include "OneOf.hpp"
#include "Structure.hpp"
#include <memory>
#include <stdexcept>
#include <string>
#include <unordered_map>
#include <vector>

class Example {
public:
    std::shared_ptr<OneOf> oneOf;
    std::unordered_map<Enumeration, int> hashMap;
    std::shared_ptr<int> optionalInt;
    std::shared_ptr<bool> optionalBool;
    std::shared_ptr<std::shared_ptr<OneOf>> optionalOneOf;
    std::shared_ptr<Structure> optionalStruct;
    std::shared_ptr<Enumeration> optionalEnum;

    Example();

    Example(std::shared_ptr<OneOf> oneOf, std::unordered_map<Enumeration, int> hashMap, std::shared_ptr<int> optionalInt, std::shared_ptr<bool> optionalBool, std::shared_ptr<std::shared_ptr<OneOf>> optionalOneOf, std::shared_ptr<Structure> optionalStruct, std::shared_ptr<Enumeration> optionalEnum);

    static Example readFrom(InputStream& stream);

    void writeTo(OutputStream& stream) const;
};

#endif