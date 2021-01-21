#include "Example.hpp"

Example::Example() { }

Example::Example(std::shared_ptr<OneOf> oneOf, std::unordered_map<Enumeration, int> hashMap, std::shared_ptr<int> optionalInt, std::shared_ptr<bool> optionalBool, std::shared_ptr<std::shared_ptr<OneOf>> optionalOneOf, std::shared_ptr<Structure> optionalStruct, std::shared_ptr<Enumeration> optionalEnum) : oneOf(oneOf), hashMap(hashMap), optionalInt(optionalInt), optionalBool(optionalBool), optionalOneOf(optionalOneOf), optionalStruct(optionalStruct), optionalEnum(optionalEnum) { }

Example Example::readFrom(InputStream& stream) {
    std::shared_ptr<OneOf> oneOf;
    oneOf = OneOf::readFrom(stream);
    std::unordered_map<Enumeration, int> hashMap;
    size_t hashMapSize = stream.readInt();
    hashMap = std::unordered_map<Enumeration, int>();
    hashMap.reserve(hashMapSize);
    for (size_t hashMapIndex = 0; hashMapIndex < hashMapSize; hashMapIndex++) {
        Enumeration hashMapKey;
        int hashMapValue;
        hashMapKey = readEnumeration(stream);
        hashMapValue = stream.readInt();
        hashMap.emplace(std::make_pair(hashMapKey, hashMapValue));
    }
    std::shared_ptr<int> optionalInt;
    if (stream.readBool()) {
        optionalInt = std::shared_ptr<int>(new int());
        *optionalInt = stream.readInt();
    } else {
        optionalInt = std::shared_ptr<int>();
    }
    std::shared_ptr<bool> optionalBool;
    if (stream.readBool()) {
        optionalBool = std::shared_ptr<bool>(new bool());
        *optionalBool = stream.readBool();
    } else {
        optionalBool = std::shared_ptr<bool>();
    }
    std::shared_ptr<std::shared_ptr<OneOf>> optionalOneOf;
    if (stream.readBool()) {
        optionalOneOf = std::shared_ptr<std::shared_ptr<OneOf>>(new std::shared_ptr<OneOf>());
        *optionalOneOf = OneOf::readFrom(stream);
    } else {
        optionalOneOf = std::shared_ptr<std::shared_ptr<OneOf>>();
    }
    std::shared_ptr<Structure> optionalStruct;
    if (stream.readBool()) {
        optionalStruct = std::shared_ptr<Structure>(new Structure());
        *optionalStruct = Structure::readFrom(stream);
    } else {
        optionalStruct = std::shared_ptr<Structure>();
    }
    std::shared_ptr<Enumeration> optionalEnum;
    if (stream.readBool()) {
        optionalEnum = std::shared_ptr<Enumeration>(new Enumeration());
        *optionalEnum = readEnumeration(stream);
    } else {
        optionalEnum = std::shared_ptr<Enumeration>();
    }
    return Example(oneOf, hashMap, optionalInt, optionalBool, optionalOneOf, optionalStruct, optionalEnum);
}

void Example::writeTo(OutputStream& stream) const {
    oneOf->writeTo(stream);
    stream.write((int)(hashMap.size()));
    for (const auto& hashMapEntry : hashMap) {
        const Enumeration& hashMapKey = hashMapEntry.first;
        const int& hashMapValue = hashMapEntry.second;
        stream.write((int)(hashMapKey));
        stream.write(hashMapValue);
    }
    if (optionalInt) {
        stream.write(true);
        const int& optionalIntValue = *optionalInt;
        stream.write(optionalIntValue);
    } else {
        stream.write(false);
    }
    if (optionalBool) {
        stream.write(true);
        const bool& optionalBoolValue = *optionalBool;
        stream.write(optionalBoolValue);
    } else {
        stream.write(false);
    }
    if (optionalOneOf) {
        stream.write(true);
        const std::shared_ptr<OneOf>& optionalOneOfValue = *optionalOneOf;
        optionalOneOfValue->writeTo(stream);
    } else {
        stream.write(false);
    }
    if (optionalStruct) {
        stream.write(true);
        const Structure& optionalStructValue = *optionalStruct;
        optionalStructValue.writeTo(stream);
    } else {
        stream.write(false);
    }
    if (optionalEnum) {
        stream.write(true);
        const Enumeration& optionalEnumValue = *optionalEnum;
        stream.write((int)(optionalEnumValue));
    } else {
        stream.write(false);
    }
}