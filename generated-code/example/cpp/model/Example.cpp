#include "Example.hpp"

Example::Example() { }

Example::Example(std::shared_ptr<OneOf> oneOf, std::unordered_map<Enumeration, int> hashMap, std::optional<int> optionalInt, std::optional<bool> optionalBool, std::optional<std::shared_ptr<OneOf>> optionalOneOf, std::optional<Structure> optionalStruct, std::optional<Enumeration> optionalEnum) : oneOf(oneOf), hashMap(hashMap), optionalInt(optionalInt), optionalBool(optionalBool), optionalOneOf(optionalOneOf), optionalStruct(optionalStruct), optionalEnum(optionalEnum) { }

// Read Example from input stream
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
    std::optional<int> optionalInt;
    if (stream.readBool()) {
        int optionalIntValue;
        optionalIntValue = stream.readInt();
        optionalInt = optionalIntValue;
    } else {
        optionalInt = std::optional<int>();
    }
    std::optional<bool> optionalBool;
    if (stream.readBool()) {
        bool optionalBoolValue;
        optionalBoolValue = stream.readBool();
        optionalBool = optionalBoolValue;
    } else {
        optionalBool = std::optional<bool>();
    }
    std::optional<std::shared_ptr<OneOf>> optionalOneOf;
    if (stream.readBool()) {
        std::shared_ptr<OneOf> optionalOneOfValue;
        optionalOneOfValue = OneOf::readFrom(stream);
        optionalOneOf = optionalOneOfValue;
    } else {
        optionalOneOf = std::optional<std::shared_ptr<OneOf>>();
    }
    std::optional<Structure> optionalStruct;
    if (stream.readBool()) {
        Structure optionalStructValue;
        optionalStructValue = Structure::readFrom(stream);
        optionalStruct = optionalStructValue;
    } else {
        optionalStruct = std::optional<Structure>();
    }
    std::optional<Enumeration> optionalEnum;
    if (stream.readBool()) {
        Enumeration optionalEnumValue;
        optionalEnumValue = readEnumeration(stream);
        optionalEnum = optionalEnumValue;
    } else {
        optionalEnum = std::optional<Enumeration>();
    }
    return Example(oneOf, hashMap, optionalInt, optionalBool, optionalOneOf, optionalStruct, optionalEnum);
}

// Write Example to output stream
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

// Get string representation of Example
std::string Example::toString() const {
    std::stringstream ss;
    ss << "Example { ";
    ss << "oneOf: ";
    ss << oneOf->toString();
    ss << ", ";
    ss << "hashMap: ";
    ss << "{ ";
    size_t hashMapIndex = 0;
    for (const auto& hashMapEntry : hashMap) {
        if (hashMapIndex != 0) {
            ss << ", ";
        }
        const Enumeration& hashMapKey = hashMapEntry.first;
        const int& hashMapValue = hashMapEntry.second;
        ss << enumerationToString(hashMapKey);
        ss << ": ";
        ss << hashMapValue;
        hashMapIndex++;
    }
    ss << " }";
    ss << ", ";
    ss << "optionalInt: ";
    if (optionalInt) {
        const int& optionalIntValue = *optionalInt;
        ss << optionalIntValue;
    } else {
        ss << "none";
    }
    ss << ", ";
    ss << "optionalBool: ";
    if (optionalBool) {
        const bool& optionalBoolValue = *optionalBool;
        ss << optionalBoolValue;
    } else {
        ss << "none";
    }
    ss << ", ";
    ss << "optionalOneOf: ";
    if (optionalOneOf) {
        const std::shared_ptr<OneOf>& optionalOneOfValue = *optionalOneOf;
        ss << optionalOneOfValue->toString();
    } else {
        ss << "none";
    }
    ss << ", ";
    ss << "optionalStruct: ";
    if (optionalStruct) {
        const Structure& optionalStructValue = *optionalStruct;
        ss << optionalStructValue.toString();
    } else {
        ss << "none";
    }
    ss << ", ";
    ss << "optionalEnum: ";
    if (optionalEnum) {
        const Enumeration& optionalEnumValue = *optionalEnum;
        ss << enumerationToString(optionalEnumValue);
    } else {
        ss << "none";
    }
    ss << " }";
    return ss.str();
}