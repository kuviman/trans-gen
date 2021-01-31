#include "OneOf.hpp"
#include <stdexcept>

OneOf::OptionOne::OptionOne(std::vector<int> vecInt, long long longInt) : vecInt(vecInt), longInt(longInt) { }

// Read OptionOne from input stream
OneOf::OptionOne OneOf::OptionOne::readFrom(InputStream& stream) {
    std::vector<int> vecInt = std::vector<int>();
    size_t vecIntSize = stream.readInt();
    vecInt.reserve(vecIntSize);
    for (size_t vecIntIndex = 0; vecIntIndex < vecIntSize; vecIntIndex++) {
        int vecIntElement = stream.readInt();
        vecInt.emplace_back(vecIntElement);
    }
    long long longInt = stream.readLongLong();
    return OneOf::OptionOne(vecInt, longInt);
}

// Write OptionOne to output stream
void OneOf::OptionOne::writeTo(OutputStream& stream) const {
    stream.write(TAG);
    stream.write((int)(vecInt.size()));
    for (const int& vecIntElement : vecInt) {
        stream.write(vecIntElement);
    }
    stream.write(longInt);
}

// Get string representation of OptionOne
std::string OneOf::OptionOne::toString() const {
    std::stringstream ss;
    ss << "OneOf::OptionOne { ";
    ss << "vecInt: ";
    ss << "[ ";
    for (size_t vecIntIndex = 0; vecIntIndex < vecInt.size(); vecIntIndex++) {
        const int& vecIntElement = vecInt[vecIntIndex];
        if (vecIntIndex != 0) {
            ss << ", ";
        }
        ss << vecIntElement;
    }
    ss << " ]";
    ss << ", ";
    ss << "longInt: ";
    ss << longInt;
    ss << " }";
    return ss.str();
}

OneOf::OptionTwo::OptionTwo(int value) : value(value) { }

// Read OptionTwo from input stream
OneOf::OptionTwo OneOf::OptionTwo::readFrom(InputStream& stream) {
    int value = stream.readInt();
    return OneOf::OptionTwo(value);
}

// Write OptionTwo to output stream
void OneOf::OptionTwo::writeTo(OutputStream& stream) const {
    stream.write(TAG);
    stream.write(value);
}

// Get string representation of OptionTwo
std::string OneOf::OptionTwo::toString() const {
    std::stringstream ss;
    ss << "OneOf::OptionTwo { ";
    ss << "value: ";
    ss << value;
    ss << " }";
    return ss.str();
}

bool OneOf::OptionTwo::operator ==(const OneOf::OptionTwo& other) const {
    return value == other.value;
}

size_t std::hash<OneOf::OptionTwo>::operator ()(const OneOf::OptionTwo& value) const {
    size_t result = 0;
    result ^= std::hash<int>{}(value.value) + 0x9e3779b9 + (result << 6) + (result >> 2);
    return result;
}

// Read OneOf from input stream
std::shared_ptr<OneOf> OneOf::readFrom(InputStream& stream) {
    switch (stream.readInt()) {
    case 0:
        return std::shared_ptr<OneOf::OptionOne>(new OneOf::OptionOne(OneOf::OptionOne::readFrom(stream)));
    case 1:
        return std::shared_ptr<OneOf::OptionTwo>(new OneOf::OptionTwo(OneOf::OptionTwo::readFrom(stream)));
    default:
        throw std::runtime_error("Unexpected tag value");
    }
}