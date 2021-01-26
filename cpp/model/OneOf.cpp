#include "OneOf.hpp"
#include <stdexcept>

OneOf::OptionOne::OptionOne() { }

OneOf::OptionOne::OptionOne(std::vector<int> vecInt, long long longInt) : vecInt(vecInt), longInt(longInt) { }

OneOf::OptionOne OneOf::OptionOne::readFrom(InputStream& stream) {
    std::vector<int> vecInt;
    vecInt = std::vector<int>(stream.readInt());
    for (size_t vecIntIndex = 0; vecIntIndex < vecInt.size(); vecIntIndex++) {
        vecInt[vecIntIndex] = stream.readInt();
    }
    long long longInt;
    longInt = stream.readLongLong();
    return OneOf::OptionOne(vecInt, longInt);
}

void OneOf::OptionOne::writeTo(OutputStream& stream) const {
    stream.write(TAG);
    stream.write((int)(vecInt.size()));
    for (const int& vecIntElement : vecInt) {
        stream.write(vecIntElement);
    }
    stream.write(longInt);
}

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

OneOf::OptionTwo::OptionTwo() { }

OneOf::OptionTwo::OptionTwo(int value) : value(value) { }

OneOf::OptionTwo OneOf::OptionTwo::readFrom(InputStream& stream) {
    int value;
    value = stream.readInt();
    return OneOf::OptionTwo(value);
}

void OneOf::OptionTwo::writeTo(OutputStream& stream) const {
    stream.write(TAG);
    stream.write(value);
}

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