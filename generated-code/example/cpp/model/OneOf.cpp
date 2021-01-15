#include "OneOf.hpp"
#include <stdexcept>

OneOf::OptionOne::OptionOne() { }
OneOf::OptionOne::OptionOne(std::vector<int> value) : value(value) { }
OneOf::OptionOne OneOf::OptionOne::readFrom(InputStream& stream) {
    OneOf::OptionOne result;
    result.value = std::vector<int>(stream.readInt());
    for (size_t i = 0; i < result.value.size(); i++) {
        result.value[i] = stream.readInt();
    }
    return result;
}
void OneOf::OptionOne::writeTo(OutputStream& stream) const {
    stream.write(TAG);
    stream.write((int)(value.size()));
    for (const int& valueElement : value) {
        stream.write(valueElement);
    }
}

OneOf::OptionTwo::OptionTwo() { }
OneOf::OptionTwo::OptionTwo(int value) : value(value) { }
OneOf::OptionTwo OneOf::OptionTwo::readFrom(InputStream& stream) {
    OneOf::OptionTwo result;
    result.value = stream.readInt();
    return result;
}
void OneOf::OptionTwo::writeTo(OutputStream& stream) const {
    stream.write(TAG);
    stream.write(value);
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
};
