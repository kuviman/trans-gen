#include "Structure.hpp"

Structure::Structure() { }
Structure::Structure(std::shared_ptr<OneOf> oneOfOne, std::shared_ptr<OneOf> oneOfTwo, std::unordered_map<Enumeration, int> hashMap, std::string text, float floatNumber, double doubleNumber) : oneOfOne(oneOfOne), oneOfTwo(oneOfTwo), hashMap(hashMap), text(text), floatNumber(floatNumber), doubleNumber(doubleNumber) { }
Structure Structure::readFrom(InputStream& stream) {
    Structure result;
    result.oneOfOne = OneOf::readFrom(stream);
    result.oneOfTwo = OneOf::readFrom(stream);
    size_t hashMapSize = stream.readInt();
    result.hashMap = std::unordered_map<Enumeration, int>();
    result.hashMap.reserve(hashMapSize);
    for (size_t i = 0; i < hashMapSize; i++) {
        Enumeration hashMapKey;
        switch (stream.readInt()) {
        case 0:
            hashMapKey = Enumeration::VALUE_ONE;
            break;
        case 1:
            hashMapKey = Enumeration::VALUE_TWO;
            break;
        default:
            throw std::runtime_error("Unexpected tag value");
        }
        int hashMapValue;
        hashMapValue = stream.readInt();
        result.hashMap.emplace(std::make_pair(hashMapKey, hashMapValue));
    }
    result.text = stream.readString();
    result.floatNumber = stream.readFloat();
    result.doubleNumber = stream.readDouble();
    return result;
}
void Structure::writeTo(OutputStream& stream) const {
    oneOfOne->writeTo(stream);
    oneOfTwo->writeTo(stream);
    stream.write((int)(hashMap.size()));
    for (const auto& hashMapEntry : hashMap) {
        stream.write((int)(hashMapEntry.first));
        stream.write(hashMapEntry.second);
    }
    stream.write(text);
    stream.write(floatNumber);
    stream.write(doubleNumber);
}
