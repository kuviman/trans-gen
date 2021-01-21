#include "Structure.hpp"

Structure::Structure() { }

Structure::Structure(std::shared_ptr<OneOf> oneOfOne, std::shared_ptr<OneOf> oneOfTwo, std::unordered_map<Enumeration, int> hashMap, std::string text, float floatNumber, double doubleNumber) : oneOfOne(oneOfOne), oneOfTwo(oneOfTwo), hashMap(hashMap), text(text), floatNumber(floatNumber), doubleNumber(doubleNumber) { }

Structure Structure::readFrom(InputStream& stream) {
    std::shared_ptr<OneOf> oneOfOne;
    oneOfOne = OneOf::readFrom(stream);
    std::shared_ptr<OneOf> oneOfTwo;
    oneOfTwo = OneOf::readFrom(stream);
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
    std::string text;
    text = stream.readString();
    float floatNumber;
    floatNumber = stream.readFloat();
    double doubleNumber;
    doubleNumber = stream.readDouble();
    return Structure(oneOfOne, oneOfTwo, hashMap, text, floatNumber, doubleNumber);
}

void Structure::writeTo(OutputStream& stream) const {
    oneOfOne->writeTo(stream);
    oneOfTwo->writeTo(stream);
    stream.write((int)(hashMap.size()));
    for (const auto& hashMapEntry : hashMap) {
        const Enumeration& hashMapKey = hashMapEntry.first;
        const int& hashMapValue = hashMapEntry.second;
        stream.write((int)(hashMapKey));
        stream.write(hashMapValue);
    }
    stream.write(text);
    stream.write(floatNumber);
    stream.write(doubleNumber);
}