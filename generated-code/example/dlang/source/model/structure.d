import model;
import stream;
import std.conv;
import std.typecons : Nullable;

struct Structure {
    OneOf oneOfOne;
    OneOf oneOfTwo;
    int[Enumeration] hashMap;
    string text;
    float floatNumber;
    double doubleNumber;

    this(OneOf oneOfOne, OneOf oneOfTwo, int[Enumeration] hashMap, string text, float floatNumber, double doubleNumber) {
        this.oneOfOne = oneOfOne;
        this.oneOfTwo = oneOfTwo;
        this.hashMap = hashMap;
        this.text = text;
        this.floatNumber = floatNumber;
        this.doubleNumber = doubleNumber;
    }

    static Structure readFrom(Stream reader) {
        OneOf oneOfOne;
        oneOfOne = OneOf.readFrom(reader);
        OneOf oneOfTwo;
        oneOfTwo = OneOf.readFrom(reader);
        int[Enumeration] hashMap;
        int hashMapSize = reader.readInt();
        hashMap.clear();
        for (int hashMapIndex = 0; hashMapIndex < hashMapSize; hashMapIndex++) {
            Enumeration hashMapKey;
            int hashMapValue;
            hashMapKey = readEnumeration(reader);
            hashMapValue = reader.readInt();
            hashMap[hashMapKey] = hashMapValue;
        }
        string text;
        text = reader.readString();
        float floatNumber;
        floatNumber = reader.readFloat();
        double doubleNumber;
        doubleNumber = reader.readDouble();
        return Structure(oneOfOne, oneOfTwo, hashMap, text, floatNumber, doubleNumber);
    }

    void writeTo(Stream writer) const {
        oneOfOne.writeTo(writer);
        oneOfTwo.writeTo(writer);
        writer.write(cast(int)(hashMap.length));
        foreach (hashMapKey, hashMapValue; hashMap) {
            writer.write(cast(int)(hashMapKey));
            writer.write(hashMapValue);
        }
        writer.write(text);
        writer.write(floatNumber);
        writer.write(doubleNumber);
    }
}