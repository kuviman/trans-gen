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
        auto result = Structure();
        result.oneOfOne = OneOf.readFrom(reader);
        result.oneOfTwo = OneOf.readFrom(reader);
        int hashMapSize = reader.readInt();
        result.hashMap.clear();
        for (int i = 0; i < hashMapSize; i++) {
            Enumeration hashMapKey;
            switch (reader.readInt()) {
            case 0:
                hashMapKey = Enumeration.ValueOne;
                break;
            case 1:
                hashMapKey = Enumeration.ValueTwo;
                break;
            default:
                throw new Exception("Unexpected tag value");
            }
            int hashMapValue;
            hashMapValue = reader.readInt();
            result.hashMap[hashMapKey] = hashMapValue;
        }
        result.text = reader.readString();
        result.floatNumber = reader.readFloat();
        result.doubleNumber = reader.readDouble();
        return result;
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
    string toString() const {
        return "Structure" ~ "(" ~
            to!string(oneOfOne) ~
            to!string(oneOfTwo) ~
            to!string(hashMap) ~
            to!string(text) ~
            to!string(floatNumber) ~
            to!string(doubleNumber) ~
            ")";
    }
}
