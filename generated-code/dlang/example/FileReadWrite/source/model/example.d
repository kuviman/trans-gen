import model;
import stream;
import std.conv;
import std.typecons : Nullable;

/// Example
struct Example {
    /// OneOf
    OneOf oneOf;
    /// Dictionary
    int[Enumeration] hashMap;
    /// Optional int
    Nullable!(int) optionalInt;
    /// Optional boolean
    Nullable!(bool) optionalBool;
    /// Optional OneOf
    Nullable!(OneOf) optionalOneOf;
    /// Optional struct
    Nullable!(Structure) optionalStruct;
    /// Optional enum
    Nullable!(Enumeration) optionalEnum;

    this(OneOf oneOf, int[Enumeration] hashMap, Nullable!(int) optionalInt, Nullable!(bool) optionalBool, Nullable!(OneOf) optionalOneOf, Nullable!(Structure) optionalStruct, Nullable!(Enumeration) optionalEnum) {
        this.oneOf = oneOf;
        this.hashMap = hashMap;
        this.optionalInt = optionalInt;
        this.optionalBool = optionalBool;
        this.optionalOneOf = optionalOneOf;
        this.optionalStruct = optionalStruct;
        this.optionalEnum = optionalEnum;
    }

    /// Read Example from reader
    static Example readFrom(Stream reader) {
        OneOf oneOf;
        oneOf = OneOf.readFrom(reader);
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
        Nullable!(int) optionalInt;
        if (reader.readBool()) {
            optionalInt = reader.readInt();
        } else {
            optionalInt.nullify();
        }
        Nullable!(bool) optionalBool;
        if (reader.readBool()) {
            optionalBool = reader.readBool();
        } else {
            optionalBool.nullify();
        }
        Nullable!(OneOf) optionalOneOf;
        if (reader.readBool()) {
            optionalOneOf = OneOf.readFrom(reader);
        } else {
            optionalOneOf.nullify();
        }
        Nullable!(Structure) optionalStruct;
        if (reader.readBool()) {
            optionalStruct = Structure.readFrom(reader);
        } else {
            optionalStruct.nullify();
        }
        Nullable!(Enumeration) optionalEnum;
        if (reader.readBool()) {
            optionalEnum = readEnumeration(reader);
        } else {
            optionalEnum.nullify();
        }
        return Example(oneOf, hashMap, optionalInt, optionalBool, optionalOneOf, optionalStruct, optionalEnum);
    }

    /// Write Example to writer
    void writeTo(Stream writer) const {
        oneOf.writeTo(writer);
        writer.write(cast(int)(hashMap.length));
        foreach (hashMapKey, hashMapValue; hashMap) {
            writer.write(cast(int)(hashMapKey));
            writer.write(hashMapValue);
        }
        if (optionalInt.isNull()) {
            writer.write(false);
        } else {
            writer.write(true);
            writer.write(optionalInt.get);
        }
        if (optionalBool.isNull()) {
            writer.write(false);
        } else {
            writer.write(true);
            writer.write(optionalBool.get);
        }
        if (optionalOneOf.isNull()) {
            writer.write(false);
        } else {
            writer.write(true);
            optionalOneOf.get.writeTo(writer);
        }
        if (optionalStruct.isNull()) {
            writer.write(false);
        } else {
            writer.write(true);
            optionalStruct.get.writeTo(writer);
        }
        if (optionalEnum.isNull()) {
            writer.write(false);
        } else {
            writer.write(true);
            writer.write(cast(int)(optionalEnum.get));
        }
    }
}