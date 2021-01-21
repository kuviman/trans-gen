import stream;

enum Enumeration : int {
    ValueOne = 0,
    ValueTwo = 1,
}

Enumeration readEnumeration(Stream reader) {
    switch (reader.readInt()) {
        case Enumeration.ValueOne:
            return Enumeration.ValueOne;
        case Enumeration.ValueTwo:
            return Enumeration.ValueTwo;
        default:
            throw new Exception("Unexpected tag value");
    }
}