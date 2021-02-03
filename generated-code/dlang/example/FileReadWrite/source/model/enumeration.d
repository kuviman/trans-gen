import stream;

/// Example enumeration
enum Enumeration : int {
    /// First option
    ValueOne = 0,
    /// Second option
    ValueTwo = 1,
}

/// Read Enumeration from reader
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