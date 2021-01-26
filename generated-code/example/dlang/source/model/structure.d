import model;
import stream;
import std.conv;
import std.typecons : Nullable;

/// Example structure
struct Structure {
    /// Text
    string text;
    /// 32-bit float
    float floatNumber;
    /// 64-bit float
    double doubleNumber;

    this(string text, float floatNumber, double doubleNumber) {
        this.text = text;
        this.floatNumber = floatNumber;
        this.doubleNumber = doubleNumber;
    }

    /// Read Structure from input stream
    static Structure readFrom(Stream reader) {
        string text;
        text = reader.readString();
        float floatNumber;
        floatNumber = reader.readFloat();
        double doubleNumber;
        doubleNumber = reader.readDouble();
        return Structure(text, floatNumber, doubleNumber);
    }

    /// Write Structure to output stream
    void writeTo(Stream writer) const {
        writer.write(text);
        writer.write(floatNumber);
        writer.write(doubleNumber);
    }
}