import model;
import stream;
import std.conv;
import std.typecons : Nullable;

struct Structure {
    string text;
    float floatNumber;
    double doubleNumber;

    this(string text, float floatNumber, double doubleNumber) {
        this.text = text;
        this.floatNumber = floatNumber;
        this.doubleNumber = doubleNumber;
    }

    static Structure readFrom(Stream reader) {
        string text;
        text = reader.readString();
        float floatNumber;
        floatNumber = reader.readFloat();
        double doubleNumber;
        doubleNumber = reader.readDouble();
        return Structure(text, floatNumber, doubleNumber);
    }

    void writeTo(Stream writer) const {
        writer.write(text);
        writer.write(floatNumber);
        writer.write(doubleNumber);
    }
}