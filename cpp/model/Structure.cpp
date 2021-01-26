#include "Structure.hpp"

Structure::Structure() { }

Structure::Structure(std::string text, float floatNumber, double doubleNumber) : text(text), floatNumber(floatNumber), doubleNumber(doubleNumber) { }

Structure Structure::readFrom(InputStream& stream) {
    std::string text;
    text = stream.readString();
    float floatNumber;
    floatNumber = stream.readFloat();
    double doubleNumber;
    doubleNumber = stream.readDouble();
    return Structure(text, floatNumber, doubleNumber);
}

void Structure::writeTo(OutputStream& stream) const {
    stream.write(text);
    stream.write(floatNumber);
    stream.write(doubleNumber);
}

std::string Structure::toString() const {
    std::stringstream ss;
    ss << "Structure { ";
    ss << "text: ";
    ss << '"' << text << '"';
    ss << ", ";
    ss << "floatNumber: ";
    ss << floatNumber;
    ss << ", ";
    ss << "doubleNumber: ";
    ss << doubleNumber;
    ss << " }";
    return ss.str();
}