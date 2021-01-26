#include "Enumeration.hpp"
#include <stdexcept>

Enumeration readEnumeration(InputStream& stream) {
    switch (stream.readInt()) {
    case 0:
        return Enumeration::VALUE_ONE;
    case 1:
        return Enumeration::VALUE_TWO;
    default:
        throw std::runtime_error("Unexpected tag value");
    }
}

std::string enumerationToString(Enumeration value) {
    switch (value) {
    case Enumeration::VALUE_ONE:
        return "VALUE_ONE";
    case Enumeration::VALUE_TWO:
        return "VALUE_TWO";
    default:
        throw std::runtime_error("Impossible happened");
    }
}