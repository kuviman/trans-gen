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