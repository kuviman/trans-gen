#ifndef __MODEL_ENUMERATION_HPP__
#define __MODEL_ENUMERATION_HPP__

#include "../Stream.hpp"

enum class Enumeration {
        VALUE_ONE = 0,
        VALUE_TWO = 1
};

Enumeration readEnumeration(InputStream& stream);

#endif