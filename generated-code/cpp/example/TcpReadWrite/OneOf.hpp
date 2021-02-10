#ifndef __MODEL_ONE_OF_HPP__
#define __MODEL_ONE_OF_HPP__

#include "Stream.hpp"
#include <memory>
#include <sstream>
#include <string>
#include <vector>

// Oneof example
class OneOf {
public:
    // First option
    class OptionOne;
    // Second option
    class OptionTwo;

    // Read OneOf from input stream
    static std::shared_ptr<OneOf> readFrom(InputStream& stream);

    // Write OneOf to output stream
    virtual void writeTo(OutputStream& stream) const = 0;

    // Get string representation of OneOf
    virtual std::string toString() const = 0;
};

// First option
class OneOf::OptionOne : public OneOf {
public:
    static const int TAG = 0;

    // List of integers
    std::vector<int> vecInt;
    // Long integer
    long long longInt;

    OptionOne(std::vector<int> vecInt, long long longInt);

    // Read OptionOne from input stream
    static OptionOne readFrom(InputStream& stream);

    // Write OptionOne to output stream
    void writeTo(OutputStream& stream) const;

    // Get string representation of OptionOne
    std::string toString() const;
};

// Second option
class OneOf::OptionTwo : public OneOf {
public:
    static const int TAG = 1;

    // usize
    int value;

    OptionTwo(int value);

    // Read OptionTwo from input stream
    static OptionTwo readFrom(InputStream& stream);

    // Write OptionTwo to output stream
    void writeTo(OutputStream& stream) const;

    // Get string representation of OptionTwo
    std::string toString() const;

    bool operator ==(const OptionTwo& other) const;
};

#endif