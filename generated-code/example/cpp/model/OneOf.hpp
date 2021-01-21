#ifndef __MODEL_ONE_OF_HPP__
#define __MODEL_ONE_OF_HPP__

#include "../Stream.hpp"
#include <memory>
#include <string>
#include <vector>

class OneOf {
public:
    class OptionOne;
    class OptionTwo;

    static std::shared_ptr<OneOf> readFrom(InputStream& stream);
    virtual void writeTo(OutputStream& stream) const = 0;
};

class OneOf::OptionOne : public OneOf {
public:
    static const int TAG = 0;

    std::vector<int> vecInt;
    long long longInt;

    OptionOne();

    OptionOne(std::vector<int> vecInt, long long longInt);

    static OptionOne readFrom(InputStream& stream);

    void writeTo(OutputStream& stream) const;
};

class OneOf::OptionTwo : public OneOf {
public:
    static const int TAG = 1;

    int value;

    OptionTwo();

    OptionTwo(int value);

    static OptionTwo readFrom(InputStream& stream);

    void writeTo(OutputStream& stream) const;

    bool operator ==(const OptionTwo& other) const;
};

namespace std {
    template<>
    struct hash<OneOf::OptionTwo> {
        size_t operator ()(const OneOf::OptionTwo& value) const;
    };
}

#endif