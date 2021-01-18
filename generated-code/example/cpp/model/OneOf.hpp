#ifndef _MODEL_ONE_OF_HPP_
#define _MODEL_ONE_OF_HPP_

#include <memory>
#include "../Stream.hpp"
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
public:
    std::vector<int> vecInt;
    long long longInt;
    OptionOne();
    OptionOne(std::vector<int> vecInt, long long longInt);
    static OptionOne readFrom(InputStream& stream);
    void writeTo(OutputStream& stream) const override;
};

class OneOf::OptionTwo : public OneOf {
public:
    static const int TAG = 1;
public:
    int value;
    OptionTwo();
    OptionTwo(int value);
    static OptionTwo readFrom(InputStream& stream);
    void writeTo(OutputStream& stream) const override;
};

#endif
