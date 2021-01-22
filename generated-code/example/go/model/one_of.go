package model

import "io"
import . "trans_gen_test/stream"
import "fmt"

type OneOf interface {
    Write(writer io.Writer)
    String() string
}

func ReadOneOf(reader io.Reader) OneOf {
    switch ReadInt32(reader) {
    case 0:
        return ReadOneOfOptionOne(reader)
    case 1:
        return ReadOneOfOptionTwo(reader)
    }
    panic("Unexpected tag value")
}

type OneOfOptionOne struct {
    VecInt32 []int32
    LongInt int64
}

func NewOneOfOptionOne(vecInt32 []int32, longInt int64) OneOfOptionOne {
    return OneOfOptionOne {
        VecInt32: vecInt32,
        LongInt: longInt,
    }
}

func ReadOneOfOptionOne(reader io.Reader) OneOfOptionOne {
    var vecInt32 []int32
    vecInt32 = make([]int32, ReadInt32(reader))
    for vecInt32Index := range vecInt32 {
        var vecInt32Element int32
        vecInt32Element = ReadInt32(reader)
        vecInt32[vecInt32Index] = vecInt32Element
    }
    var longInt int64
    longInt = ReadInt64(reader)
    return OneOfOptionOne {
        VecInt32: vecInt32,
        LongInt: longInt,
    }
}

func (oneOfOptionOne OneOfOptionOne) Write(writer io.Writer) {
    WriteInt32(writer, 0)
    vecInt32 := oneOfOptionOne.VecInt32
    WriteInt32(writer, int32(len(vecInt32)))
    for _, vecInt32Element := range vecInt32 {
        WriteInt32(writer, vecInt32Element)
    }
    longInt := oneOfOptionOne.LongInt
    WriteInt64(writer, longInt)
}

func (oneOfOptionOne OneOfOptionOne) String() string {
    stringResult := "{ "
    stringResult += "VecInt32: "
    vecInt32 := oneOfOptionOne.VecInt32
    stringResult += "[ "
    for vecInt32Index, vecInt32Element := range vecInt32 {
        if vecInt32Index != 0 {
            stringResult += ", "
        }
        stringResult += fmt.Sprint(vecInt32Element)
    }
    stringResult += " ]"
    stringResult += ", "
    stringResult += "LongInt: "
    longInt := oneOfOptionOne.LongInt
    stringResult += fmt.Sprint(longInt)
    stringResult += " }"
    return stringResult
}

type OneOfOptionTwo struct {
    Value int32
}

func NewOneOfOptionTwo(value int32) OneOfOptionTwo {
    return OneOfOptionTwo {
        Value: value,
    }
}

func ReadOneOfOptionTwo(reader io.Reader) OneOfOptionTwo {
    var value int32
    value = ReadInt32(reader)
    return OneOfOptionTwo {
        Value: value,
    }
}

func (oneOfOptionTwo OneOfOptionTwo) Write(writer io.Writer) {
    WriteInt32(writer, 1)
    value := oneOfOptionTwo.Value
    WriteInt32(writer, value)
}

func (oneOfOptionTwo OneOfOptionTwo) String() string {
    stringResult := "{ "
    stringResult += "Value: "
    value := oneOfOptionTwo.Value
    stringResult += fmt.Sprint(value)
    stringResult += " }"
    return stringResult
}