package common

import "fmt"
import "io"
import . "trans_gen_test/stream"

// Oneof example
type OneOf interface {
    // Write OneOf to writer
    Write(writer io.Writer)

    // Get string representation of OneOf
    String() string
}

// Read OneOf from reader
func ReadOneOf(reader io.Reader) OneOf {
    switch ReadInt32(reader) {
    case 0:
        return ReadOneOfOptionOne(reader)
    case 1:
        return ReadOneOfOptionTwo(reader)
    }
    panic("Unexpected tag value")
}

// First option
type OneOfOptionOne struct {
    // List of integers
    VecInt32 []int32
    // Long integer
    LongInt int64
}

func NewOneOfOptionOne(vecInt32 []int32, longInt int64) OneOfOptionOne {
    return OneOfOptionOne {
        VecInt32: vecInt32,
        LongInt: longInt,
    }
}

// Read OptionOne from reader
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

// Write OptionOne to writer
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

// Get string representation of OptionOne
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

// Second option
type OneOfOptionTwo struct {
    // usize
    Value int32
}

func NewOneOfOptionTwo(value int32) OneOfOptionTwo {
    return OneOfOptionTwo {
        Value: value,
    }
}

// Read OptionTwo from reader
func ReadOneOfOptionTwo(reader io.Reader) OneOfOptionTwo {
    var value int32
    value = ReadInt32(reader)
    return OneOfOptionTwo {
        Value: value,
    }
}

// Write OptionTwo to writer
func (oneOfOptionTwo OneOfOptionTwo) Write(writer io.Writer) {
    WriteInt32(writer, 1)
    value := oneOfOptionTwo.Value
    WriteInt32(writer, value)
}

// Get string representation of OptionTwo
func (oneOfOptionTwo OneOfOptionTwo) String() string {
    stringResult := "{ "
    stringResult += "Value: "
    value := oneOfOptionTwo.Value
    stringResult += fmt.Sprint(value)
    stringResult += " }"
    return stringResult
}