package model

import "io"
import . "trans_gen_test/stream"

type OneOf interface {
    Write(writer io.Writer)
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
    result := OneOfOptionOne {}
    result.VecInt32 = make([]int32, ReadInt32(reader))
    for i := range result.VecInt32 {
        result.VecInt32[i] = ReadInt32(reader)
    }
    result.LongInt = ReadInt64(reader)
    return result
}
func (value OneOfOptionOne) Write(writer io.Writer) {
    WriteInt32(writer, 0)
    WriteInt32(writer, int32(len(value.VecInt32)))
    for _, VecInt32Element := range value.VecInt32 {
        WriteInt32(writer, VecInt32Element)
    }
    WriteInt64(writer, value.LongInt)
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
    result := OneOfOptionTwo {}
    result.Value = ReadInt32(reader)
    return result
}
func (value OneOfOptionTwo) Write(writer io.Writer) {
    WriteInt32(writer, 1)
    WriteInt32(writer, value.Value)
}
