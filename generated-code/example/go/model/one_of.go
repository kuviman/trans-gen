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
    Value []int32
}
func NewOneOfOptionOne(value []int32) OneOfOptionOne {
    return OneOfOptionOne {
        Value: value,
    }
}
func ReadOneOfOptionOne(reader io.Reader) OneOfOptionOne {
    result := OneOfOptionOne {}
    result.Value = make([]int32, ReadInt32(reader))
    for i := range result.Value {
        result.Value[i] = ReadInt32(reader)
    }
    return result
}
func (value OneOfOptionOne) Write(writer io.Writer) {
    WriteInt32(writer, 0)
    WriteInt32(writer, int32(len(value.Value)))
    for _, ValueElement := range value.Value {
        WriteInt32(writer, ValueElement)
    }
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
