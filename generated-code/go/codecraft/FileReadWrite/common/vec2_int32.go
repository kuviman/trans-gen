package common

import "fmt"
import "io"
import . "trans_gen_test/stream"

// 2 dimensional vector.
type Vec2Int32 struct {
    // `x` coordinate of the vector
    X int32
    // `y` coordinate of the vector
    Y int32
}

func NewVec2Int32(x int32, y int32) Vec2Int32 {
    return Vec2Int32 {
        X: x,
        Y: y,
    }
}

// Read Vec2Int32 from reader
func ReadVec2Int32(reader io.Reader) Vec2Int32 {
    var x int32
    x = ReadInt32(reader)
    var y int32
    y = ReadInt32(reader)
    return Vec2Int32 {
        X: x,
        Y: y,
    }
}

// Write Vec2Int32 to writer
func (vec2Int32 Vec2Int32) Write(writer io.Writer) {
    x := vec2Int32.X
    WriteInt32(writer, x)
    y := vec2Int32.Y
    WriteInt32(writer, y)
}

// Get string representation of Vec2Int32
func (vec2Int32 Vec2Int32) String() string {
    stringResult := "{ "
    stringResult += "X: "
    x := vec2Int32.X
    stringResult += fmt.Sprint(x)
    stringResult += ", "
    stringResult += "Y: "
    y := vec2Int32.Y
    stringResult += fmt.Sprint(y)
    stringResult += " }"
    return stringResult
}