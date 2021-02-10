package common

import "fmt"
import "io"
import . "trans_gen_test/stream"

// 2 dimensional vector.
type Vec2Float32 struct {
    // `x` coordinate of the vector
    X float32
    // `y` coordinate of the vector
    Y float32
}

func NewVec2Float32(x float32, y float32) Vec2Float32 {
    return Vec2Float32 {
        X: x,
        Y: y,
    }
}

// Read Vec2Float32 from reader
func ReadVec2Float32(reader io.Reader) Vec2Float32 {
    var x float32
    x = ReadFloat32(reader)
    var y float32
    y = ReadFloat32(reader)
    return Vec2Float32 {
        X: x,
        Y: y,
    }
}

// Write Vec2Float32 to writer
func (vec2Float32 Vec2Float32) Write(writer io.Writer) {
    x := vec2Float32.X
    WriteFloat32(writer, x)
    y := vec2Float32.Y
    WriteFloat32(writer, y)
}

// Get string representation of Vec2Float32
func (vec2Float32 Vec2Float32) String() string {
    stringResult := "{ "
    stringResult += "X: "
    x := vec2Float32.X
    stringResult += fmt.Sprint(x)
    stringResult += ", "
    stringResult += "Y: "
    y := vec2Float32.Y
    stringResult += fmt.Sprint(y)
    stringResult += " }"
    return stringResult
}