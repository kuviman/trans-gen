package common

import "fmt"
import "io"
import . "trans_gen_test/stream"

// RGBA Color
type Color struct {
    // Red component
    R float32
    // Green component
    G float32
    // Blue component
    B float32
    // Alpha (opacity) component
    A float32
}

func NewColor(r float32, g float32, b float32, a float32) Color {
    return Color {
        R: r,
        G: g,
        B: b,
        A: a,
    }
}

// Read Color from reader
func ReadColor(reader io.Reader) Color {
    var r float32
    r = ReadFloat32(reader)
    var g float32
    g = ReadFloat32(reader)
    var b float32
    b = ReadFloat32(reader)
    var a float32
    a = ReadFloat32(reader)
    return Color {
        R: r,
        G: g,
        B: b,
        A: a,
    }
}

// Write Color to writer
func (color Color) Write(writer io.Writer) {
    r := color.R
    WriteFloat32(writer, r)
    g := color.G
    WriteFloat32(writer, g)
    b := color.B
    WriteFloat32(writer, b)
    a := color.A
    WriteFloat32(writer, a)
}

// Get string representation of Color
func (color Color) String() string {
    stringResult := "{ "
    stringResult += "R: "
    r := color.R
    stringResult += fmt.Sprint(r)
    stringResult += ", "
    stringResult += "G: "
    g := color.G
    stringResult += fmt.Sprint(g)
    stringResult += ", "
    stringResult += "B: "
    b := color.B
    stringResult += fmt.Sprint(b)
    stringResult += ", "
    stringResult += "A: "
    a := color.A
    stringResult += fmt.Sprint(a)
    stringResult += " }"
    return stringResult
}