package model

import "io"
import . "trans_gen_test/stream"

type Vec2Int32 struct {
    X int32
    Y int32
}

func NewVec2Int32(x int32, y int32) Vec2Int32 {
    return Vec2Int32 {
        X: x,
        Y: y,
    }
}

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

func (vec2Int32 Vec2Int32) Write(writer io.Writer) {
    x := vec2Int32.X
    WriteInt32(writer, x)
    y := vec2Int32.Y
    WriteInt32(writer, y)
}