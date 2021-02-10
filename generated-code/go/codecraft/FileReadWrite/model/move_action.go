package model

import "fmt"
import "io"
import . "trans_gen_test/common"
import . "trans_gen_test/stream"

// Move action
type MoveAction struct {
    // Target position
    Target Vec2Int32
    // Whether to try find closest position, if path to target is not found
    FindClosestPosition bool
    // Whether to destroy other entities on the way
    BreakThrough bool
}

func NewMoveAction(target Vec2Int32, findClosestPosition bool, breakThrough bool) MoveAction {
    return MoveAction {
        Target: target,
        FindClosestPosition: findClosestPosition,
        BreakThrough: breakThrough,
    }
}

// Read MoveAction from reader
func ReadMoveAction(reader io.Reader) MoveAction {
    var target Vec2Int32
    target = ReadVec2Int32(reader)
    var findClosestPosition bool
    findClosestPosition = ReadBool(reader)
    var breakThrough bool
    breakThrough = ReadBool(reader)
    return MoveAction {
        Target: target,
        FindClosestPosition: findClosestPosition,
        BreakThrough: breakThrough,
    }
}

// Write MoveAction to writer
func (moveAction MoveAction) Write(writer io.Writer) {
    target := moveAction.Target
    target.Write(writer)
    findClosestPosition := moveAction.FindClosestPosition
    WriteBool(writer, findClosestPosition)
    breakThrough := moveAction.BreakThrough
    WriteBool(writer, breakThrough)
}

// Get string representation of MoveAction
func (moveAction MoveAction) String() string {
    stringResult := "{ "
    stringResult += "Target: "
    target := moveAction.Target
    stringResult += target.String()
    stringResult += ", "
    stringResult += "FindClosestPosition: "
    findClosestPosition := moveAction.FindClosestPosition
    stringResult += fmt.Sprint(findClosestPosition)
    stringResult += ", "
    stringResult += "BreakThrough: "
    breakThrough := moveAction.BreakThrough
    stringResult += fmt.Sprint(breakThrough)
    stringResult += " }"
    return stringResult
}