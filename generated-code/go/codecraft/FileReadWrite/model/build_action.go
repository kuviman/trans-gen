package model

import "io"
import . "trans_gen_test/common"
import . "trans_gen_test/stream"

// Build action
type BuildAction struct {
    // Type of an entity to build
    EntityType EntityType
    // Desired position of new entity
    Position Vec2Int32
}

func NewBuildAction(entityType EntityType, position Vec2Int32) BuildAction {
    return BuildAction {
        EntityType: entityType,
        Position: position,
    }
}

// Read BuildAction from reader
func ReadBuildAction(reader io.Reader) BuildAction {
    var entityType EntityType
    entityType = ReadEntityType(reader)
    var position Vec2Int32
    position = ReadVec2Int32(reader)
    return BuildAction {
        EntityType: entityType,
        Position: position,
    }
}

// Write BuildAction to writer
func (buildAction BuildAction) Write(writer io.Writer) {
    entityType := buildAction.EntityType
    WriteInt32(writer, int32(entityType))
    position := buildAction.Position
    position.Write(writer)
}

// Get string representation of BuildAction
func (buildAction BuildAction) String() string {
    stringResult := "{ "
    stringResult += "EntityType: "
    entityType := buildAction.EntityType
    stringResult += EntityTypeToString(entityType)
    stringResult += ", "
    stringResult += "Position: "
    position := buildAction.Position
    stringResult += position.String()
    stringResult += " }"
    return stringResult
}