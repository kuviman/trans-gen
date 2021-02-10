package model

import "fmt"
import "io"
import . "trans_gen_test/stream"

// Player's action
type Action struct {
    // New actions for entities. If entity does not get new action, if will continue to perform previously set one
    EntityActions map[int32]EntityAction
}

func NewAction(entityActions map[int32]EntityAction) Action {
    return Action {
        EntityActions: entityActions,
    }
}

// Read Action from reader
func ReadAction(reader io.Reader) Action {
    var entityActions map[int32]EntityAction
    entityActionsSize := ReadInt32(reader)
    entityActions = make(map[int32]EntityAction)
    for entityActionsIndex := int32(0); entityActionsIndex < entityActionsSize; entityActionsIndex++ {
        var entityActionsKey int32
        entityActionsKey = ReadInt32(reader)
        var entityActionsValue EntityAction
        entityActionsValue = ReadEntityAction(reader)
        entityActions[entityActionsKey] = entityActionsValue
    }
    return Action {
        EntityActions: entityActions,
    }
}

// Write Action to writer
func (action Action) Write(writer io.Writer) {
    entityActions := action.EntityActions
    WriteInt32(writer, int32(len(entityActions)))
    for entityActionsKey, entityActionsValue := range entityActions {
        WriteInt32(writer, entityActionsKey)
        entityActionsValue.Write(writer)
    }
}

// Get string representation of Action
func (action Action) String() string {
    stringResult := "{ "
    stringResult += "EntityActions: "
    entityActions := action.EntityActions
    stringResult += "map[ "
    entityActionsIndex := 0
    for entityActionsKey, entityActionsValue := range entityActions {
        if entityActionsIndex != 0 {
            stringResult += ", "
        }
        stringResult += fmt.Sprint(entityActionsKey)
        stringResult += ": "
        stringResult += entityActionsValue.String()
        entityActionsIndex++
    }
    stringResult += " ]"
    stringResult += " }"
    return stringResult
}