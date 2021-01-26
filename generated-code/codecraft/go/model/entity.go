package model

import "io"
import . "trans_gen_test/stream"
import "fmt"

// Game entity
type Entity struct {
    // Entity's ID. Unique for each entity
    Id int32
    // Entity's owner player ID, if owned by a player
    PlayerId *int32
    // Entity's type
    EntityType EntityType
    // Entity's position (corner with minimal coordinates)
    Position Vec2Int32
    // Current health
    Health int32
    // If entity is active, it can perform actions
    Active bool
}

func NewEntity(id int32, playerId *int32, entityType EntityType, position Vec2Int32, health int32, active bool) Entity {
    return Entity {
        Id: id,
        PlayerId: playerId,
        EntityType: entityType,
        Position: position,
        Health: health,
        Active: active,
    }
}

// Read Entity from reader
func ReadEntity(reader io.Reader) Entity {
    var id int32
    id = ReadInt32(reader)
    var playerId *int32
    if ReadBool(reader) {
        var playerIdValue int32
        playerIdValue = ReadInt32(reader)
        playerId = &playerIdValue
    } else {
        playerId = nil
    }
    var entityType EntityType
    entityType = ReadEntityType(reader)
    var position Vec2Int32
    position = ReadVec2Int32(reader)
    var health int32
    health = ReadInt32(reader)
    var active bool
    active = ReadBool(reader)
    return Entity {
        Id: id,
        PlayerId: playerId,
        EntityType: entityType,
        Position: position,
        Health: health,
        Active: active,
    }
}

// Write Entity to writer
func (entity Entity) Write(writer io.Writer) {
    id := entity.Id
    WriteInt32(writer, id)
    playerId := entity.PlayerId
    if playerId == nil {
        WriteBool(writer, false)
    } else {
        WriteBool(writer, true)
        playerIdValue := *playerId
        WriteInt32(writer, playerIdValue)
    }
    entityType := entity.EntityType
    WriteInt32(writer, int32(entityType))
    position := entity.Position
    position.Write(writer)
    health := entity.Health
    WriteInt32(writer, health)
    active := entity.Active
    WriteBool(writer, active)
}

// Get string representation of Entity
func (entity Entity) String() string {
    stringResult := "{ "
    stringResult += "Id: "
    id := entity.Id
    stringResult += fmt.Sprint(id)
    stringResult += ", "
    stringResult += "PlayerId: "
    playerId := entity.PlayerId
    if playerId == nil {
        stringResult += "nil"
    } else {
        playerIdValue := *playerId
        stringResult += fmt.Sprint(playerIdValue)
    }
    stringResult += ", "
    stringResult += "EntityType: "
    entityType := entity.EntityType
    stringResult += EntityTypeToString(entityType)
    stringResult += ", "
    stringResult += "Position: "
    position := entity.Position
    stringResult += position.String()
    stringResult += ", "
    stringResult += "Health: "
    health := entity.Health
    stringResult += fmt.Sprint(health)
    stringResult += ", "
    stringResult += "Active: "
    active := entity.Active
    stringResult += fmt.Sprint(active)
    stringResult += " }"
    return stringResult
}