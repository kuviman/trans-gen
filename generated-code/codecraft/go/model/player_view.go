package model

import "io"
import . "trans_gen_test/stream"

type PlayerView struct {
    MyId int32
    MapSize int32
    FogOfWar bool
    EntityProperties map[EntityType]EntityProperties
    MaxTickCount int32
    MaxPathfindNodes int32
    CurrentTick int32
    Players []Player
    Entities []Entity
}

func NewPlayerView(myId int32, mapSize int32, fogOfWar bool, entityProperties map[EntityType]EntityProperties, maxTickCount int32, maxPathfindNodes int32, currentTick int32, players []Player, entities []Entity) PlayerView {
    return PlayerView {
        MyId: myId,
        MapSize: mapSize,
        FogOfWar: fogOfWar,
        EntityProperties: entityProperties,
        MaxTickCount: maxTickCount,
        MaxPathfindNodes: maxPathfindNodes,
        CurrentTick: currentTick,
        Players: players,
        Entities: entities,
    }
}

func ReadPlayerView(reader io.Reader) PlayerView {
    var myId int32
    myId = ReadInt32(reader)
    var mapSize int32
    mapSize = ReadInt32(reader)
    var fogOfWar bool
    fogOfWar = ReadBool(reader)
    var entityProperties map[EntityType]EntityProperties
    entityPropertiesSize := ReadInt32(reader)
    entityProperties = make(map[EntityType]EntityProperties)
    for entityPropertiesIndex := int32(0); entityPropertiesIndex < entityPropertiesSize; entityPropertiesIndex++ {
        var entityPropertiesKey EntityType
        entityPropertiesKey = ReadEntityType(reader)
        var entityPropertiesValue EntityProperties
        entityPropertiesValue = ReadEntityProperties(reader)
        entityProperties[entityPropertiesKey] = entityPropertiesValue
    }
    var maxTickCount int32
    maxTickCount = ReadInt32(reader)
    var maxPathfindNodes int32
    maxPathfindNodes = ReadInt32(reader)
    var currentTick int32
    currentTick = ReadInt32(reader)
    var players []Player
    players = make([]Player, ReadInt32(reader))
    for playersIndex := range players {
        var playersElement Player
        playersElement = ReadPlayer(reader)
        players[playersIndex] = playersElement
    }
    var entities []Entity
    entities = make([]Entity, ReadInt32(reader))
    for entitiesIndex := range entities {
        var entitiesElement Entity
        entitiesElement = ReadEntity(reader)
        entities[entitiesIndex] = entitiesElement
    }
    return PlayerView {
        MyId: myId,
        MapSize: mapSize,
        FogOfWar: fogOfWar,
        EntityProperties: entityProperties,
        MaxTickCount: maxTickCount,
        MaxPathfindNodes: maxPathfindNodes,
        CurrentTick: currentTick,
        Players: players,
        Entities: entities,
    }
}

func (playerView PlayerView) Write(writer io.Writer) {
    myId := playerView.MyId
    WriteInt32(writer, myId)
    mapSize := playerView.MapSize
    WriteInt32(writer, mapSize)
    fogOfWar := playerView.FogOfWar
    WriteBool(writer, fogOfWar)
    entityProperties := playerView.EntityProperties
    WriteInt32(writer, int32(len(entityProperties)))
    for entityPropertiesKey, entityPropertiesValue := range entityProperties {
        WriteInt32(writer, int32(entityPropertiesKey))
        entityPropertiesValue.Write(writer)
    }
    maxTickCount := playerView.MaxTickCount
    WriteInt32(writer, maxTickCount)
    maxPathfindNodes := playerView.MaxPathfindNodes
    WriteInt32(writer, maxPathfindNodes)
    currentTick := playerView.CurrentTick
    WriteInt32(writer, currentTick)
    players := playerView.Players
    WriteInt32(writer, int32(len(players)))
    for _, playersElement := range players {
        playersElement.Write(writer)
    }
    entities := playerView.Entities
    WriteInt32(writer, int32(len(entities)))
    for _, entitiesElement := range entities {
        entitiesElement.Write(writer)
    }
}