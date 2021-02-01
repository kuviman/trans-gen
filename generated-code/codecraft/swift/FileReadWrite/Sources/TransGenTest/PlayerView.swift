/// Information available to the player
public struct PlayerView {
    /// Your player's ID
    let myId: Int32

    /// Size of the map
    let mapSize: Int32

    /// Whether fog of war is enabled
    let fogOfWar: Bool

    /// Entity properties for each entity type
    let entityProperties: [EntityType: EntityProperties]

    /// Max tick count for the game
    let maxTickCount: Int32

    /// Max pathfind nodes when performing pathfinding in the game simulator
    let maxPathfindNodes: Int32

    /// Current tick
    let currentTick: Int32

    /// List of players
    let players: [Player]

    /// List of entities
    let entities: [Entity]

    /// Read PlayerView from input stream
    static func readFrom<S: InputStream>(_ stream: S) -> PlayerView {
        var myId: Int32
        myId = stream.readInt32()
        var mapSize: Int32
        mapSize = stream.readInt32()
        var fogOfWar: Bool
        fogOfWar = stream.readBool()
        var entityProperties: [EntityType: EntityProperties]
        let entityPropertiesSize = stream.readInt32()
        entityProperties = [:]
        for _ in 0..<entityPropertiesSize {
            let entityPropertiesKey: EntityType
            let entityPropertiesValue: EntityProperties
            entityPropertiesKey = EntityType.readFrom(stream)
            entityPropertiesValue = EntityProperties.readFrom(stream)
            entityProperties[entityPropertiesKey] = entityPropertiesValue
        }
        var maxTickCount: Int32
        maxTickCount = stream.readInt32()
        var maxPathfindNodes: Int32
        maxPathfindNodes = stream.readInt32()
        var currentTick: Int32
        currentTick = stream.readInt32()
        var players: [Player]
        let playersSize = stream.readInt32()
        players = (0..<playersSize).map{ _ in
            var playersSize: Player
            playersSize = Player.readFrom(stream)
            return playersSize
        }
        var entities: [Entity]
        let entitiesSize = stream.readInt32()
        entities = (0..<entitiesSize).map{ _ in
            var entitiesSize: Entity
            entitiesSize = Entity.readFrom(stream)
            return entitiesSize
        }
        return PlayerView(myId: myId, mapSize: mapSize, fogOfWar: fogOfWar, entityProperties: entityProperties, maxTickCount: maxTickCount, maxPathfindNodes: maxPathfindNodes, currentTick: currentTick, players: players, entities: entities)
    }

    /// Write PlayerView to output stream
    func writeTo<S: OutputStream>(_ stream: S) {
        stream.writeInt32(myId)
        stream.writeInt32(mapSize)
        stream.writeBool(fogOfWar)
        stream.writeInt32(Int32(entityProperties.count))
        for (entityPropertiesKey, entityPropertiesValue) in entityProperties {
            entityPropertiesKey.writeTo(stream)
            entityPropertiesValue.writeTo(stream)
        }
        stream.writeInt32(maxTickCount)
        stream.writeInt32(maxPathfindNodes)
        stream.writeInt32(currentTick)
        stream.writeInt32(Int32(players.count))
        for playersElement in players {
            playersElement.writeTo(stream)
        }
        stream.writeInt32(Int32(entities.count))
        for entitiesElement in entities {
            entitiesElement.writeTo(stream)
        }
    }
}