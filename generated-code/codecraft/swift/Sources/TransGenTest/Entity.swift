public struct Entity {
    let id: Int32
    let playerId: Int32?
    let entityType: EntityType
    let position: Vec2Int
    let health: Int32
    let active: Bool

    static func readFrom<S: InputStream>(_ stream: S) -> Entity {
        var id: Int32
        id = stream.readInt32()
        var playerId: Int32?
        if stream.readBool() {
            playerId = stream.readInt32()
        } else {
            playerId = nil
        }
        var entityType: EntityType
        entityType = EntityType.readFrom(stream)
        var position: Vec2Int
        position = Vec2Int.readFrom(stream)
        var health: Int32
        health = stream.readInt32()
        var active: Bool
        active = stream.readBool()
        return Entity(id: id, playerId: playerId, entityType: entityType, position: position, health: health, active: active)
    }

    func writeTo<S: OutputStream>(_ stream: S) {
        stream.writeInt32(id)
        if playerId == nil {
            stream.writeBool(false)
        } else {
            stream.writeBool(true)
            let playerIdValue = playerId!
            stream.writeInt32(playerIdValue)
        }
        entityType.writeTo(stream)
        position.writeTo(stream)
        stream.writeInt32(health)
        stream.writeBool(active)
    }
}