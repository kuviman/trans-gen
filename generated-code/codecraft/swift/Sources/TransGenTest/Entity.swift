/// Game entity
public struct Entity {
    /// Entity's ID. Unique for each entity
    let id: Int32

    /// Entity's owner player ID, if owned by a player
    let playerId: Int32?

    /// Entity's type
    let entityType: EntityType

    /// Entity's position (corner with minimal coordinates)
    let position: Vec2Int

    /// Current health
    let health: Int32

    /// If entity is active, it can perform actions
    let active: Bool

    /// Read Entity from input stream
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

    /// Write Entity to output stream
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