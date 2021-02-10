/// Build action
public struct BuildAction {
    /// Type of an entity to build
    let entityType: EntityType

    /// Desired position of new entity
    let position: Vec2Int

    /// Read BuildAction from input stream
    static func readFrom<S: InputStream>(_ stream: S) -> BuildAction {
        var entityType: EntityType
        entityType = EntityType.readFrom(stream)
        var position: Vec2Int
        position = Vec2Int.readFrom(stream)
        return BuildAction(entityType: entityType, position: position)
    }

    /// Write BuildAction to output stream
    func writeTo<S: OutputStream>(_ stream: S) {
        entityType.writeTo(stream)
        position.writeTo(stream)
    }
}