/// Player's action
public struct Action {
    /// New actions for entities. If entity does not get new action, if will continue to perform previously set one
    let entityActions: [Int32: EntityAction]

    /// Read Action from input stream
    static func readFrom<S: InputStream>(_ stream: S) -> Action {
        var entityActions: [Int32: EntityAction]
        let entityActionsSize = stream.readInt32()
        entityActions = [:]
        for _ in 0..<entityActionsSize {
            let entityActionsKey: Int32
            let entityActionsValue: EntityAction
            entityActionsKey = stream.readInt32()
            entityActionsValue = EntityAction.readFrom(stream)
            entityActions[entityActionsKey] = entityActionsValue
        }
        return Action(entityActions: entityActions)
    }

    /// Write Action to output stream
    func writeTo<S: OutputStream>(_ stream: S) {
        stream.writeInt32(Int32(entityActions.count))
        for (entityActionsKey, entityActionsValue) in entityActions {
            stream.writeInt32(entityActionsKey)
            entityActionsValue.writeTo(stream)
        }
    }
}