/// Entity's action
public struct EntityAction {
    /// Move action
    let moveAction: MoveAction?

    /// Build action
    let buildAction: BuildAction?

    /// Attack action
    let attackAction: AttackAction?

    /// Repair action
    let repairAction: RepairAction?

    /// Read EntityAction from input stream
    static func readFrom<S: InputStream>(_ stream: S) -> EntityAction {
        var moveAction: MoveAction?
        if stream.readBool() {
            moveAction = MoveAction.readFrom(stream)
        } else {
            moveAction = nil
        }
        var buildAction: BuildAction?
        if stream.readBool() {
            buildAction = BuildAction.readFrom(stream)
        } else {
            buildAction = nil
        }
        var attackAction: AttackAction?
        if stream.readBool() {
            attackAction = AttackAction.readFrom(stream)
        } else {
            attackAction = nil
        }
        var repairAction: RepairAction?
        if stream.readBool() {
            repairAction = RepairAction.readFrom(stream)
        } else {
            repairAction = nil
        }
        return EntityAction(moveAction: moveAction, buildAction: buildAction, attackAction: attackAction, repairAction: repairAction)
    }

    /// Write EntityAction to output stream
    func writeTo<S: OutputStream>(_ stream: S) {
        if moveAction == nil {
            stream.writeBool(false)
        } else {
            stream.writeBool(true)
            let moveActionValue = moveAction!
            moveActionValue.writeTo(stream)
        }
        if buildAction == nil {
            stream.writeBool(false)
        } else {
            stream.writeBool(true)
            let buildActionValue = buildAction!
            buildActionValue.writeTo(stream)
        }
        if attackAction == nil {
            stream.writeBool(false)
        } else {
            stream.writeBool(true)
            let attackActionValue = attackAction!
            attackActionValue.writeTo(stream)
        }
        if repairAction == nil {
            stream.writeBool(false)
        } else {
            stream.writeBool(true)
            let repairActionValue = repairAction!
            repairActionValue.writeTo(stream)
        }
    }
}