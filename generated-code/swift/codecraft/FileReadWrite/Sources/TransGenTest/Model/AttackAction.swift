/// Attack action
public struct AttackAction {
    /// If specified, target entity's ID
    let target: Int32?

    /// If specified, configures auto attacking
    let autoAttack: AutoAttack?

    /// Read AttackAction from input stream
    static func readFrom<S: InputStream>(_ stream: S) -> AttackAction {
        var target: Int32?
        if stream.readBool() {
            target = stream.readInt32()
        } else {
            target = nil
        }
        var autoAttack: AutoAttack?
        if stream.readBool() {
            autoAttack = AutoAttack.readFrom(stream)
        } else {
            autoAttack = nil
        }
        return AttackAction(target: target, autoAttack: autoAttack)
    }

    /// Write AttackAction to output stream
    func writeTo<S: OutputStream>(_ stream: S) {
        if target == nil {
            stream.writeBool(false)
        } else {
            stream.writeBool(true)
            let targetValue = target!
            stream.writeInt32(targetValue)
        }
        if autoAttack == nil {
            stream.writeBool(false)
        } else {
            stream.writeBool(true)
            let autoAttackValue = autoAttack!
            autoAttackValue.writeTo(stream)
        }
    }
}