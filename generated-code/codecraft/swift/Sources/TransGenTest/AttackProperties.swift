public struct AttackProperties {
    let attackRange: Int32
    let damage: Int32
    let collectResource: Bool

    static func readFrom<S: InputStream>(_ stream: S) -> AttackProperties {
        var attackRange: Int32
        attackRange = stream.readInt32()
        var damage: Int32
        damage = stream.readInt32()
        var collectResource: Bool
        collectResource = stream.readBool()
        return AttackProperties(attackRange: attackRange, damage: damage, collectResource: collectResource)
    }

    func writeTo<S: OutputStream>(_ stream: S) {
        stream.writeInt32(attackRange)
        stream.writeInt32(damage)
        stream.writeBool(collectResource)
    }
}