/// Entity's attack properties
public struct AttackProperties {
    /// Maximum attack range
    let attackRange: Int32

    /// Damage dealt in one tick
    let damage: Int32

    /// If true, dealing damage will collect resource from target
    let collectResource: Bool

    /// Read AttackProperties from input stream
    static func readFrom<S: InputStream>(_ stream: S) -> AttackProperties {
        var attackRange: Int32
        attackRange = stream.readInt32()
        var damage: Int32
        damage = stream.readInt32()
        var collectResource: Bool
        collectResource = stream.readBool()
        return AttackProperties(attackRange: attackRange, damage: damage, collectResource: collectResource)
    }

    /// Write AttackProperties to output stream
    func writeTo<S: OutputStream>(_ stream: S) {
        stream.writeInt32(attackRange)
        stream.writeInt32(damage)
        stream.writeBool(collectResource)
    }
}