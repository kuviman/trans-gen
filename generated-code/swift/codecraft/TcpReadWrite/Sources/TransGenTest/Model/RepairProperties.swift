/// Entity's repair properties
public struct RepairProperties {
    /// Valid target entity types
    let validTargets: [EntityType]

    /// Health restored in one tick
    let power: Int32

    /// Read RepairProperties from input stream
    static func readFrom<S: InputStream>(_ stream: S) -> RepairProperties {
        var validTargets: [EntityType]
        let validTargetsSize = stream.readInt32()
        validTargets = (0..<validTargetsSize).map{ _ in
            var validTargetsSize: EntityType
            validTargetsSize = EntityType.readFrom(stream)
            return validTargetsSize
        }
        var power: Int32
        power = stream.readInt32()
        return RepairProperties(validTargets: validTargets, power: power)
    }

    /// Write RepairProperties to output stream
    func writeTo<S: OutputStream>(_ stream: S) {
        stream.writeInt32(Int32(validTargets.count))
        for validTargetsElement in validTargets {
            validTargetsElement.writeTo(stream)
        }
        stream.writeInt32(power)
    }
}