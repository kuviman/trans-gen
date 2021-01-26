public struct RepairProperties {
    let validTargets: [EntityType]
    let power: Int32

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

    func writeTo<S: OutputStream>(_ stream: S) {
        stream.writeInt32(Int32(validTargets.count))
        for validTargetsElement in validTargets {
            validTargetsElement.writeTo(stream)
        }
        stream.writeInt32(power)
    }
}