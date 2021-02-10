/// Auto attack options
public struct AutoAttack {
    /// Maximum distance to pathfind
    let pathfindRange: Int32

    /// List of target entity types to try to attack. If empty, all types but resource are considered
    let validTargets: [EntityType]

    /// Read AutoAttack from input stream
    static func readFrom<S: InputStream>(_ stream: S) -> AutoAttack {
        var pathfindRange: Int32
        pathfindRange = stream.readInt32()
        var validTargets: [EntityType]
        let validTargetsSize = stream.readInt32()
        validTargets = (0..<validTargetsSize).map{ _ in
            var validTargetsSize: EntityType
            validTargetsSize = EntityType.readFrom(stream)
            return validTargetsSize
        }
        return AutoAttack(pathfindRange: pathfindRange, validTargets: validTargets)
    }

    /// Write AutoAttack to output stream
    func writeTo<S: OutputStream>(_ stream: S) {
        stream.writeInt32(pathfindRange)
        stream.writeInt32(Int32(validTargets.count))
        for validTargetsElement in validTargets {
            validTargetsElement.writeTo(stream)
        }
    }
}