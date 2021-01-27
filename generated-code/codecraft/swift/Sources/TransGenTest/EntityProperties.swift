/// Entity properties
public struct EntityProperties {
    /// Size. Entity has a form of a square with side of this length
    let size: Int32

    /// Score for building this entity
    let buildScore: Int32

    /// Score for destroying this entity
    let destroyScore: Int32

    /// Whether this entity can move
    let canMove: Bool

    /// Number of population points this entity provides, if active
    let populationProvide: Int32

    /// Number of population points this entity uses
    let populationUse: Int32

    /// Maximum health points
    let maxHealth: Int32

    /// Cost to build this first entity of this type. If this is a unit (entity can move), the cost is increased by 1 for each existing unit of this type
    let initialCost: Int32

    /// If fog of war is enabled, maximum distance at which other entities are considered visible
    let sightRange: Int32

    /// Amount of resource added to enemy able to collect resource on dealing damage for 1 health point
    let resourcePerHealth: Int32

    /// Build properties, if entity can build
    let build: BuildProperties?

    /// Attack properties, if entity can attack
    let attack: AttackProperties?

    /// Repair properties, if entity can repair
    let repair: RepairProperties?

    /// Read EntityProperties from input stream
    static func readFrom<S: InputStream>(_ stream: S) -> EntityProperties {
        var size: Int32
        size = stream.readInt32()
        var buildScore: Int32
        buildScore = stream.readInt32()
        var destroyScore: Int32
        destroyScore = stream.readInt32()
        var canMove: Bool
        canMove = stream.readBool()
        var populationProvide: Int32
        populationProvide = stream.readInt32()
        var populationUse: Int32
        populationUse = stream.readInt32()
        var maxHealth: Int32
        maxHealth = stream.readInt32()
        var initialCost: Int32
        initialCost = stream.readInt32()
        var sightRange: Int32
        sightRange = stream.readInt32()
        var resourcePerHealth: Int32
        resourcePerHealth = stream.readInt32()
        var build: BuildProperties?
        if stream.readBool() {
            build = BuildProperties.readFrom(stream)
        } else {
            build = nil
        }
        var attack: AttackProperties?
        if stream.readBool() {
            attack = AttackProperties.readFrom(stream)
        } else {
            attack = nil
        }
        var repair: RepairProperties?
        if stream.readBool() {
            repair = RepairProperties.readFrom(stream)
        } else {
            repair = nil
        }
        return EntityProperties(size: size, buildScore: buildScore, destroyScore: destroyScore, canMove: canMove, populationProvide: populationProvide, populationUse: populationUse, maxHealth: maxHealth, initialCost: initialCost, sightRange: sightRange, resourcePerHealth: resourcePerHealth, build: build, attack: attack, repair: repair)
    }

    /// Write EntityProperties to output stream
    func writeTo<S: OutputStream>(_ stream: S) {
        stream.writeInt32(size)
        stream.writeInt32(buildScore)
        stream.writeInt32(destroyScore)
        stream.writeBool(canMove)
        stream.writeInt32(populationProvide)
        stream.writeInt32(populationUse)
        stream.writeInt32(maxHealth)
        stream.writeInt32(initialCost)
        stream.writeInt32(sightRange)
        stream.writeInt32(resourcePerHealth)
        if build == nil {
            stream.writeBool(false)
        } else {
            stream.writeBool(true)
            let buildValue = build!
            buildValue.writeTo(stream)
        }
        if attack == nil {
            stream.writeBool(false)
        } else {
            stream.writeBool(true)
            let attackValue = attack!
            attackValue.writeTo(stream)
        }
        if repair == nil {
            stream.writeBool(false)
        } else {
            stream.writeBool(true)
            let repairValue = repair!
            repairValue.writeTo(stream)
        }
    }
}