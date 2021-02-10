package trans_gen_test.model

import trans_gen_test.util.StreamUtil

/**
 * Entity properties
 */
class EntityProperties {
    /**
     * Size. Entity has a form of a square with side of this length
     */
    var size: Int
    /**
     * Score for building this entity
     */
    var buildScore: Int
    /**
     * Score for destroying this entity
     */
    var destroyScore: Int
    /**
     * Whether this entity can move
     */
    var canMove: Boolean
    /**
     * Number of population points this entity provides, if active
     */
    var populationProvide: Int
    /**
     * Number of population points this entity uses
     */
    var populationUse: Int
    /**
     * Maximum health points
     */
    var maxHealth: Int
    /**
     * Cost to build this first entity of this type. If this is a unit (entity can move), the cost is increased by 1 for each existing unit of this type
     */
    var initialCost: Int
    /**
     * If fog of war is enabled, maximum distance at which other entities are considered visible
     */
    var sightRange: Int
    /**
     * Amount of resource added to enemy able to collect resource on dealing damage for 1 health point
     */
    var resourcePerHealth: Int
    /**
     * Build properties, if entity can build
     */
    var build: trans_gen_test.model.BuildProperties?
    /**
     * Attack properties, if entity can attack
     */
    var attack: trans_gen_test.model.AttackProperties?
    /**
     * Repair properties, if entity can repair
     */
    var repair: trans_gen_test.model.RepairProperties?

    constructor(size: Int, buildScore: Int, destroyScore: Int, canMove: Boolean, populationProvide: Int, populationUse: Int, maxHealth: Int, initialCost: Int, sightRange: Int, resourcePerHealth: Int, build: trans_gen_test.model.BuildProperties?, attack: trans_gen_test.model.AttackProperties?, repair: trans_gen_test.model.RepairProperties?) {
        this.size = size
        this.buildScore = buildScore
        this.destroyScore = destroyScore
        this.canMove = canMove
        this.populationProvide = populationProvide
        this.populationUse = populationUse
        this.maxHealth = maxHealth
        this.initialCost = initialCost
        this.sightRange = sightRange
        this.resourcePerHealth = resourcePerHealth
        this.build = build
        this.attack = attack
        this.repair = repair
    }

    /**
     * Write EntityProperties to output stream
     */
    @Throws(java.io.IOException::class)
    fun writeTo(stream: java.io.OutputStream) {
        StreamUtil.writeInt(stream, size)
        StreamUtil.writeInt(stream, buildScore)
        StreamUtil.writeInt(stream, destroyScore)
        StreamUtil.writeBoolean(stream, canMove)
        StreamUtil.writeInt(stream, populationProvide)
        StreamUtil.writeInt(stream, populationUse)
        StreamUtil.writeInt(stream, maxHealth)
        StreamUtil.writeInt(stream, initialCost)
        StreamUtil.writeInt(stream, sightRange)
        StreamUtil.writeInt(stream, resourcePerHealth)
        val buildValue = build
        if (buildValue == null) {
            StreamUtil.writeBoolean(stream, false)
        } else {
            StreamUtil.writeBoolean(stream, true)
            buildValue.writeTo(stream)
        }
        val attackValue = attack
        if (attackValue == null) {
            StreamUtil.writeBoolean(stream, false)
        } else {
            StreamUtil.writeBoolean(stream, true)
            attackValue.writeTo(stream)
        }
        val repairValue = repair
        if (repairValue == null) {
            StreamUtil.writeBoolean(stream, false)
        } else {
            StreamUtil.writeBoolean(stream, true)
            repairValue.writeTo(stream)
        }
    }

    /**
     * Get string representation of EntityProperties
     */
    override fun toString(): String {
        var stringBuilder = StringBuilder("EntityProperties { ")
        stringBuilder.append("size: ")
        stringBuilder.append(size)
        stringBuilder.append(", ")
        stringBuilder.append("buildScore: ")
        stringBuilder.append(buildScore)
        stringBuilder.append(", ")
        stringBuilder.append("destroyScore: ")
        stringBuilder.append(destroyScore)
        stringBuilder.append(", ")
        stringBuilder.append("canMove: ")
        stringBuilder.append(canMove)
        stringBuilder.append(", ")
        stringBuilder.append("populationProvide: ")
        stringBuilder.append(populationProvide)
        stringBuilder.append(", ")
        stringBuilder.append("populationUse: ")
        stringBuilder.append(populationUse)
        stringBuilder.append(", ")
        stringBuilder.append("maxHealth: ")
        stringBuilder.append(maxHealth)
        stringBuilder.append(", ")
        stringBuilder.append("initialCost: ")
        stringBuilder.append(initialCost)
        stringBuilder.append(", ")
        stringBuilder.append("sightRange: ")
        stringBuilder.append(sightRange)
        stringBuilder.append(", ")
        stringBuilder.append("resourcePerHealth: ")
        stringBuilder.append(resourcePerHealth)
        stringBuilder.append(", ")
        stringBuilder.append("build: ")
        stringBuilder.append(build)
        stringBuilder.append(", ")
        stringBuilder.append("attack: ")
        stringBuilder.append(attack)
        stringBuilder.append(", ")
        stringBuilder.append("repair: ")
        stringBuilder.append(repair)
        stringBuilder.append(" }")
        return stringBuilder.toString()
    }

    companion object {
        /**
         * Read EntityProperties from input stream
         */
        @Throws(java.io.IOException::class)
        fun readFrom(stream: java.io.InputStream): EntityProperties {
            var size: Int
            size = StreamUtil.readInt(stream)
            var buildScore: Int
            buildScore = StreamUtil.readInt(stream)
            var destroyScore: Int
            destroyScore = StreamUtil.readInt(stream)
            var canMove: Boolean
            canMove = StreamUtil.readBoolean(stream)
            var populationProvide: Int
            populationProvide = StreamUtil.readInt(stream)
            var populationUse: Int
            populationUse = StreamUtil.readInt(stream)
            var maxHealth: Int
            maxHealth = StreamUtil.readInt(stream)
            var initialCost: Int
            initialCost = StreamUtil.readInt(stream)
            var sightRange: Int
            sightRange = StreamUtil.readInt(stream)
            var resourcePerHealth: Int
            resourcePerHealth = StreamUtil.readInt(stream)
            var build: trans_gen_test.model.BuildProperties?
            if (StreamUtil.readBoolean(stream)) {
                build = trans_gen_test.model.BuildProperties.readFrom(stream)
            } else {
                build = null
            }
            var attack: trans_gen_test.model.AttackProperties?
            if (StreamUtil.readBoolean(stream)) {
                attack = trans_gen_test.model.AttackProperties.readFrom(stream)
            } else {
                attack = null
            }
            var repair: trans_gen_test.model.RepairProperties?
            if (StreamUtil.readBoolean(stream)) {
                repair = trans_gen_test.model.RepairProperties.readFrom(stream)
            } else {
                repair = null
            }
            return EntityProperties(size, buildScore, destroyScore, canMove, populationProvide, populationUse, maxHealth, initialCost, sightRange, resourcePerHealth, build, attack, repair)
        }
    }
}