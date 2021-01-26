package model

import util.StreamUtil

/**
 * Entity's attack properties
 */
class AttackProperties {
    /**
     * Maximum attack range
     */
    var attackRange: Int = 0
    /**
     * Damage dealt in one tick
     */
    var damage: Int = 0
    /**
     * If true, dealing damage will collect resource from target
     */
    var collectResource: Boolean = false

    constructor(attackRange: Int, damage: Int, collectResource: Boolean) {
        this.attackRange = attackRange
        this.damage = damage
        this.collectResource = collectResource
    }

    /**
     * Write AttackProperties to output stream
     */
    @Throws(java.io.IOException::class)
    fun writeTo(stream: java.io.OutputStream) {
        StreamUtil.writeInt(stream, attackRange)
        StreamUtil.writeInt(stream, damage)
        StreamUtil.writeBoolean(stream, collectResource)
    }

    /**
     * Get string representation of AttackProperties
     */
    override fun toString(): String {
        var stringBuilder = StringBuilder("AttackProperties { ")
        stringBuilder.append("attackRange: ")
        stringBuilder.append(attackRange)
        stringBuilder.append(", ")
        stringBuilder.append("damage: ")
        stringBuilder.append(damage)
        stringBuilder.append(", ")
        stringBuilder.append("collectResource: ")
        stringBuilder.append(collectResource)
        stringBuilder.append(" }")
        return stringBuilder.toString()
    }

    companion object {
        /**
         * Read AttackProperties from input stream
         */
        @Throws(java.io.IOException::class)
        fun readFrom(stream: java.io.InputStream): AttackProperties {
            var attackRange: Int
            attackRange = StreamUtil.readInt(stream)
            var damage: Int
            damage = StreamUtil.readInt(stream)
            var collectResource: Boolean
            collectResource = StreamUtil.readBoolean(stream)
            return AttackProperties(attackRange, damage, collectResource)
        }
    }
}