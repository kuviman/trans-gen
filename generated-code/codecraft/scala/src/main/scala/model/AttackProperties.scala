package model

import util.StreamUtil

/**
 * Entity's attack properties
 *
 * @param attackRange Maximum attack range
 * @param damage Damage dealt in one tick
 * @param collectResource If true, dealing damage will collect resource from target
 */
case class AttackProperties(attackRange: Int, damage: Int, collectResource: Boolean) {
    /**
     * Write AttackProperties to output stream
     */
    def writeTo(stream: java.io.OutputStream) {
        StreamUtil.writeInt(stream, attackRange)
        StreamUtil.writeInt(stream, damage)
        StreamUtil.writeBoolean(stream, collectResource)
    }

    /**
     * Get string representation of AttackProperties
     */
    override def toString(): String = {
        var stringBuilder = new StringBuilder("AttackProperties { ")
        stringBuilder.append("attackRange: ")
        stringBuilder.append(attackRange)
        stringBuilder.append(", ")
        stringBuilder.append("damage: ")
        stringBuilder.append(damage)
        stringBuilder.append(", ")
        stringBuilder.append("collectResource: ")
        stringBuilder.append(collectResource)
        stringBuilder.append(" }")
        stringBuilder.toString()
    }
}

object AttackProperties {
    /**
     * Read AttackProperties from input stream
     */
    def readFrom(stream: java.io.InputStream): AttackProperties = AttackProperties(
        StreamUtil.readInt(stream),
        StreamUtil.readInt(stream),
        StreamUtil.readBoolean(stream)
    )
}