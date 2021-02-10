package trans_gen_test.model

import trans_gen_test.util.StreamUtil

/**
 * Entity's repair properties
 *
 * @param validTargets Valid target entity types
 * @param power Health restored in one tick
 */
case class RepairProperties(validTargets: Seq[trans_gen_test.model.EntityType], power: Int) {
    /**
     * Write RepairProperties to output stream
     */
    def writeTo(stream: java.io.OutputStream) {
        StreamUtil.writeInt(stream, validTargets.length)
        validTargets.foreach { value =>
            value.writeTo(stream)
        }
        StreamUtil.writeInt(stream, power)
    }

    /**
     * Get string representation of RepairProperties
     */
    override def toString(): String = {
        var stringBuilder = new StringBuilder("RepairProperties { ")
        stringBuilder.append("validTargets: ")
        stringBuilder.append(validTargets)
        stringBuilder.append(", ")
        stringBuilder.append("power: ")
        stringBuilder.append(power)
        stringBuilder.append(" }")
        stringBuilder.toString()
    }
}

object RepairProperties {
    /**
     * Read RepairProperties from input stream
     */
    def readFrom(stream: java.io.InputStream): RepairProperties = RepairProperties(
        (0 until StreamUtil.readInt(stream)).map { _ =>
            trans_gen_test.model.EntityType.readFrom(stream)
        },
        StreamUtil.readInt(stream)
    )
}