package trans_gen_test.model

import trans_gen_test.util.StreamUtil

/**
 * Auto attack options
 *
 * @param pathfindRange Maximum distance to pathfind
 * @param validTargets List of target entity types to try to attack. If empty, all types but resource are considered
 */
case class AutoAttack(pathfindRange: Int, validTargets: Seq[trans_gen_test.model.EntityType]) {
    /**
     * Write AutoAttack to output stream
     */
    def writeTo(stream: java.io.OutputStream) {
        StreamUtil.writeInt(stream, pathfindRange)
        StreamUtil.writeInt(stream, validTargets.length)
        validTargets.foreach { value =>
            value.writeTo(stream)
        }
    }

    /**
     * Get string representation of AutoAttack
     */
    override def toString(): String = {
        var stringBuilder = new StringBuilder("AutoAttack { ")
        stringBuilder.append("pathfindRange: ")
        stringBuilder.append(pathfindRange)
        stringBuilder.append(", ")
        stringBuilder.append("validTargets: ")
        stringBuilder.append(validTargets)
        stringBuilder.append(" }")
        stringBuilder.toString()
    }
}

object AutoAttack {
    /**
     * Read AutoAttack from input stream
     */
    def readFrom(stream: java.io.InputStream): AutoAttack = AutoAttack(
        StreamUtil.readInt(stream),
        (0 until StreamUtil.readInt(stream)).map { _ =>
            trans_gen_test.model.EntityType.readFrom(stream)
        }
    )
}