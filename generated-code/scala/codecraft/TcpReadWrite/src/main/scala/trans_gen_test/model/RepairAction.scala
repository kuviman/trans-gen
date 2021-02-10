package trans_gen_test.model

import trans_gen_test.util.StreamUtil

/**
 * Repair action
 *
 * @param target Target entity's ID
 */
case class RepairAction(target: Int) {
    /**
     * Write RepairAction to output stream
     */
    def writeTo(stream: java.io.OutputStream) {
        StreamUtil.writeInt(stream, target)
    }

    /**
     * Get string representation of RepairAction
     */
    override def toString(): String = {
        var stringBuilder = new StringBuilder("RepairAction { ")
        stringBuilder.append("target: ")
        stringBuilder.append(target)
        stringBuilder.append(" }")
        stringBuilder.toString()
    }
}

object RepairAction {
    /**
     * Read RepairAction from input stream
     */
    def readFrom(stream: java.io.InputStream): RepairAction = RepairAction(
        StreamUtil.readInt(stream)
    )
}