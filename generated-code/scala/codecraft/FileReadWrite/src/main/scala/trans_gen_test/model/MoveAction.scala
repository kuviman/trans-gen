package trans_gen_test.model

import trans_gen_test.util.StreamUtil

/**
 * Move action
 *
 * @param target Target position
 * @param findClosestPosition Whether to try find closest position, if path to target is not found
 * @param breakThrough Whether to destroy other entities on the way
 */
case class MoveAction(target: trans_gen_test.Vec2Int, findClosestPosition: Boolean, breakThrough: Boolean) {
    /**
     * Write MoveAction to output stream
     */
    def writeTo(stream: java.io.OutputStream) {
        target.writeTo(stream)
        StreamUtil.writeBoolean(stream, findClosestPosition)
        StreamUtil.writeBoolean(stream, breakThrough)
    }

    /**
     * Get string representation of MoveAction
     */
    override def toString(): String = {
        var stringBuilder = new StringBuilder("MoveAction { ")
        stringBuilder.append("target: ")
        stringBuilder.append(target)
        stringBuilder.append(", ")
        stringBuilder.append("findClosestPosition: ")
        stringBuilder.append(findClosestPosition)
        stringBuilder.append(", ")
        stringBuilder.append("breakThrough: ")
        stringBuilder.append(breakThrough)
        stringBuilder.append(" }")
        stringBuilder.toString()
    }
}

object MoveAction {
    /**
     * Read MoveAction from input stream
     */
    def readFrom(stream: java.io.InputStream): MoveAction = MoveAction(
        trans_gen_test.Vec2Int.readFrom(stream),
        StreamUtil.readBoolean(stream),
        StreamUtil.readBoolean(stream)
    )
}