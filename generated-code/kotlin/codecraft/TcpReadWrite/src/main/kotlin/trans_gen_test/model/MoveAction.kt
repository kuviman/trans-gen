package trans_gen_test.model

import trans_gen_test.util.StreamUtil

/**
 * Move action
 */
class MoveAction {
    /**
     * Target position
     */
    var target: trans_gen_test.Vec2Int
    /**
     * Whether to try find closest position, if path to target is not found
     */
    var findClosestPosition: Boolean
    /**
     * Whether to destroy other entities on the way
     */
    var breakThrough: Boolean

    constructor(target: trans_gen_test.Vec2Int, findClosestPosition: Boolean, breakThrough: Boolean) {
        this.target = target
        this.findClosestPosition = findClosestPosition
        this.breakThrough = breakThrough
    }

    /**
     * Write MoveAction to output stream
     */
    @Throws(java.io.IOException::class)
    fun writeTo(stream: java.io.OutputStream) {
        target.writeTo(stream)
        StreamUtil.writeBoolean(stream, findClosestPosition)
        StreamUtil.writeBoolean(stream, breakThrough)
    }

    /**
     * Get string representation of MoveAction
     */
    override fun toString(): String {
        var stringBuilder = StringBuilder("MoveAction { ")
        stringBuilder.append("target: ")
        stringBuilder.append(target)
        stringBuilder.append(", ")
        stringBuilder.append("findClosestPosition: ")
        stringBuilder.append(findClosestPosition)
        stringBuilder.append(", ")
        stringBuilder.append("breakThrough: ")
        stringBuilder.append(breakThrough)
        stringBuilder.append(" }")
        return stringBuilder.toString()
    }

    companion object {
        /**
         * Read MoveAction from input stream
         */
        @Throws(java.io.IOException::class)
        fun readFrom(stream: java.io.InputStream): MoveAction {
            var target: trans_gen_test.Vec2Int
            target = trans_gen_test.Vec2Int.readFrom(stream)
            var findClosestPosition: Boolean
            findClosestPosition = StreamUtil.readBoolean(stream)
            var breakThrough: Boolean
            breakThrough = StreamUtil.readBoolean(stream)
            return MoveAction(target, findClosestPosition, breakThrough)
        }
    }
}