package trans_gen_test.model

import trans_gen_test.util.StreamUtil

/**
 * Repair action
 */
class RepairAction {
    /**
     * Target entity's ID
     */
    var target: Int

    constructor(target: Int) {
        this.target = target
    }

    /**
     * Write RepairAction to output stream
     */
    @Throws(java.io.IOException::class)
    fun writeTo(stream: java.io.OutputStream) {
        StreamUtil.writeInt(stream, target)
    }

    /**
     * Get string representation of RepairAction
     */
    override fun toString(): String {
        var stringBuilder = StringBuilder("RepairAction { ")
        stringBuilder.append("target: ")
        stringBuilder.append(target)
        stringBuilder.append(" }")
        return stringBuilder.toString()
    }

    companion object {
        /**
         * Read RepairAction from input stream
         */
        @Throws(java.io.IOException::class)
        fun readFrom(stream: java.io.InputStream): RepairAction {
            var target: Int
            target = StreamUtil.readInt(stream)
            return RepairAction(target)
        }
    }
}