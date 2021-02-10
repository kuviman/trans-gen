package trans_gen_test

import trans_gen_test.util.StreamUtil

/**
 * 2 dimensional vector.
 */
class Vec2Int {
    /**
     * `x` coordinate of the vector
     */
    var x: Int
    /**
     * `y` coordinate of the vector
     */
    var y: Int

    constructor(x: Int, y: Int) {
        this.x = x
        this.y = y
    }

    /**
     * Write Vec2Int to output stream
     */
    @Throws(java.io.IOException::class)
    fun writeTo(stream: java.io.OutputStream) {
        StreamUtil.writeInt(stream, x)
        StreamUtil.writeInt(stream, y)
    }

    /**
     * Get string representation of Vec2Int
     */
    override fun toString(): String {
        var stringBuilder = StringBuilder("Vec2Int { ")
        stringBuilder.append("x: ")
        stringBuilder.append(x)
        stringBuilder.append(", ")
        stringBuilder.append("y: ")
        stringBuilder.append(y)
        stringBuilder.append(" }")
        return stringBuilder.toString()
    }

    companion object {
        /**
         * Read Vec2Int from input stream
         */
        @Throws(java.io.IOException::class)
        fun readFrom(stream: java.io.InputStream): Vec2Int {
            var x: Int
            x = StreamUtil.readInt(stream)
            var y: Int
            y = StreamUtil.readInt(stream)
            return Vec2Int(x, y)
        }
    }
}