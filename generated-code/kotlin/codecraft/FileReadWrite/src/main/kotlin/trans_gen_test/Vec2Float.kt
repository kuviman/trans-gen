package trans_gen_test

import trans_gen_test.util.StreamUtil

/**
 * 2 dimensional vector.
 */
class Vec2Float {
    /**
     * `x` coordinate of the vector
     */
    var x: Float
    /**
     * `y` coordinate of the vector
     */
    var y: Float

    constructor(x: Float, y: Float) {
        this.x = x
        this.y = y
    }

    /**
     * Write Vec2Float to output stream
     */
    @Throws(java.io.IOException::class)
    fun writeTo(stream: java.io.OutputStream) {
        StreamUtil.writeFloat(stream, x)
        StreamUtil.writeFloat(stream, y)
    }

    /**
     * Get string representation of Vec2Float
     */
    override fun toString(): String {
        var stringBuilder = StringBuilder("Vec2Float { ")
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
         * Read Vec2Float from input stream
         */
        @Throws(java.io.IOException::class)
        fun readFrom(stream: java.io.InputStream): Vec2Float {
            var x: Float
            x = StreamUtil.readFloat(stream)
            var y: Float
            y = StreamUtil.readFloat(stream)
            return Vec2Float(x, y)
        }
    }
}