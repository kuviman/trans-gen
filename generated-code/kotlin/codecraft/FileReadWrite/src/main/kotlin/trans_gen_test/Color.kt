package trans_gen_test

import trans_gen_test.util.StreamUtil

/**
 * RGBA Color
 */
class Color {
    /**
     * Red component
     */
    var r: Float
    /**
     * Green component
     */
    var g: Float
    /**
     * Blue component
     */
    var b: Float
    /**
     * Alpha (opacity) component
     */
    var a: Float

    constructor(r: Float, g: Float, b: Float, a: Float) {
        this.r = r
        this.g = g
        this.b = b
        this.a = a
    }

    /**
     * Write Color to output stream
     */
    @Throws(java.io.IOException::class)
    fun writeTo(stream: java.io.OutputStream) {
        StreamUtil.writeFloat(stream, r)
        StreamUtil.writeFloat(stream, g)
        StreamUtil.writeFloat(stream, b)
        StreamUtil.writeFloat(stream, a)
    }

    /**
     * Get string representation of Color
     */
    override fun toString(): String {
        var stringBuilder = StringBuilder("Color { ")
        stringBuilder.append("r: ")
        stringBuilder.append(r)
        stringBuilder.append(", ")
        stringBuilder.append("g: ")
        stringBuilder.append(g)
        stringBuilder.append(", ")
        stringBuilder.append("b: ")
        stringBuilder.append(b)
        stringBuilder.append(", ")
        stringBuilder.append("a: ")
        stringBuilder.append(a)
        stringBuilder.append(" }")
        return stringBuilder.toString()
    }

    companion object {
        /**
         * Read Color from input stream
         */
        @Throws(java.io.IOException::class)
        fun readFrom(stream: java.io.InputStream): Color {
            var r: Float
            r = StreamUtil.readFloat(stream)
            var g: Float
            g = StreamUtil.readFloat(stream)
            var b: Float
            b = StreamUtil.readFloat(stream)
            var a: Float
            a = StreamUtil.readFloat(stream)
            return Color(r, g, b, a)
        }
    }
}