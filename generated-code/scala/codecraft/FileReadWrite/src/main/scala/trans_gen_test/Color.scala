package trans_gen_test

import trans_gen_test.util.StreamUtil

/**
 * RGBA Color
 *
 * @param r Red component
 * @param g Green component
 * @param b Blue component
 * @param a Alpha (opacity) component
 */
case class Color(r: Float, g: Float, b: Float, a: Float) {
    /**
     * Write Color to output stream
     */
    def writeTo(stream: java.io.OutputStream) {
        StreamUtil.writeFloat(stream, r)
        StreamUtil.writeFloat(stream, g)
        StreamUtil.writeFloat(stream, b)
        StreamUtil.writeFloat(stream, a)
    }

    /**
     * Get string representation of Color
     */
    override def toString(): String = {
        var stringBuilder = new StringBuilder("Color { ")
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
        stringBuilder.toString()
    }
}

object Color {
    /**
     * Read Color from input stream
     */
    def readFrom(stream: java.io.InputStream): Color = Color(
        StreamUtil.readFloat(stream),
        StreamUtil.readFloat(stream),
        StreamUtil.readFloat(stream),
        StreamUtil.readFloat(stream)
    )
}