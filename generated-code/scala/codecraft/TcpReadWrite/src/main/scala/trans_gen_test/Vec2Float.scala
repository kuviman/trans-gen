package trans_gen_test

import trans_gen_test.util.StreamUtil

/**
 * 2 dimensional vector.
 *
 * @param x `x` coordinate of the vector
 * @param y `y` coordinate of the vector
 */
case class Vec2Float(x: Float, y: Float) {
    /**
     * Write Vec2Float to output stream
     */
    def writeTo(stream: java.io.OutputStream) {
        StreamUtil.writeFloat(stream, x)
        StreamUtil.writeFloat(stream, y)
    }

    /**
     * Get string representation of Vec2Float
     */
    override def toString(): String = {
        var stringBuilder = new StringBuilder("Vec2Float { ")
        stringBuilder.append("x: ")
        stringBuilder.append(x)
        stringBuilder.append(", ")
        stringBuilder.append("y: ")
        stringBuilder.append(y)
        stringBuilder.append(" }")
        stringBuilder.toString()
    }
}

object Vec2Float {
    /**
     * Read Vec2Float from input stream
     */
    def readFrom(stream: java.io.InputStream): Vec2Float = Vec2Float(
        StreamUtil.readFloat(stream),
        StreamUtil.readFloat(stream)
    )
}