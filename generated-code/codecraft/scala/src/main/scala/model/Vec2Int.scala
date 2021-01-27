package model

import util.StreamUtil

/**
 * 2 dimensional vector.
 *
 * @param x `x` coordinate of the vector
 * @param y `y` coordinate of the vector
 */
case class Vec2Int(x: Int, y: Int) {
    /**
     * Write Vec2Int to output stream
     */
    def writeTo(stream: java.io.OutputStream) {
        StreamUtil.writeInt(stream, x)
        StreamUtil.writeInt(stream, y)
    }

    /**
     * Get string representation of Vec2Int
     */
    override def toString(): String = {
        var stringBuilder = new StringBuilder("Vec2Int { ")
        stringBuilder.append("x: ")
        stringBuilder.append(x)
        stringBuilder.append(", ")
        stringBuilder.append("y: ")
        stringBuilder.append(y)
        stringBuilder.append(" }")
        stringBuilder.toString()
    }
}

object Vec2Int {
    /**
     * Read Vec2Int from input stream
     */
    def readFrom(stream: java.io.InputStream): Vec2Int = Vec2Int(
        StreamUtil.readInt(stream),
        StreamUtil.readInt(stream)
    )
}