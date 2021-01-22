package model

import util.StreamUtil

case class Vec2Int(x: Int, y: Int) {
    def writeTo(stream: java.io.OutputStream) {
        StreamUtil.writeInt(stream, x)
        StreamUtil.writeInt(stream, y)
    }

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
    def readFrom(stream: java.io.InputStream): Vec2Int = Vec2Int(
        StreamUtil.readInt(stream),
        StreamUtil.readInt(stream)
    )
}