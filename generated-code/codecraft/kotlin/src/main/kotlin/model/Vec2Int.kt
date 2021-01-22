package model

import util.StreamUtil

class Vec2Int {
    var x: Int = 0
    var y: Int = 0

    constructor(x: Int, y: Int) {
        this.x = x
        this.y = y
    }

    @Throws(java.io.IOException::class)
    fun writeTo(stream: java.io.OutputStream) {
        StreamUtil.writeInt(stream, x)
        StreamUtil.writeInt(stream, y)
    }

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