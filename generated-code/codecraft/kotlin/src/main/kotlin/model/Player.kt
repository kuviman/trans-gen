package model

import util.StreamUtil

class Player {
    var id: Int = 0
    var score: Int = 0
    var resource: Int = 0

    constructor(id: Int, score: Int, resource: Int) {
        this.id = id
        this.score = score
        this.resource = resource
    }

    @Throws(java.io.IOException::class)
    fun writeTo(stream: java.io.OutputStream) {
        StreamUtil.writeInt(stream, id)
        StreamUtil.writeInt(stream, score)
        StreamUtil.writeInt(stream, resource)
    }

    override fun toString(): String {
        var stringBuilder = StringBuilder("Player { ")
        stringBuilder.append("id: ")
        stringBuilder.append(id)
        stringBuilder.append(", ")
        stringBuilder.append("score: ")
        stringBuilder.append(score)
        stringBuilder.append(", ")
        stringBuilder.append("resource: ")
        stringBuilder.append(resource)
        stringBuilder.append(" }")
        return stringBuilder.toString()
    }

    companion object {
        @Throws(java.io.IOException::class)
        fun readFrom(stream: java.io.InputStream): Player {
            var id: Int
            id = StreamUtil.readInt(stream)
            var score: Int
            score = StreamUtil.readInt(stream)
            var resource: Int
            resource = StreamUtil.readInt(stream)
            return Player(id, score, resource)
        }
    }
}