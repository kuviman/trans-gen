package model

import util.StreamUtil

case class Player(id: Int, score: Int, resource: Int) {
    def writeTo(stream: java.io.OutputStream) {
        StreamUtil.writeInt(stream, id)
        StreamUtil.writeInt(stream, score)
        StreamUtil.writeInt(stream, resource)
    }

    override def toString(): String = {
        var stringBuilder = new StringBuilder("Player { ")
        stringBuilder.append("id: ")
        stringBuilder.append(id)
        stringBuilder.append(", ")
        stringBuilder.append("score: ")
        stringBuilder.append(score)
        stringBuilder.append(", ")
        stringBuilder.append("resource: ")
        stringBuilder.append(resource)
        stringBuilder.append(" }")
        stringBuilder.toString()
    }
}

object Player {
    def readFrom(stream: java.io.InputStream): Player = Player(
        StreamUtil.readInt(stream),
        StreamUtil.readInt(stream),
        StreamUtil.readInt(stream)
    )
}