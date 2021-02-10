package trans_gen_test.model

import trans_gen_test.util.StreamUtil

/**
 * Player (strategy, client)
 *
 * @param id Player's ID
 * @param score Current score
 * @param resource Current amount of resource
 */
case class Player(id: Int, score: Int, resource: Int) {
    /**
     * Write Player to output stream
     */
    def writeTo(stream: java.io.OutputStream) {
        StreamUtil.writeInt(stream, id)
        StreamUtil.writeInt(stream, score)
        StreamUtil.writeInt(stream, resource)
    }

    /**
     * Get string representation of Player
     */
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
    /**
     * Read Player from input stream
     */
    def readFrom(stream: java.io.InputStream): Player = Player(
        StreamUtil.readInt(stream),
        StreamUtil.readInt(stream),
        StreamUtil.readInt(stream)
    )
}