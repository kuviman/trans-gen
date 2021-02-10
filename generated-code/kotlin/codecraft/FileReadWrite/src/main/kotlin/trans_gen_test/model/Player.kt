package trans_gen_test.model

import trans_gen_test.util.StreamUtil

/**
 * Player (strategy, client)
 */
class Player {
    /**
     * Player's ID
     */
    var id: Int
    /**
     * Current score
     */
    var score: Int
    /**
     * Current amount of resource
     */
    var resource: Int

    constructor(id: Int, score: Int, resource: Int) {
        this.id = id
        this.score = score
        this.resource = resource
    }

    /**
     * Write Player to output stream
     */
    @Throws(java.io.IOException::class)
    fun writeTo(stream: java.io.OutputStream) {
        StreamUtil.writeInt(stream, id)
        StreamUtil.writeInt(stream, score)
        StreamUtil.writeInt(stream, resource)
    }

    /**
     * Get string representation of Player
     */
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
        /**
         * Read Player from input stream
         */
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