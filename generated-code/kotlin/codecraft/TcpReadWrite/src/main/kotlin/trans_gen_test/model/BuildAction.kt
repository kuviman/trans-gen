package trans_gen_test.model

import trans_gen_test.util.StreamUtil

/**
 * Build action
 */
class BuildAction {
    /**
     * Type of an entity to build
     */
    var entityType: trans_gen_test.model.EntityType
    /**
     * Desired position of new entity
     */
    var position: trans_gen_test.Vec2Int

    constructor(entityType: trans_gen_test.model.EntityType, position: trans_gen_test.Vec2Int) {
        this.entityType = entityType
        this.position = position
    }

    /**
     * Write BuildAction to output stream
     */
    @Throws(java.io.IOException::class)
    fun writeTo(stream: java.io.OutputStream) {
        StreamUtil.writeInt(stream, entityType.tag)
        position.writeTo(stream)
    }

    /**
     * Get string representation of BuildAction
     */
    override fun toString(): String {
        var stringBuilder = StringBuilder("BuildAction { ")
        stringBuilder.append("entityType: ")
        stringBuilder.append(entityType)
        stringBuilder.append(", ")
        stringBuilder.append("position: ")
        stringBuilder.append(position)
        stringBuilder.append(" }")
        return stringBuilder.toString()
    }

    companion object {
        /**
         * Read BuildAction from input stream
         */
        @Throws(java.io.IOException::class)
        fun readFrom(stream: java.io.InputStream): BuildAction {
            var entityType: trans_gen_test.model.EntityType
            entityType = trans_gen_test.model.EntityType.readFrom(stream)
            var position: trans_gen_test.Vec2Int
            position = trans_gen_test.Vec2Int.readFrom(stream)
            return BuildAction(entityType, position)
        }
    }
}