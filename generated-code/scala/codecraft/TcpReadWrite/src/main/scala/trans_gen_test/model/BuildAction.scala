package trans_gen_test.model

import trans_gen_test.util.StreamUtil

/**
 * Build action
 *
 * @param entityType Type of an entity to build
 * @param position Desired position of new entity
 */
case class BuildAction(entityType: trans_gen_test.model.EntityType, position: trans_gen_test.Vec2Int) {
    /**
     * Write BuildAction to output stream
     */
    def writeTo(stream: java.io.OutputStream) {
        entityType.writeTo(stream)
        position.writeTo(stream)
    }

    /**
     * Get string representation of BuildAction
     */
    override def toString(): String = {
        var stringBuilder = new StringBuilder("BuildAction { ")
        stringBuilder.append("entityType: ")
        stringBuilder.append(entityType)
        stringBuilder.append(", ")
        stringBuilder.append("position: ")
        stringBuilder.append(position)
        stringBuilder.append(" }")
        stringBuilder.toString()
    }
}

object BuildAction {
    /**
     * Read BuildAction from input stream
     */
    def readFrom(stream: java.io.InputStream): BuildAction = BuildAction(
        trans_gen_test.model.EntityType.readFrom(stream),
        trans_gen_test.Vec2Int.readFrom(stream)
    )
}