package trans_gen_test.model

import trans_gen_test.util.StreamUtil

/**
 * Player's action
 *
 * @param entityActions New actions for entities. If entity does not get new action, if will continue to perform previously set one
 */
case class Action(entityActions: Map[Int, trans_gen_test.model.EntityAction]) {
    /**
     * Write Action to output stream
     */
    def writeTo(stream: java.io.OutputStream) {
        StreamUtil.writeInt(stream, entityActions.size)
        entityActions.foreach { case (key, value) =>
            StreamUtil.writeInt(stream, key)
            value.writeTo(stream)
        }
    }

    /**
     * Get string representation of Action
     */
    override def toString(): String = {
        var stringBuilder = new StringBuilder("Action { ")
        stringBuilder.append("entityActions: ")
        stringBuilder.append(entityActions)
        stringBuilder.append(" }")
        stringBuilder.toString()
    }
}

object Action {
    /**
     * Read Action from input stream
     */
    def readFrom(stream: java.io.InputStream): Action = Action(
        (0 until StreamUtil.readInt(stream)).map { _ => (
            StreamUtil.readInt(stream),
            trans_gen_test.model.EntityAction.readFrom(stream)
        )}.toMap
    )
}