package trans_gen_test.model

import trans_gen_test.util.StreamUtil

/**
 * Player's action
 */
class Action {
    /**
     * New actions for entities. If entity does not get new action, if will continue to perform previously set one
     */
    var entityActions: MutableMap<Int, trans_gen_test.model.EntityAction>

    constructor(entityActions: MutableMap<Int, trans_gen_test.model.EntityAction>) {
        this.entityActions = entityActions
    }

    /**
     * Write Action to output stream
     */
    @Throws(java.io.IOException::class)
    fun writeTo(stream: java.io.OutputStream) {
        StreamUtil.writeInt(stream, entityActions.size)
        for (entityActionsEntry in entityActions) {
            val entityActionsKey = entityActionsEntry.key
            StreamUtil.writeInt(stream, entityActionsKey)
            val entityActionsValue = entityActionsEntry.value
            entityActionsValue.writeTo(stream)
        }
    }

    /**
     * Get string representation of Action
     */
    override fun toString(): String {
        var stringBuilder = StringBuilder("Action { ")
        stringBuilder.append("entityActions: ")
        stringBuilder.append(entityActions)
        stringBuilder.append(" }")
        return stringBuilder.toString()
    }

    companion object {
        /**
         * Read Action from input stream
         */
        @Throws(java.io.IOException::class)
        fun readFrom(stream: java.io.InputStream): Action {
            var entityActions: MutableMap<Int, trans_gen_test.model.EntityAction>
            val entityActionsSize = StreamUtil.readInt(stream)
            entityActions = mutableMapOf();
            for (entityActionsIndex in 0 until entityActionsSize) {
                var entityActionsKey: Int
                entityActionsKey = StreamUtil.readInt(stream)
                var entityActionsValue: trans_gen_test.model.EntityAction
                entityActionsValue = trans_gen_test.model.EntityAction.readFrom(stream)
                entityActions.put(entityActionsKey, entityActionsValue)
            }
            return Action(entityActions)
        }
    }
}