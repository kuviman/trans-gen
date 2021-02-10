package trans_gen_test.model

import trans_gen_test.util.StreamUtil

/**
 * Entity's action
 */
class EntityAction {
    /**
     * Move action
     */
    var moveAction: trans_gen_test.model.MoveAction?
    /**
     * Build action
     */
    var buildAction: trans_gen_test.model.BuildAction?
    /**
     * Attack action
     */
    var attackAction: trans_gen_test.model.AttackAction?
    /**
     * Repair action
     */
    var repairAction: trans_gen_test.model.RepairAction?

    constructor(moveAction: trans_gen_test.model.MoveAction?, buildAction: trans_gen_test.model.BuildAction?, attackAction: trans_gen_test.model.AttackAction?, repairAction: trans_gen_test.model.RepairAction?) {
        this.moveAction = moveAction
        this.buildAction = buildAction
        this.attackAction = attackAction
        this.repairAction = repairAction
    }

    /**
     * Write EntityAction to output stream
     */
    @Throws(java.io.IOException::class)
    fun writeTo(stream: java.io.OutputStream) {
        val moveActionValue = moveAction
        if (moveActionValue == null) {
            StreamUtil.writeBoolean(stream, false)
        } else {
            StreamUtil.writeBoolean(stream, true)
            moveActionValue.writeTo(stream)
        }
        val buildActionValue = buildAction
        if (buildActionValue == null) {
            StreamUtil.writeBoolean(stream, false)
        } else {
            StreamUtil.writeBoolean(stream, true)
            buildActionValue.writeTo(stream)
        }
        val attackActionValue = attackAction
        if (attackActionValue == null) {
            StreamUtil.writeBoolean(stream, false)
        } else {
            StreamUtil.writeBoolean(stream, true)
            attackActionValue.writeTo(stream)
        }
        val repairActionValue = repairAction
        if (repairActionValue == null) {
            StreamUtil.writeBoolean(stream, false)
        } else {
            StreamUtil.writeBoolean(stream, true)
            repairActionValue.writeTo(stream)
        }
    }

    /**
     * Get string representation of EntityAction
     */
    override fun toString(): String {
        var stringBuilder = StringBuilder("EntityAction { ")
        stringBuilder.append("moveAction: ")
        stringBuilder.append(moveAction)
        stringBuilder.append(", ")
        stringBuilder.append("buildAction: ")
        stringBuilder.append(buildAction)
        stringBuilder.append(", ")
        stringBuilder.append("attackAction: ")
        stringBuilder.append(attackAction)
        stringBuilder.append(", ")
        stringBuilder.append("repairAction: ")
        stringBuilder.append(repairAction)
        stringBuilder.append(" }")
        return stringBuilder.toString()
    }

    companion object {
        /**
         * Read EntityAction from input stream
         */
        @Throws(java.io.IOException::class)
        fun readFrom(stream: java.io.InputStream): EntityAction {
            var moveAction: trans_gen_test.model.MoveAction?
            if (StreamUtil.readBoolean(stream)) {
                moveAction = trans_gen_test.model.MoveAction.readFrom(stream)
            } else {
                moveAction = null
            }
            var buildAction: trans_gen_test.model.BuildAction?
            if (StreamUtil.readBoolean(stream)) {
                buildAction = trans_gen_test.model.BuildAction.readFrom(stream)
            } else {
                buildAction = null
            }
            var attackAction: trans_gen_test.model.AttackAction?
            if (StreamUtil.readBoolean(stream)) {
                attackAction = trans_gen_test.model.AttackAction.readFrom(stream)
            } else {
                attackAction = null
            }
            var repairAction: trans_gen_test.model.RepairAction?
            if (StreamUtil.readBoolean(stream)) {
                repairAction = trans_gen_test.model.RepairAction.readFrom(stream)
            } else {
                repairAction = null
            }
            return EntityAction(moveAction, buildAction, attackAction, repairAction)
        }
    }
}