package trans_gen_test.model

import trans_gen_test.util.StreamUtil

/**
 * Entity's action
 *
 * @param moveAction Move action
 * @param buildAction Build action
 * @param attackAction Attack action
 * @param repairAction Repair action
 */
case class EntityAction(moveAction: Option[trans_gen_test.model.MoveAction], buildAction: Option[trans_gen_test.model.BuildAction], attackAction: Option[trans_gen_test.model.AttackAction], repairAction: Option[trans_gen_test.model.RepairAction]) {
    /**
     * Write EntityAction to output stream
     */
    def writeTo(stream: java.io.OutputStream) {
        moveAction match {
            case None => StreamUtil.writeBoolean(stream, false)
            case Some(value) => {
                StreamUtil.writeBoolean(stream, true)
                value.writeTo(stream)
            }
        }
        buildAction match {
            case None => StreamUtil.writeBoolean(stream, false)
            case Some(value) => {
                StreamUtil.writeBoolean(stream, true)
                value.writeTo(stream)
            }
        }
        attackAction match {
            case None => StreamUtil.writeBoolean(stream, false)
            case Some(value) => {
                StreamUtil.writeBoolean(stream, true)
                value.writeTo(stream)
            }
        }
        repairAction match {
            case None => StreamUtil.writeBoolean(stream, false)
            case Some(value) => {
                StreamUtil.writeBoolean(stream, true)
                value.writeTo(stream)
            }
        }
    }

    /**
     * Get string representation of EntityAction
     */
    override def toString(): String = {
        var stringBuilder = new StringBuilder("EntityAction { ")
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
        stringBuilder.toString()
    }
}

object EntityAction {
    /**
     * Read EntityAction from input stream
     */
    def readFrom(stream: java.io.InputStream): EntityAction = EntityAction(
        if (StreamUtil.readBoolean(stream)) Some(
            trans_gen_test.model.MoveAction.readFrom(stream)
        ) else None,
        if (StreamUtil.readBoolean(stream)) Some(
            trans_gen_test.model.BuildAction.readFrom(stream)
        ) else None,
        if (StreamUtil.readBoolean(stream)) Some(
            trans_gen_test.model.AttackAction.readFrom(stream)
        ) else None,
        if (StreamUtil.readBoolean(stream)) Some(
            trans_gen_test.model.RepairAction.readFrom(stream)
        ) else None
    )
}