package trans_gen_test.model

import trans_gen_test.util.StreamUtil

/**
 * Attack action
 *
 * @param target If specified, target entity's ID
 * @param autoAttack If specified, configures auto attacking
 */
case class AttackAction(target: Option[Int], autoAttack: Option[trans_gen_test.model.AutoAttack]) {
    /**
     * Write AttackAction to output stream
     */
    def writeTo(stream: java.io.OutputStream) {
        target match {
            case None => StreamUtil.writeBoolean(stream, false)
            case Some(value) => {
                StreamUtil.writeBoolean(stream, true)
                StreamUtil.writeInt(stream, value)
            }
        }
        autoAttack match {
            case None => StreamUtil.writeBoolean(stream, false)
            case Some(value) => {
                StreamUtil.writeBoolean(stream, true)
                value.writeTo(stream)
            }
        }
    }

    /**
     * Get string representation of AttackAction
     */
    override def toString(): String = {
        var stringBuilder = new StringBuilder("AttackAction { ")
        stringBuilder.append("target: ")
        stringBuilder.append(target)
        stringBuilder.append(", ")
        stringBuilder.append("autoAttack: ")
        stringBuilder.append(autoAttack)
        stringBuilder.append(" }")
        stringBuilder.toString()
    }
}

object AttackAction {
    /**
     * Read AttackAction from input stream
     */
    def readFrom(stream: java.io.InputStream): AttackAction = AttackAction(
        if (StreamUtil.readBoolean(stream)) Some(
            StreamUtil.readInt(stream)
        ) else None,
        if (StreamUtil.readBoolean(stream)) Some(
            trans_gen_test.model.AutoAttack.readFrom(stream)
        ) else None
    )
}