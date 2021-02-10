package trans_gen_test.model

import trans_gen_test.util.StreamUtil

/**
 * Attack action
 */
class AttackAction {
    /**
     * If specified, target entity's ID
     */
    var target: Int?
    /**
     * If specified, configures auto attacking
     */
    var autoAttack: trans_gen_test.model.AutoAttack?

    constructor(target: Int?, autoAttack: trans_gen_test.model.AutoAttack?) {
        this.target = target
        this.autoAttack = autoAttack
    }

    /**
     * Write AttackAction to output stream
     */
    @Throws(java.io.IOException::class)
    fun writeTo(stream: java.io.OutputStream) {
        val targetValue = target
        if (targetValue == null) {
            StreamUtil.writeBoolean(stream, false)
        } else {
            StreamUtil.writeBoolean(stream, true)
            StreamUtil.writeInt(stream, targetValue)
        }
        val autoAttackValue = autoAttack
        if (autoAttackValue == null) {
            StreamUtil.writeBoolean(stream, false)
        } else {
            StreamUtil.writeBoolean(stream, true)
            autoAttackValue.writeTo(stream)
        }
    }

    /**
     * Get string representation of AttackAction
     */
    override fun toString(): String {
        var stringBuilder = StringBuilder("AttackAction { ")
        stringBuilder.append("target: ")
        stringBuilder.append(target)
        stringBuilder.append(", ")
        stringBuilder.append("autoAttack: ")
        stringBuilder.append(autoAttack)
        stringBuilder.append(" }")
        return stringBuilder.toString()
    }

    companion object {
        /**
         * Read AttackAction from input stream
         */
        @Throws(java.io.IOException::class)
        fun readFrom(stream: java.io.InputStream): AttackAction {
            var target: Int?
            if (StreamUtil.readBoolean(stream)) {
                target = StreamUtil.readInt(stream)
            } else {
                target = null
            }
            var autoAttack: trans_gen_test.model.AutoAttack?
            if (StreamUtil.readBoolean(stream)) {
                autoAttack = trans_gen_test.model.AutoAttack.readFrom(stream)
            } else {
                autoAttack = null
            }
            return AttackAction(target, autoAttack)
        }
    }
}