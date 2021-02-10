package trans_gen_test.model

import trans_gen_test.util.StreamUtil

/**
 * Auto attack options
 */
class AutoAttack {
    /**
     * Maximum distance to pathfind
     */
    var pathfindRange: Int
    /**
     * List of target entity types to try to attack. If empty, all types but resource are considered
     */
    var validTargets: Array<trans_gen_test.model.EntityType>

    constructor(pathfindRange: Int, validTargets: Array<trans_gen_test.model.EntityType>) {
        this.pathfindRange = pathfindRange
        this.validTargets = validTargets
    }

    /**
     * Write AutoAttack to output stream
     */
    @Throws(java.io.IOException::class)
    fun writeTo(stream: java.io.OutputStream) {
        StreamUtil.writeInt(stream, pathfindRange)
        StreamUtil.writeInt(stream, validTargets.size)
        for (validTargetsElement in validTargets) {
            StreamUtil.writeInt(stream, validTargetsElement.tag)
        }
    }

    /**
     * Get string representation of AutoAttack
     */
    override fun toString(): String {
        var stringBuilder = StringBuilder("AutoAttack { ")
        stringBuilder.append("pathfindRange: ")
        stringBuilder.append(pathfindRange)
        stringBuilder.append(", ")
        stringBuilder.append("validTargets: ")
        stringBuilder.append("[ ")
        var validTargetsIndex = 0
        for (validTargetsElement in validTargets) {
            if (validTargetsIndex != 0) {
                stringBuilder.append(", ")
            }
            stringBuilder.append(validTargetsElement)
            validTargetsIndex++
        }
        stringBuilder.append(" ]")
        stringBuilder.append(" }")
        return stringBuilder.toString()
    }

    companion object {
        /**
         * Read AutoAttack from input stream
         */
        @Throws(java.io.IOException::class)
        fun readFrom(stream: java.io.InputStream): AutoAttack {
            var pathfindRange: Int
            pathfindRange = StreamUtil.readInt(stream)
            var validTargets: Array<trans_gen_test.model.EntityType>
            validTargets = Array(StreamUtil.readInt(stream), {
                var validTargetsElement: trans_gen_test.model.EntityType
                validTargetsElement = trans_gen_test.model.EntityType.readFrom(stream)
                validTargetsElement
            })
            return AutoAttack(pathfindRange, validTargets)
        }
    }
}