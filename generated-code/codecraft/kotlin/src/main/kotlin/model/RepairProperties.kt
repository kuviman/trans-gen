package model

import util.StreamUtil

class RepairProperties {
    lateinit var validTargets: Array<model.EntityType>
    var power: Int = 0

    constructor(validTargets: Array<model.EntityType>, power: Int) {
        this.validTargets = validTargets
        this.power = power
    }

    @Throws(java.io.IOException::class)
    fun writeTo(stream: java.io.OutputStream) {
        StreamUtil.writeInt(stream, validTargets.size)
        for (validTargetsElement in validTargets) {
            StreamUtil.writeInt(stream, validTargetsElement.tag)
        }
        StreamUtil.writeInt(stream, power)
    }

    companion object {
        @Throws(java.io.IOException::class)
        fun readFrom(stream: java.io.InputStream): RepairProperties {
            var validTargets: Array<model.EntityType>
            validTargets = Array(StreamUtil.readInt(stream), {
                var validTargetsElement: model.EntityType
                when (StreamUtil.readInt(stream)) {
                0 -> validTargetsElement = model.EntityType.WALL
                1 -> validTargetsElement = model.EntityType.HOUSE
                2 -> validTargetsElement = model.EntityType.BUILDER_BASE
                3 -> validTargetsElement = model.EntityType.BUILDER_UNIT
                4 -> validTargetsElement = model.EntityType.MELEE_BASE
                5 -> validTargetsElement = model.EntityType.MELEE_UNIT
                6 -> validTargetsElement = model.EntityType.RANGED_BASE
                7 -> validTargetsElement = model.EntityType.RANGED_UNIT
                8 -> validTargetsElement = model.EntityType.RESOURCE
                9 -> validTargetsElement = model.EntityType.TURRET
                else -> throw java.io.IOException("Unexpected tag value")
                }
                validTargetsElement
            })
            var power: Int
            power = StreamUtil.readInt(stream)
            return RepairProperties(validTargets, power)
        }
    }
}