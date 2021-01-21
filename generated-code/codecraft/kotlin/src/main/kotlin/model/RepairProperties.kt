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
                validTargetsElement = model.EntityType.readFrom(stream)
                validTargetsElement
            })
            var power: Int
            power = StreamUtil.readInt(stream)
            return RepairProperties(validTargets, power)
        }
    }
}