package model

import util.StreamUtil

class BuildProperties {
    lateinit var options: Array<model.EntityType>
    var initHealth: Int? = null

    constructor(options: Array<model.EntityType>, initHealth: Int?) {
        this.options = options
        this.initHealth = initHealth
    }

    @Throws(java.io.IOException::class)
    fun writeTo(stream: java.io.OutputStream) {
        StreamUtil.writeInt(stream, options.size)
        for (optionsElement in options) {
            StreamUtil.writeInt(stream, optionsElement.tag)
        }
        val initHealthValue = initHealth
        if (initHealthValue == null) {
            StreamUtil.writeBoolean(stream, false)
        } else {
            StreamUtil.writeBoolean(stream, true)
            StreamUtil.writeInt(stream, initHealthValue)
        }
    }

    companion object {
        @Throws(java.io.IOException::class)
        fun readFrom(stream: java.io.InputStream): BuildProperties {
            var options: Array<model.EntityType>
            options = Array(StreamUtil.readInt(stream), {
                var optionsElement: model.EntityType
                when (StreamUtil.readInt(stream)) {
                0 -> optionsElement = model.EntityType.WALL
                1 -> optionsElement = model.EntityType.HOUSE
                2 -> optionsElement = model.EntityType.BUILDER_BASE
                3 -> optionsElement = model.EntityType.BUILDER_UNIT
                4 -> optionsElement = model.EntityType.MELEE_BASE
                5 -> optionsElement = model.EntityType.MELEE_UNIT
                6 -> optionsElement = model.EntityType.RANGED_BASE
                7 -> optionsElement = model.EntityType.RANGED_UNIT
                8 -> optionsElement = model.EntityType.RESOURCE
                9 -> optionsElement = model.EntityType.TURRET
                else -> throw java.io.IOException("Unexpected tag value")
                }
                optionsElement
            })
            var initHealth: Int?
            if (StreamUtil.readBoolean(stream)) {
                initHealth = StreamUtil.readInt(stream)
            } else {
                initHealth = null
            }
            return BuildProperties(options, initHealth)
        }
    }
}