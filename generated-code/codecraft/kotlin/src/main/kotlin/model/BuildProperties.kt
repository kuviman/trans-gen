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

    override fun toString(): String {
        var stringBuilder = StringBuilder("BuildProperties { ")
        stringBuilder.append("options: ")
        stringBuilder.append("[ ")
        var optionsIndex = 0
        for (optionsElement in options) {
            if (optionsIndex != 0) {
                stringBuilder.append(", ")
            }
            stringBuilder.append(optionsElement)
            optionsIndex++
        }
        stringBuilder.append(" ]")
        stringBuilder.append(", ")
        stringBuilder.append("initHealth: ")
        stringBuilder.append(initHealth)
        stringBuilder.append(" }")
        return stringBuilder.toString()
    }

    companion object {
        @Throws(java.io.IOException::class)
        fun readFrom(stream: java.io.InputStream): BuildProperties {
            var options: Array<model.EntityType>
            options = Array(StreamUtil.readInt(stream), {
                var optionsElement: model.EntityType
                optionsElement = model.EntityType.readFrom(stream)
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