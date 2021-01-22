package model

import util.StreamUtil

class Structure {
    lateinit var text: String
    var floatNumber: Float = 0.0f
    var doubleNumber: Double = 0.0

    constructor(text: String, floatNumber: Float, doubleNumber: Double) {
        this.text = text
        this.floatNumber = floatNumber
        this.doubleNumber = doubleNumber
    }

    @Throws(java.io.IOException::class)
    fun writeTo(stream: java.io.OutputStream) {
        StreamUtil.writeString(stream, text)
        StreamUtil.writeFloat(stream, floatNumber)
        StreamUtil.writeDouble(stream, doubleNumber)
    }

    override fun toString(): String {
        var stringBuilder = StringBuilder("Structure { ")
        stringBuilder.append("text: ")
        stringBuilder.append('"' + text + '"')
        stringBuilder.append(", ")
        stringBuilder.append("floatNumber: ")
        stringBuilder.append(floatNumber)
        stringBuilder.append(", ")
        stringBuilder.append("doubleNumber: ")
        stringBuilder.append(doubleNumber)
        stringBuilder.append(" }")
        return stringBuilder.toString()
    }

    companion object {
        @Throws(java.io.IOException::class)
        fun readFrom(stream: java.io.InputStream): Structure {
            var text: String
            text = StreamUtil.readString(stream)
            var floatNumber: Float
            floatNumber = StreamUtil.readFloat(stream)
            var doubleNumber: Double
            doubleNumber = StreamUtil.readDouble(stream)
            return Structure(text, floatNumber, doubleNumber)
        }
    }
}