package model

import util.StreamUtil

case class Structure(text: String, floatNumber: Float, doubleNumber: Double) {
    def writeTo(stream: java.io.OutputStream) {
        StreamUtil.writeString(stream, text)
        StreamUtil.writeFloat(stream, floatNumber)
        StreamUtil.writeDouble(stream, doubleNumber)
    }
}

object Structure {
    def readFrom(stream: java.io.InputStream): Structure = Structure(
        StreamUtil.readString(stream),
        StreamUtil.readFloat(stream),
        StreamUtil.readDouble(stream)
    )
}