package model

import util.StreamUtil

/**
 * Example structure
 *
 * @param text Text
 * @param floatNumber 32-bit float
 * @param doubleNumber 64-bit float
 */
case class Structure(text: String, floatNumber: Float, doubleNumber: Double) {
    /**
     * Write Structure to output stream
     */
    def writeTo(stream: java.io.OutputStream) {
        StreamUtil.writeString(stream, text)
        StreamUtil.writeFloat(stream, floatNumber)
        StreamUtil.writeDouble(stream, doubleNumber)
    }

    /**
     * Get string representation of Structure
     */
    override def toString(): String = {
        var stringBuilder = new StringBuilder("Structure { ")
        stringBuilder.append("text: ")
        stringBuilder.append('"' + text + '"')
        stringBuilder.append(", ")
        stringBuilder.append("floatNumber: ")
        stringBuilder.append(floatNumber)
        stringBuilder.append(", ")
        stringBuilder.append("doubleNumber: ")
        stringBuilder.append(doubleNumber)
        stringBuilder.append(" }")
        stringBuilder.toString()
    }
}

object Structure {
    /**
     * Read Structure from input stream
     */
    def readFrom(stream: java.io.InputStream): Structure = Structure(
        StreamUtil.readString(stream),
        StreamUtil.readFloat(stream),
        StreamUtil.readDouble(stream)
    )
}