package trans_gen_test

import trans_gen_test.util.StreamUtil

/**
 * Example structure
 */
class Structure {
    /**
     * Text
     */
    var text: String
    /**
     * 32-bit float
     */
    var floatNumber: Float
    /**
     * 64-bit float
     */
    var doubleNumber: Double

    constructor(text: String, floatNumber: Float, doubleNumber: Double) {
        this.text = text
        this.floatNumber = floatNumber
        this.doubleNumber = doubleNumber
    }

    /**
     * Write Structure to output stream
     */
    @Throws(java.io.IOException::class)
    fun writeTo(stream: java.io.OutputStream) {
        StreamUtil.writeString(stream, text)
        StreamUtil.writeFloat(stream, floatNumber)
        StreamUtil.writeDouble(stream, doubleNumber)
    }

    /**
     * Get string representation of Structure
     */
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
        /**
         * Read Structure from input stream
         */
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