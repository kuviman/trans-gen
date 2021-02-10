package trans_gen_test.model.debug_interface

import trans_gen_test.util.StreamUtil

/**
 * Primitive type for debug rendering
 */
sealed abstract class PrimitiveType (val tag: Int) {
    /**
     * Write PrimitiveType to output stream
     */
    def writeTo(stream: java.io.OutputStream) {
        StreamUtil.writeInt(stream, tag)
    }
}

object PrimitiveType {
    /**
     * Lines, number of vertices should be divisible by 2
     */
    case object LINES extends PrimitiveType(0)
    /**
     * Triangles, number of vertices should be divisible by 3
     */
    case object TRIANGLES extends PrimitiveType(1)

    /**
     * Read PrimitiveType from input stream
     */
    def readFrom(stream: java.io.InputStream): PrimitiveType =
        StreamUtil.readInt(stream) match {
            case LINES.tag => LINES
            case TRIANGLES.tag => TRIANGLES
            case _ => throw new java.io.IOException("Unexpected tag value")
        }
}