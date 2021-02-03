package model

import util.StreamUtil

/**
 * Example enumeration
 */
sealed abstract class Enumeration (val tag: Int) {
    /**
     * Write Enumeration to output stream
     */
    def writeTo(stream: java.io.OutputStream) {
        StreamUtil.writeInt(stream, tag)
    }
}

object Enumeration {
    /**
     * First option
     */
    case object VALUE_ONE extends Enumeration(0)
    /**
     * Second option
     */
    case object VALUE_TWO extends Enumeration(1)

    /**
     * Read Enumeration from input stream
     */
    def readFrom(stream: java.io.InputStream): Enumeration =
        StreamUtil.readInt(stream) match {
            case VALUE_ONE.tag => VALUE_ONE
            case VALUE_TWO.tag => VALUE_TWO
            case _ => throw new java.io.IOException("Unexpected tag value")
        }
}