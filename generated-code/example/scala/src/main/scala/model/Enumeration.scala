package model

import util.StreamUtil

sealed abstract class Enumeration (val tag: Int) {
    def writeTo(stream: java.io.OutputStream) {
        StreamUtil.writeInt(stream, tag)
    }
}

object Enumeration {
    case object VALUE_ONE extends Enumeration(0)
    case object VALUE_TWO extends Enumeration(1)
    def readFrom(stream: java.io.InputStream): Enumeration = StreamUtil.readInt(stream) match {
        case 0 => VALUE_ONE
        case 1 => VALUE_TWO
        case _ => throw new java.io.IOException("Unexpected tag value")
    }
}
