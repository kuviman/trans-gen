package model

import util.StreamUtil

sealed trait OneOf {
    def writeTo(stream: java.io.OutputStream)
}
object OneOf {
    case class OptionOne(vecI32: Seq[Int], longInt: Long) extends OneOf {
        override def writeTo(stream: java.io.OutputStream) {
            StreamUtil.writeInt(stream, OptionOne.TAG)
            StreamUtil.writeInt(stream, vecI32.length)
            vecI32.foreach { value =>
                StreamUtil.writeInt(stream, value)
            }
            StreamUtil.writeLong(stream, longInt)
        }
    }
    object OptionOne {
        val TAG: Int = 0
        def readFrom(stream: java.io.InputStream): OptionOne = OptionOne(
            (0 until StreamUtil.readInt(stream)).map { _ =>
                StreamUtil.readInt(stream)
            }
            ,
            StreamUtil.readLong(stream)
            )
    }

    case class OptionTwo(value: Int) extends OneOf {
        override def writeTo(stream: java.io.OutputStream) {
            StreamUtil.writeInt(stream, OptionTwo.TAG)
            StreamUtil.writeInt(stream, value)
        }
    }
    object OptionTwo {
        val TAG: Int = 1
        def readFrom(stream: java.io.InputStream): OptionTwo = OptionTwo(
            StreamUtil.readInt(stream)
            )
    }

    def readFrom(stream: java.io.InputStream): OneOf = {
        StreamUtil.readInt(stream) match {
            case OptionOne.TAG => OptionOne.readFrom(stream)
            case OptionTwo.TAG => OptionTwo.readFrom(stream)
            case _ => throw new java.io.IOException("Unexpected tag value")
        }
    }
}
