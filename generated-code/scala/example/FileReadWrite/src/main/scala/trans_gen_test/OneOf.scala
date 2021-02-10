package trans_gen_test

import trans_gen_test.util.StreamUtil

/**
 * Oneof example
 */
sealed trait OneOf {
    /**
     * Write OneOf to output stream
     */
    def writeTo(stream: java.io.OutputStream)
}

object OneOf {
    /**
     * First option
     *
     * @param vecInt List of integers
     * @param longInt Long integer
     */
    case class OptionOne(vecInt: Seq[Int], longInt: Long) extends OneOf {
        /**
         * Write OptionOne to output stream
         */
        override def writeTo(stream: java.io.OutputStream) {
            StreamUtil.writeInt(stream, OptionOne.TAG)
            StreamUtil.writeInt(stream, vecInt.length)
            vecInt.foreach { value =>
                StreamUtil.writeInt(stream, value)
            }
            StreamUtil.writeLong(stream, longInt)
        }
    
        /**
         * Get string representation of OptionOne
         */
        override def toString(): String = {
            var stringBuilder = new StringBuilder("OptionOne { ")
            stringBuilder.append("vecInt: ")
            stringBuilder.append(vecInt)
            stringBuilder.append(", ")
            stringBuilder.append("longInt: ")
            stringBuilder.append(longInt)
            stringBuilder.append(" }")
            stringBuilder.toString()
        }
    }
    
    object OptionOne {
        val TAG: Int = 0
    
        /**
         * Read OptionOne from input stream
         */
        def readFrom(stream: java.io.InputStream): OptionOne = OptionOne(
            (0 until StreamUtil.readInt(stream)).map { _ =>
                StreamUtil.readInt(stream)
            },
            StreamUtil.readLong(stream)
        )
    }

    /**
     * Second option
     *
     * @param value usize
     */
    case class OptionTwo(value: Int) extends OneOf {
        /**
         * Write OptionTwo to output stream
         */
        override def writeTo(stream: java.io.OutputStream) {
            StreamUtil.writeInt(stream, OptionTwo.TAG)
            StreamUtil.writeInt(stream, value)
        }
    
        /**
         * Get string representation of OptionTwo
         */
        override def toString(): String = {
            var stringBuilder = new StringBuilder("OptionTwo { ")
            stringBuilder.append("value: ")
            stringBuilder.append(value)
            stringBuilder.append(" }")
            stringBuilder.toString()
        }
    }
    
    object OptionTwo {
        val TAG: Int = 1
    
        /**
         * Read OptionTwo from input stream
         */
        def readFrom(stream: java.io.InputStream): OptionTwo = OptionTwo(
            StreamUtil.readInt(stream)
        )
    }

    /**
     * Read OneOf from input stream
     */
    def readFrom(stream: java.io.InputStream): OneOf = {
        StreamUtil.readInt(stream) match {
            case OptionOne.TAG => OptionOne.readFrom(stream)
            case OptionTwo.TAG => OptionTwo.readFrom(stream)
            case _ => throw new java.io.IOException("Unexpected tag value")
        }
    }
}