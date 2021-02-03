package model

import util.StreamUtil

/**
 * Oneof example
 */
abstract class OneOf {
    /**
     * Write OneOf to output stream
     */
    @Throws(java.io.IOException::class)
    abstract fun writeTo(stream: java.io.OutputStream)

    companion object {
        /**
         * Read OneOf from input stream
         */
        @Throws(java.io.IOException::class)
        fun readFrom(stream: java.io.InputStream): OneOf {
            when (StreamUtil.readInt(stream)) {
                OptionOne.TAG -> return OptionOne.readFrom(stream)
                OptionTwo.TAG -> return OptionTwo.readFrom(stream)
                else -> throw java.io.IOException("Unexpected tag value")
            }
        }
    }

    /**
     * First option
     */
    class OptionOne : OneOf {
        /**
         * List of integers
         */
        lateinit var vecInt: Array<Int>
        /**
         * Long integer
         */
        var longInt: Long = 0L
    
        constructor(vecInt: Array<Int>, longInt: Long) {
            this.vecInt = vecInt
            this.longInt = longInt
        }
    
        /**
         * Write OptionOne to output stream
         */
        @Throws(java.io.IOException::class)
        override fun writeTo(stream: java.io.OutputStream) {
            StreamUtil.writeInt(stream, TAG)
            StreamUtil.writeInt(stream, vecInt.size)
            for (vecIntElement in vecInt) {
                StreamUtil.writeInt(stream, vecIntElement)
            }
            StreamUtil.writeLong(stream, longInt)
        }
    
        /**
         * Get string representation of OptionOne
         */
        override fun toString(): String {
            var stringBuilder = StringBuilder("OptionOne { ")
            stringBuilder.append("vecInt: ")
            stringBuilder.append("[ ")
            var vecIntIndex = 0
            for (vecIntElement in vecInt) {
                if (vecIntIndex != 0) {
                    stringBuilder.append(", ")
                }
                stringBuilder.append(vecIntElement)
                vecIntIndex++
            }
            stringBuilder.append(" ]")
            stringBuilder.append(", ")
            stringBuilder.append("longInt: ")
            stringBuilder.append(longInt)
            stringBuilder.append(" }")
            return stringBuilder.toString()
        }
    
        companion object {
            val TAG = 0
    
            /**
             * Read OptionOne from input stream
             */
            @Throws(java.io.IOException::class)
            fun readFrom(stream: java.io.InputStream): OptionOne {
                var vecInt: Array<Int>
                vecInt = Array(StreamUtil.readInt(stream), {
                    var vecIntElement: Int
                    vecIntElement = StreamUtil.readInt(stream)
                    vecIntElement
                })
                var longInt: Long
                longInt = StreamUtil.readLong(stream)
                return OptionOne(vecInt, longInt)
            }
        }
    }

    /**
     * Second option
     */
    class OptionTwo : OneOf {
        /**
         * usize
         */
        var value: Int = 0
    
        constructor(value: Int) {
            this.value = value
        }
    
        /**
         * Write OptionTwo to output stream
         */
        @Throws(java.io.IOException::class)
        override fun writeTo(stream: java.io.OutputStream) {
            StreamUtil.writeInt(stream, TAG)
            StreamUtil.writeInt(stream, value)
        }
    
        /**
         * Get string representation of OptionTwo
         */
        override fun toString(): String {
            var stringBuilder = StringBuilder("OptionTwo { ")
            stringBuilder.append("value: ")
            stringBuilder.append(value)
            stringBuilder.append(" }")
            return stringBuilder.toString()
        }
    
        companion object {
            val TAG = 1
    
            /**
             * Read OptionTwo from input stream
             */
            @Throws(java.io.IOException::class)
            fun readFrom(stream: java.io.InputStream): OptionTwo {
                var value: Int
                value = StreamUtil.readInt(stream)
                return OptionTwo(value)
            }
        }
    }
}