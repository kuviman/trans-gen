package model

import util.StreamUtil

abstract class OneOf {
    @Throws(java.io.IOException::class)
    abstract fun writeTo(stream: java.io.OutputStream)

    companion object {
        @Throws(java.io.IOException::class)
        fun readFrom(stream: java.io.InputStream): OneOf {
            when (StreamUtil.readInt(stream)) {
                OptionOne.TAG -> return OptionOne.readFrom(stream)
                OptionTwo.TAG -> return OptionTwo.readFrom(stream)
                else -> throw java.io.IOException("Unexpected tag value")
            }
        }
    }

    class OptionOne : OneOf {
        lateinit var vecInt: Array<Int>
        var longInt: Long = 0L
    
        constructor(vecInt: Array<Int>, longInt: Long) {
            this.vecInt = vecInt
            this.longInt = longInt
        }
    
        @Throws(java.io.IOException::class)
        override fun writeTo(stream: java.io.OutputStream) {
            StreamUtil.writeInt(stream, TAG)
            StreamUtil.writeInt(stream, vecInt.size)
            for (vecIntElement in vecInt) {
                StreamUtil.writeInt(stream, vecIntElement)
            }
            StreamUtil.writeLong(stream, longInt)
        }
    
        companion object {
            val TAG = 0
    
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

    class OptionTwo : OneOf {
        var value: Int = 0
    
        constructor(value: Int) {
            this.value = value
        }
    
        @Throws(java.io.IOException::class)
        override fun writeTo(stream: java.io.OutputStream) {
            StreamUtil.writeInt(stream, TAG)
            StreamUtil.writeInt(stream, value)
        }
    
        companion object {
            val TAG = 1
    
            @Throws(java.io.IOException::class)
            fun readFrom(stream: java.io.InputStream): OptionTwo {
                var value: Int
                value = StreamUtil.readInt(stream)
                return OptionTwo(value)
            }
        }
    }
}