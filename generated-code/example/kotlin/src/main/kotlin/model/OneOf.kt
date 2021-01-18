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
        constructor() {}
        constructor(vecInt: Array<Int>, longInt: Long) {
            this.vecInt = vecInt
            this.longInt = longInt
        }
        companion object {
            val TAG = 0
            @Throws(java.io.IOException::class)
            fun readFrom(stream: java.io.InputStream): OptionOne {
                val result = OptionOne()
                result.vecInt = Array(StreamUtil.readInt(stream), {
                    var vecIntValue: Int
                    vecIntValue = StreamUtil.readInt(stream)
                    vecIntValue
                })
                result.longInt = StreamUtil.readLong(stream)
                return result
            }
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
    }

    class OptionTwo : OneOf {
        var value: Int = 0
        constructor() {}
        constructor(value: Int) {
            this.value = value
        }
        companion object {
            val TAG = 1
            @Throws(java.io.IOException::class)
            fun readFrom(stream: java.io.InputStream): OptionTwo {
                val result = OptionTwo()
                result.value = StreamUtil.readInt(stream)
                return result
            }
        }
        @Throws(java.io.IOException::class)
        override fun writeTo(stream: java.io.OutputStream) {
            StreamUtil.writeInt(stream, TAG)
            StreamUtil.writeInt(stream, value)
        }
    }
}
