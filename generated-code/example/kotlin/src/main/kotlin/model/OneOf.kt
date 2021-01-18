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
        lateinit var vecI32: Array<Int>
        var longInt: Long = 0L
        constructor() {}
        constructor(vecI32: Array<Int>, longInt: Long) {
            this.vecI32 = vecI32
            this.longInt = longInt
        }
        companion object {
            val TAG = 0
            @Throws(java.io.IOException::class)
            fun readFrom(stream: java.io.InputStream): OptionOne {
                val result = OptionOne()
                result.vecI32 = Array(StreamUtil.readInt(stream), {
                    var vecI32Value: Int
                    vecI32Value = StreamUtil.readInt(stream)
                    vecI32Value
                })
                result.longInt = StreamUtil.readLong(stream)
                return result
            }
        }
        @Throws(java.io.IOException::class)
        override fun writeTo(stream: java.io.OutputStream) {
            StreamUtil.writeInt(stream, TAG)
            StreamUtil.writeInt(stream, vecI32.size)
            for (vecI32Element in vecI32) {
                StreamUtil.writeInt(stream, vecI32Element)
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
