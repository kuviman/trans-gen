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
        lateinit var value: Array<Int>
        constructor() {}
        constructor(value: Array<Int>) {
            this.value = value
        }
        companion object {
            val TAG = 0
            @Throws(java.io.IOException::class)
            fun readFrom(stream: java.io.InputStream): OptionOne {
                val result = OptionOne()
                result.value = Array(StreamUtil.readInt(stream), {
                    var valueValue: Int
                    valueValue = StreamUtil.readInt(stream)
                    valueValue
                })
                return result
            }
        }
        @Throws(java.io.IOException::class)
        override fun writeTo(stream: java.io.OutputStream) {
            StreamUtil.writeInt(stream, TAG)
            StreamUtil.writeInt(stream, value.size)
            for (valueElement in value) {
                StreamUtil.writeInt(stream, valueElement)
            }
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
