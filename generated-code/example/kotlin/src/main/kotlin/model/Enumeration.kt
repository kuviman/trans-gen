package model

import util.StreamUtil

enum class Enumeration private constructor(val tag: Int) {
    VALUE_ONE(0),
    VALUE_TWO(1);

    companion object {
        @Throws(java.io.IOException::class)
        fun readFrom(stream: java.io.InputStream): Enumeration {
            return when (StreamUtil.readInt(stream)) {
            VALUE_ONE.tag -> VALUE_ONE
            VALUE_TWO.tag -> VALUE_TWO
            else -> throw java.io.IOException("Unexpected tag value")
            }
        }
    }
}