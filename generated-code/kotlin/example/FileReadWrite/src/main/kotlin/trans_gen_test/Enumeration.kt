package trans_gen_test

import trans_gen_test.util.StreamUtil

/**
 * Example enumeration
 */
enum class Enumeration private constructor(val tag: Int) {
    /**
     * First option
     */
    VALUE_ONE(0),
    /**
     * Second option
     */
    VALUE_TWO(1);

    companion object {
        /**
         * Read Enumeration from input stream
         */
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