package trans_gen_test.model.debug_interface

import trans_gen_test.util.StreamUtil

/**
 * Primitive type for debug rendering
 */
enum class PrimitiveType private constructor(val tag: Int) {
    /**
     * Lines, number of vertices should be divisible by 2
     */
    LINES(0),
    /**
     * Triangles, number of vertices should be divisible by 3
     */
    TRIANGLES(1);

    companion object {
        /**
         * Read PrimitiveType from input stream
         */
        @Throws(java.io.IOException::class)
        fun readFrom(stream: java.io.InputStream): PrimitiveType {
            return when (StreamUtil.readInt(stream)) {
            LINES.tag -> LINES
            TRIANGLES.tag -> TRIANGLES
            else -> throw java.io.IOException("Unexpected tag value")
            }
        }
    }
}