package trans_gen_test.model.debug_interface;

import trans_gen_test.util.StreamUtil;

/**
 * Primitive type for debug rendering
 */
public enum PrimitiveType {
    /**
     * Lines, number of vertices should be divisible by 2
     */
    LINES(0),
    /**
     * Triangles, number of vertices should be divisible by 3
     */
    TRIANGLES(1);

    public int tag;

    PrimitiveType(int tag) {
        this.tag = tag;
    }

    /**
     * Read PrimitiveType from input stream
     */
    public static PrimitiveType readFrom(java.io.InputStream stream) throws java.io.IOException {
        switch (StreamUtil.readInt(stream)) {
        case 0:
            return LINES;
        case 1:
            return TRIANGLES;
        default:
            throw new java.io.IOException("Unexpected tag value");
        }
    }
}