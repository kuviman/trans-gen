package trans_gen_test;

import trans_gen_test.util.StreamUtil;

/**
 * Example enumeration
 */
public enum Enumeration {
    /**
     * First option
     */
    VALUE_ONE(0),
    /**
     * Second option
     */
    VALUE_TWO(1);

    public int tag;

    Enumeration(int tag) {
        this.tag = tag;
    }

    /**
     * Read Enumeration from input stream
     */
    public static Enumeration readFrom(java.io.InputStream stream) throws java.io.IOException {
        switch (StreamUtil.readInt(stream)) {
        case 0:
            return VALUE_ONE;
        case 1:
            return VALUE_TWO;
        default:
            throw new java.io.IOException("Unexpected tag value");
        }
    }
}