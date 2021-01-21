package model;

import util.StreamUtil;

public enum Enumeration {
    VALUE_ONE(0),
    VALUE_TWO(1);

    public int tag;

    Enumeration(int tag) {
        this.tag = tag;
    }

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