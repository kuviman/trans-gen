package model;

import util.StreamUtil;

public abstract class OneOf {
    public abstract void writeTo(java.io.OutputStream stream) throws java.io.IOException;
    public static OneOf readFrom(java.io.InputStream stream) throws java.io.IOException {
        switch (StreamUtil.readInt(stream)) {
            case OptionOne.TAG:
                return OptionOne.readFrom(stream);
            case OptionTwo.TAG:
                return OptionTwo.readFrom(stream);
            default:
                throw new java.io.IOException("Unexpected tag value");
        }
    }

    public static class OptionOne extends OneOf {
        public static final int TAG = 0;
        private int[] vecI32;
        public int[] getVecI32() { return vecI32; }
        public void setVecI32(int[] vecI32) { this.vecI32 = vecI32; }
        private long longInt;
        public long getLongInt() { return longInt; }
        public void setLongInt(long longInt) { this.longInt = longInt; }
        public OptionOne() {}
        public OptionOne(int[] vecI32, long longInt) {
            this.vecI32 = vecI32;
            this.longInt = longInt;
        }
        public static OptionOne readFrom(java.io.InputStream stream) throws java.io.IOException {
            OptionOne result = new OptionOne();
            result.vecI32 = new int[StreamUtil.readInt(stream)];
            for (int i = 0; i < result.vecI32.length; i++) {
                result.vecI32[i] = StreamUtil.readInt(stream);
            }
            result.longInt = StreamUtil.readLong(stream);
            return result;
        }
        @Override
        public void writeTo(java.io.OutputStream stream) throws java.io.IOException {
            StreamUtil.writeInt(stream, TAG);
            StreamUtil.writeInt(stream, vecI32.length);
            for (int vecI32Element : vecI32) {
                StreamUtil.writeInt(stream, vecI32Element);
            }
            StreamUtil.writeLong(stream, longInt);
        }
    }

    public static class OptionTwo extends OneOf {
        public static final int TAG = 1;
        private int value;
        public int getValue() { return value; }
        public void setValue(int value) { this.value = value; }
        public OptionTwo() {}
        public OptionTwo(int value) {
            this.value = value;
        }
        public static OptionTwo readFrom(java.io.InputStream stream) throws java.io.IOException {
            OptionTwo result = new OptionTwo();
            result.value = StreamUtil.readInt(stream);
            return result;
        }
        @Override
        public void writeTo(java.io.OutputStream stream) throws java.io.IOException {
            StreamUtil.writeInt(stream, TAG);
            StreamUtil.writeInt(stream, value);
        }
    }
}
