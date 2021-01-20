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
    
        private int[] vecInt;
    
        public int[] getVecInt() {
            return vecInt;
        }
    
        public void setVecInt(int[] value) {
            this.vecInt = value;
        }
        private long longInt;
    
        public long getLongInt() {
            return longInt;
        }
    
        public void setLongInt(long value) {
            this.longInt = value;
        }
    
        public OptionOne(int[] vecInt, long longInt) {
            this.vecInt = vecInt;
            this.longInt = longInt;
        }
    
        public static OptionOne readFrom(java.io.InputStream stream) throws java.io.IOException {
            int[] vecInt;
            vecInt = new int[StreamUtil.readInt(stream)];
            for (int vecIntIndex = 0; vecIntIndex < vecInt.length; vecIntIndex++) {
                int vecIntElement;
                vecIntElement = StreamUtil.readInt(stream);
                vecInt[vecIntIndex] = vecIntElement;
            }
            long longInt;
            longInt = StreamUtil.readLong(stream);
            return new OptionOne(vecInt, longInt);
        }
    
        @Override
        public void writeTo(java.io.OutputStream stream) throws java.io.IOException {
            StreamUtil.writeInt(stream, TAG);
            StreamUtil.writeInt(stream, vecInt.length);
            for (int vecIntElement : vecInt) {
                StreamUtil.writeInt(stream, vecIntElement);
            }
            StreamUtil.writeLong(stream, longInt);
        }
    }

    public static class OptionTwo extends OneOf {
        public static final int TAG = 1;
    
        private int value;
    
        public int getValue() {
            return value;
        }
    
        public void setValue(int value) {
            this.value = value;
        }
    
        public OptionTwo(int value) {
            this.value = value;
        }
    
        public static OptionTwo readFrom(java.io.InputStream stream) throws java.io.IOException {
            int value;
            value = StreamUtil.readInt(stream);
            return new OptionTwo(value);
        }
    
        @Override
        public void writeTo(java.io.OutputStream stream) throws java.io.IOException {
            StreamUtil.writeInt(stream, TAG);
            StreamUtil.writeInt(stream, value);
        }
    }
}