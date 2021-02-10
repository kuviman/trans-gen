package trans_gen_test;

import trans_gen_test.util.StreamUtil;

/**
 * Oneof example
 */
public abstract class OneOf {
    /**
     * Write OneOf to output stream
     */
    public abstract void writeTo(java.io.OutputStream stream) throws java.io.IOException;

    /**
     * Read OneOf from input stream
     */
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

    /**
     * First option
     */
    public static class OptionOne extends OneOf {
        public static final int TAG = 0;
    
        /**
         * List of integers
         */
        private int[] vecInt;
    
        /**
         * List of integers
         */
        public int[] getVecInt() {
            return vecInt;
        }
    
        /**
         * List of integers
         */
        public void setVecInt(int[] value) {
            this.vecInt = value;
        }
        /**
         * Long integer
         */
        private long longInt;
    
        /**
         * Long integer
         */
        public long getLongInt() {
            return longInt;
        }
    
        /**
         * Long integer
         */
        public void setLongInt(long value) {
            this.longInt = value;
        }
    
        public OptionOne(int[] vecInt, long longInt) {
            this.vecInt = vecInt;
            this.longInt = longInt;
        }
    
        /**
         * Read OptionOne from input stream
         */
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
    
        /**
         * Write OptionOne to output stream
         */
        @Override
        public void writeTo(java.io.OutputStream stream) throws java.io.IOException {
            StreamUtil.writeInt(stream, TAG);
            StreamUtil.writeInt(stream, vecInt.length);
            for (int vecIntElement : vecInt) {
                StreamUtil.writeInt(stream, vecIntElement);
            }
            StreamUtil.writeLong(stream, longInt);
        }
    
        /**
         * Get string representation of OptionOne
         */
        @Override
        public String toString() {
            StringBuilder stringBuilder = new StringBuilder("OptionOne { ");
            stringBuilder.append("vecInt: ");
            stringBuilder.append("[ ");
            for (int vecIntIndex = 0; vecIntIndex < vecInt.length; vecIntIndex++) {
                if (vecIntIndex != 0) {
                    stringBuilder.append(", ");
                }
                int vecIntElement = vecInt[vecIntIndex];
                stringBuilder.append(String.valueOf(vecIntElement));
            }
            stringBuilder.append(" ]");
            stringBuilder.append(", ");
            stringBuilder.append("longInt: ");
            stringBuilder.append(String.valueOf(longInt));
            stringBuilder.append(" }");
            return stringBuilder.toString();
        }
    }

    /**
     * Second option
     */
    public static class OptionTwo extends OneOf {
        public static final int TAG = 1;
    
        /**
         * usize
         */
        private int value;
    
        /**
         * usize
         */
        public int getValue() {
            return value;
        }
    
        /**
         * usize
         */
        public void setValue(int value) {
            this.value = value;
        }
    
        public OptionTwo(int value) {
            this.value = value;
        }
    
        /**
         * Read OptionTwo from input stream
         */
        public static OptionTwo readFrom(java.io.InputStream stream) throws java.io.IOException {
            int value;
            value = StreamUtil.readInt(stream);
            return new OptionTwo(value);
        }
    
        /**
         * Write OptionTwo to output stream
         */
        @Override
        public void writeTo(java.io.OutputStream stream) throws java.io.IOException {
            StreamUtil.writeInt(stream, TAG);
            StreamUtil.writeInt(stream, value);
        }
    
        /**
         * Get string representation of OptionTwo
         */
        @Override
        public String toString() {
            StringBuilder stringBuilder = new StringBuilder("OptionTwo { ");
            stringBuilder.append("value: ");
            stringBuilder.append(String.valueOf(value));
            stringBuilder.append(" }");
            return stringBuilder.toString();
        }
    }
}