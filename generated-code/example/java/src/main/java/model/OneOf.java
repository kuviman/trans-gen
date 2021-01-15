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
        private int[] value;
        public int[] getValue() { return value; }
        public void setValue(int[] value) { this.value = value; }
        public OptionOne() {}
        public OptionOne(int[] value) {
            this.value = value;
        }
        public static OptionOne readFrom(java.io.InputStream stream) throws java.io.IOException {
            OptionOne result = new OptionOne();
            result.value = new int[StreamUtil.readInt(stream)];
            for (int i = 0; i < result.value.length; i++) {
                result.value[i] = StreamUtil.readInt(stream);
            }
            return result;
        }
        @Override
        public void writeTo(java.io.OutputStream stream) throws java.io.IOException {
            StreamUtil.writeInt(stream, TAG);
            StreamUtil.writeInt(stream, value.length);
            for (int valueElement : value) {
                StreamUtil.writeInt(stream, valueElement);
            }
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
