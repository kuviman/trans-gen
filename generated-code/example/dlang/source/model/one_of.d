import model;
import stream;
import std.conv;
import std.typecons : Nullable;

abstract class OneOf {
    abstract void writeTo(Stream writer) const;
    static OneOf readFrom(Stream reader) {
        switch (reader.readInt()) {
            case OptionOne.TAG:
                return OptionOne.readFrom(reader);
            case OptionTwo.TAG:
                return OptionTwo.readFrom(reader);
            default:
                throw new Exception("Unexpected tag value");
        }
    }

    static class OptionOne : OneOf {
        static const int TAG = 0;
        int[] vecI32;
        long longInt;
        this() {}
        this(int[] vecI32, long longInt) {
            this.vecI32 = vecI32;
            this.longInt = longInt;
        }
        static OptionOne readFrom(Stream reader) {
            auto result = new OptionOne();
            result.vecI32 = new int[reader.readInt()];
            for (int i = 0; i < result.vecI32.length; i++) {
                result.vecI32[i] = reader.readInt();
            }
            result.longInt = reader.readLong();
            return result;
        }
        override void writeTo(Stream writer) const {
            writer.write(TAG);
            writer.write(cast(int)(vecI32.length));
            foreach (vecI32Element; vecI32) {
                writer.write(vecI32Element);
            }
            writer.write(longInt);
        }
        override string toString() const {
            return "OptionOne" ~ "(" ~
                to!string(vecI32) ~
                to!string(longInt) ~
                ")";
        }
    }

    static class OptionTwo : OneOf {
        static const int TAG = 1;
        int value;
        this() {}
        this(int value) {
            this.value = value;
        }
        static OptionTwo readFrom(Stream reader) {
            auto result = new OptionTwo();
            result.value = reader.readInt();
            return result;
        }
        override void writeTo(Stream writer) const {
            writer.write(TAG);
            writer.write(value);
        }
        override string toString() const {
            return "OptionTwo" ~ "(" ~
                to!string(value) ~
                ")";
        }
    }
}
