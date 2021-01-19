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
    
        int[] vecInt;
        long longInt;
    
        this() {}
    
        this(int[] vecInt, long longInt) {
            this.vecInt = vecInt;
            this.longInt = longInt;
        }
    
        static OptionOne readFrom(Stream reader) {
            int[] vecInt;
            vecInt = new int[reader.readInt()];
            for (int vecIntIndex = 0; vecIntIndex < vecInt.length; vecIntIndex++) {
                int vecIntKey;
                vecIntKey = reader.readInt();
                vecInt[vecIntIndex] = vecIntKey;
            }
            long longInt;
            longInt = reader.readLong();
            return new OptionOne(vecInt, longInt);
        }
    
        override void writeTo(Stream writer) const {
            writer.write(TAG);
            writer.write(cast(int)(vecInt.length));
            foreach (vecIntElement; vecInt) {
                writer.write(vecIntElement);
            }
            writer.write(longInt);
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
            int value;
            value = reader.readInt();
            return new OptionTwo(value);
        }
    
        override void writeTo(Stream writer) const {
            writer.write(TAG);
            writer.write(value);
        }
    }
}