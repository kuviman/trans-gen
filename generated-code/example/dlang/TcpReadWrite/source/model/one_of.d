import model;
import stream;
import std.conv;
import std.typecons : Nullable;

/// Oneof example
abstract class OneOf {
    /// Write OneOf to writer
    abstract void writeTo(Stream writer) const;

    /// Read OneOf from reader
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

    /// First option
    static class OptionOne : OneOf {
        static const int TAG = 0;
    
        /// List of integers
        int[] vecInt;
        /// Long integer
        long longInt;
    
        this() {}
    
        this(int[] vecInt, long longInt) {
            this.vecInt = vecInt;
            this.longInt = longInt;
        }
    
        /// Read OptionOne from reader
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
    
        /// Write OptionOne to writer
        override void writeTo(Stream writer) const {
            writer.write(TAG);
            writer.write(cast(int)(vecInt.length));
            foreach (vecIntElement; vecInt) {
                writer.write(vecIntElement);
            }
            writer.write(longInt);
        }
    }

    /// Second option
    static class OptionTwo : OneOf {
        static const int TAG = 1;
    
        /// usize
        int value;
    
        this() {}
    
        this(int value) {
            this.value = value;
        }
    
        /// Read OptionTwo from reader
        static OptionTwo readFrom(Stream reader) {
            int value;
            value = reader.readInt();
            return new OptionTwo(value);
        }
    
        /// Write OptionTwo to writer
        override void writeTo(Stream writer) const {
            writer.write(TAG);
            writer.write(value);
        }
    }
}