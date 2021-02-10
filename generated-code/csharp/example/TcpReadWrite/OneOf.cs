namespace TransGenTest
{
    /// <summary>
    /// Oneof example
    /// </summary>
    public abstract class OneOf
    {
        /// <summary> Write OneOf to writer </summary>
        public abstract void WriteTo(System.IO.BinaryWriter writer);

        /// <summary> Read OneOf from reader </summary>
        public static OneOf ReadFrom(System.IO.BinaryReader reader)
        {
            switch (reader.ReadInt32())
            {
                case OptionOne.TAG:
                    return OptionOne.ReadFrom(reader);
                case OptionTwo.TAG:
                    return OptionTwo.ReadFrom(reader);
                default:
                    throw new System.Exception("Unexpected tag value");
            }
        }

        /// <summary>
        /// First option
        /// </summary>
        public class OptionOne : OneOf
        {
            public const int TAG = 0;
        
            /// <summary>
            /// List of integers
            /// </summary>
            public int[] VecInt { get; set; }
            /// <summary>
            /// Long integer
            /// </summary>
            public long LongInt { get; set; }
        
            public OptionOne() { }
        
            public OptionOne(int[] vecInt, long longInt)
            {
                this.VecInt = vecInt;
                this.LongInt = longInt;
            }
        
            /// <summary> Read OptionOne from reader </summary>
            public static new OptionOne ReadFrom(System.IO.BinaryReader reader)
            {
                var result = new OptionOne();
                result.VecInt = new int[reader.ReadInt32()];
                for (int vecIntIndex = 0; vecIntIndex < result.VecInt.Length; vecIntIndex++)
                {
                    result.VecInt[vecIntIndex] = reader.ReadInt32();
                }
                result.LongInt = reader.ReadInt64();
                return result;
            }
        
            /// <summary> Write OptionOne to writer </summary>
            public override void WriteTo(System.IO.BinaryWriter writer)
            {
                writer.Write(TAG);
                writer.Write(VecInt.Length);
                foreach (var vecIntElement in VecInt)
                {
                    writer.Write(vecIntElement);
                }
                writer.Write(LongInt);
            }
        
            /// <summary> Get string representation of OptionOne </summary>
            public override string ToString() {
                string stringResult = "OptionOne { ";
                stringResult += "VecInt: ";
                stringResult += "[ ";
                int vecIntIndex = 0;
                foreach (var vecIntElement in VecInt)
                {
                    if (vecIntIndex != 0) {
                        stringResult += ", ";
                    }
                    stringResult += vecIntElement.ToString();
                    vecIntIndex++;
                }
                stringResult += " ]";
                stringResult += ", ";
                stringResult += "LongInt: ";
                stringResult += LongInt.ToString();
                stringResult += " }";
                return stringResult;
            }
        }

        /// <summary>
        /// Second option
        /// </summary>
        public class OptionTwo : OneOf
        {
            public const int TAG = 1;
        
            /// <summary>
            /// usize
            /// </summary>
            public int Value { get; set; }
        
            public OptionTwo() { }
        
            public OptionTwo(int value)
            {
                this.Value = value;
            }
        
            /// <summary> Read OptionTwo from reader </summary>
            public static new OptionTwo ReadFrom(System.IO.BinaryReader reader)
            {
                var result = new OptionTwo();
                result.Value = reader.ReadInt32();
                return result;
            }
        
            /// <summary> Write OptionTwo to writer </summary>
            public override void WriteTo(System.IO.BinaryWriter writer)
            {
                writer.Write(TAG);
                writer.Write(Value);
            }
        
            /// <summary> Get string representation of OptionTwo </summary>
            public override string ToString() {
                string stringResult = "OptionTwo { ";
                stringResult += "Value: ";
                stringResult += Value.ToString();
                stringResult += " }";
                return stringResult;
            }
        }
    }
}