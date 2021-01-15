namespace TransGenTest.Model
{
    public abstract class OneOf
    {
        public abstract void WriteTo(System.IO.BinaryWriter writer);
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

        public class OptionOne : OneOf
        {
            public const int TAG = 0;
            public int[] Value { get; set; }
            public OptionOne() {}
            public OptionOne(int[] value)
            {
                this.Value = value;
            }
            public static new OptionOne ReadFrom(System.IO.BinaryReader reader)
            {
                var result = new OptionOne();
                result.Value = new int[reader.ReadInt32()];
                for (int i = 0; i < result.Value.Length; i++)
                {
                    result.Value[i] = reader.ReadInt32();
                }
                return result;
            }
            public override void WriteTo(System.IO.BinaryWriter writer)
            {
                writer.Write(TAG);
                writer.Write(Value.Length);
                foreach (var ValueElement in Value)
                {
                    writer.Write(ValueElement);
                }
            }
        }

        public class OptionTwo : OneOf
        {
            public const int TAG = 1;
            public int Value { get; set; }
            public OptionTwo() {}
            public OptionTwo(int value)
            {
                this.Value = value;
            }
            public static new OptionTwo ReadFrom(System.IO.BinaryReader reader)
            {
                var result = new OptionTwo();
                result.Value = reader.ReadInt32();
                return result;
            }
            public override void WriteTo(System.IO.BinaryWriter writer)
            {
                writer.Write(TAG);
                writer.Write(Value);
            }
        }
    }
}
