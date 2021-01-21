namespace TransGenTest.Model
{
    public enum Enumeration
    {
        ValueOne = 0,
        ValueTwo = 1,
    }

    public static class EnumerationHelper {
        public static Enumeration ReadFrom(System.IO.BinaryReader reader) {
            switch (reader.ReadInt32())
            {
                case 0:
                    return Enumeration.ValueOne;
                case 1:
                    return Enumeration.ValueTwo;
                default:
                    throw new System.Exception("Unexpected tag value");
            }
        }
    }
}