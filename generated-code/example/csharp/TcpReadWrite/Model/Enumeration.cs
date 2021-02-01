namespace TransGenTest.Model
{
    /// <summary>
    /// Example enumeration
    /// </summary>
    public enum Enumeration
    {
        /// <summary>
        /// First option
        /// </summary>
        ValueOne = 0,
        /// <summary>
        /// Second option
        /// </summary>
        ValueTwo = 1,
    }

    public static class EnumerationHelper {
        /// <summary> Read Enumeration from reader </summary>
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