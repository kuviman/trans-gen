namespace TransGenTest.Model.DebugInterface
{
    /// <summary>
    /// Primitive type for debug rendering
    /// </summary>
    public enum PrimitiveType
    {
        /// <summary>
        /// Lines, number of vertices should be divisible by 2
        /// </summary>
        Lines = 0,
        /// <summary>
        /// Triangles, number of vertices should be divisible by 3
        /// </summary>
        Triangles = 1,
    }

    public static class PrimitiveTypeHelper {
        /// <summary> Read PrimitiveType from reader </summary>
        public static PrimitiveType ReadFrom(System.IO.BinaryReader reader) {
            switch (reader.ReadInt32())
            {
                case 0:
                    return PrimitiveType.Lines;
                case 1:
                    return PrimitiveType.Triangles;
                default:
                    throw new System.Exception("Unexpected tag value");
            }
        }
    }
}