namespace TransGenTest.Model
{
    /// <summary>
    /// 2 dimensional vector.
    /// </summary>
    public struct Vec2Int
    {
        /// <summary>
        /// `x` coordinate of the vector
        /// </summary>
        public int X { get; set; }
        /// <summary>
        /// `y` coordinate of the vector
        /// </summary>
        public int Y { get; set; }
    
        public Vec2Int(int x, int y)
        {
            this.X = x;
            this.Y = y;
        }
    
        /// <summary> Read Vec2Int from reader </summary>
        public static Vec2Int ReadFrom(System.IO.BinaryReader reader)
        {
            var result = new Vec2Int();
            result.X = reader.ReadInt32();
            result.Y = reader.ReadInt32();
            return result;
        }
    
        /// <summary> Write Vec2Int to writer </summary>
        public void WriteTo(System.IO.BinaryWriter writer)
        {
            writer.Write(X);
            writer.Write(Y);
        }
    
        /// <summary> Get string representation of Vec2Int </summary>
        public override string ToString() {
            string stringResult = "Vec2Int { ";
            stringResult += "X: ";
            stringResult += X.ToString();
            stringResult += ", ";
            stringResult += "Y: ";
            stringResult += Y.ToString();
            stringResult += " }";
            return stringResult;
        }
    }
}