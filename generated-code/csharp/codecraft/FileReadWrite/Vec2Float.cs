namespace TransGenTest
{
    /// <summary>
    /// 2 dimensional vector.
    /// </summary>
    public struct Vec2Float
    {
        /// <summary>
        /// `x` coordinate of the vector
        /// </summary>
        public float X { get; set; }
        /// <summary>
        /// `y` coordinate of the vector
        /// </summary>
        public float Y { get; set; }
    
        public Vec2Float(float x, float y)
        {
            this.X = x;
            this.Y = y;
        }
    
        /// <summary> Read Vec2Float from reader </summary>
        public static Vec2Float ReadFrom(System.IO.BinaryReader reader)
        {
            var result = new Vec2Float();
            result.X = reader.ReadSingle();
            result.Y = reader.ReadSingle();
            return result;
        }
    
        /// <summary> Write Vec2Float to writer </summary>
        public void WriteTo(System.IO.BinaryWriter writer)
        {
            writer.Write(X);
            writer.Write(Y);
        }
    
        /// <summary> Get string representation of Vec2Float </summary>
        public override string ToString() {
            string stringResult = "Vec2Float { ";
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