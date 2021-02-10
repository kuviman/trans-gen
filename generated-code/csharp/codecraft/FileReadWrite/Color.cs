namespace TransGenTest
{
    /// <summary>
    /// RGBA Color
    /// </summary>
    public struct Color
    {
        /// <summary>
        /// Red component
        /// </summary>
        public float R { get; set; }
        /// <summary>
        /// Green component
        /// </summary>
        public float G { get; set; }
        /// <summary>
        /// Blue component
        /// </summary>
        public float B { get; set; }
        /// <summary>
        /// Alpha (opacity) component
        /// </summary>
        public float A { get; set; }
    
        public Color(float r, float g, float b, float a)
        {
            this.R = r;
            this.G = g;
            this.B = b;
            this.A = a;
        }
    
        /// <summary> Read Color from reader </summary>
        public static Color ReadFrom(System.IO.BinaryReader reader)
        {
            var result = new Color();
            result.R = reader.ReadSingle();
            result.G = reader.ReadSingle();
            result.B = reader.ReadSingle();
            result.A = reader.ReadSingle();
            return result;
        }
    
        /// <summary> Write Color to writer </summary>
        public void WriteTo(System.IO.BinaryWriter writer)
        {
            writer.Write(R);
            writer.Write(G);
            writer.Write(B);
            writer.Write(A);
        }
    
        /// <summary> Get string representation of Color </summary>
        public override string ToString() {
            string stringResult = "Color { ";
            stringResult += "R: ";
            stringResult += R.ToString();
            stringResult += ", ";
            stringResult += "G: ";
            stringResult += G.ToString();
            stringResult += ", ";
            stringResult += "B: ";
            stringResult += B.ToString();
            stringResult += ", ";
            stringResult += "A: ";
            stringResult += A.ToString();
            stringResult += " }";
            return stringResult;
        }
    }
}