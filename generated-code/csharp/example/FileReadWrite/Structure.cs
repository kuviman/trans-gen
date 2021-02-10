namespace TransGenTest
{
    /// <summary>
    /// Example structure
    /// </summary>
    public struct Structure
    {
        /// <summary>
        /// Text
        /// </summary>
        public string Text { get; set; }
        /// <summary>
        /// 32-bit float
        /// </summary>
        public float FloatNumber { get; set; }
        /// <summary>
        /// 64-bit float
        /// </summary>
        public double DoubleNumber { get; set; }
    
        public Structure(string text, float floatNumber, double doubleNumber)
        {
            this.Text = text;
            this.FloatNumber = floatNumber;
            this.DoubleNumber = doubleNumber;
        }
    
        /// <summary> Read Structure from reader </summary>
        public static Structure ReadFrom(System.IO.BinaryReader reader)
        {
            var result = new Structure();
            result.Text = System.Text.Encoding.UTF8.GetString(reader.ReadBytes(reader.ReadInt32()));
            result.FloatNumber = reader.ReadSingle();
            result.DoubleNumber = reader.ReadDouble();
            return result;
        }
    
        /// <summary> Write Structure to writer </summary>
        public void WriteTo(System.IO.BinaryWriter writer)
        {
            var textData = System.Text.Encoding.UTF8.GetBytes(Text);
            writer.Write(textData.Length);
            writer.Write(textData);
            writer.Write(FloatNumber);
            writer.Write(DoubleNumber);
        }
    
        /// <summary> Get string representation of Structure </summary>
        public override string ToString() {
            string stringResult = "Structure { ";
            stringResult += "Text: ";
            stringResult += "\"" + Text + "\"";
            stringResult += ", ";
            stringResult += "FloatNumber: ";
            stringResult += FloatNumber.ToString();
            stringResult += ", ";
            stringResult += "DoubleNumber: ";
            stringResult += DoubleNumber.ToString();
            stringResult += " }";
            return stringResult;
        }
    }
}