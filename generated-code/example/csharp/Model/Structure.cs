namespace TransGenTest.Model
{
    public struct Structure
    {
        public string Text { get; set; }
        public float FloatNumber { get; set; }
        public double DoubleNumber { get; set; }
    
        public Structure(string text, float floatNumber, double doubleNumber)
        {
            this.Text = text;
            this.FloatNumber = floatNumber;
            this.DoubleNumber = doubleNumber;
        }
    
        public static Structure ReadFrom(System.IO.BinaryReader reader)
        {
            var result = new Structure();
            result.Text = System.Text.Encoding.UTF8.GetString(reader.ReadBytes(reader.ReadInt32()));
            result.FloatNumber = reader.ReadSingle();
            result.DoubleNumber = reader.ReadDouble();
            return result;
        }
    
        public void WriteTo(System.IO.BinaryWriter writer)
        {
            var textData = System.Text.Encoding.UTF8.GetBytes(Text);
            writer.Write(textData.Length);
            writer.Write(textData);
            writer.Write(FloatNumber);
            writer.Write(DoubleNumber);
        }
    }
}