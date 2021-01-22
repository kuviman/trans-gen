namespace TransGenTest.Model
{
    public struct Vec2Int
    {
        public int X { get; set; }
        public int Y { get; set; }
    
        public Vec2Int(int x, int y)
        {
            this.X = x;
            this.Y = y;
        }
    
        public static Vec2Int ReadFrom(System.IO.BinaryReader reader)
        {
            var result = new Vec2Int();
            result.X = reader.ReadInt32();
            result.Y = reader.ReadInt32();
            return result;
        }
    
        public void WriteTo(System.IO.BinaryWriter writer)
        {
            writer.Write(X);
            writer.Write(Y);
        }
    
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