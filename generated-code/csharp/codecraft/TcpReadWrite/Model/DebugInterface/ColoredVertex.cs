namespace TransGenTest.Model.DebugInterface
{
    /// <summary>
    /// Vertex for debug rendering
    /// </summary>
    public struct ColoredVertex
    {
        /// <summary>
        /// Position in world coordinates (if none, screen position (0, 0) is used)
        /// </summary>
        public Vec2Float? WorldPos { get; set; }
        /// <summary>
        /// Additional offset in screen coordinates
        /// </summary>
        public Vec2Float ScreenOffset { get; set; }
        /// <summary>
        /// Color to use
        /// </summary>
        public Color Color { get; set; }
    
        public ColoredVertex(Vec2Float? worldPos, Vec2Float screenOffset, Color color)
        {
            this.WorldPos = worldPos;
            this.ScreenOffset = screenOffset;
            this.Color = color;
        }
    
        /// <summary> Read ColoredVertex from reader </summary>
        public static ColoredVertex ReadFrom(System.IO.BinaryReader reader)
        {
            var result = new ColoredVertex();
            if (reader.ReadBoolean())
            {
                result.WorldPos = Vec2Float.ReadFrom(reader);
            } else
            {
                result.WorldPos = null;
            }
            result.ScreenOffset = Vec2Float.ReadFrom(reader);
            result.Color = Color.ReadFrom(reader);
            return result;
        }
    
        /// <summary> Write ColoredVertex to writer </summary>
        public void WriteTo(System.IO.BinaryWriter writer)
        {
            if (!WorldPos.HasValue)
            {
                writer.Write(false);
            } else
            {
                writer.Write(true);
                WorldPos.Value.WriteTo(writer);
            }
            ScreenOffset.WriteTo(writer);
            Color.WriteTo(writer);
        }
    
        /// <summary> Get string representation of ColoredVertex </summary>
        public override string ToString() {
            string stringResult = "ColoredVertex { ";
            stringResult += "WorldPos: ";
            if (!WorldPos.HasValue)
            {
                stringResult += "null";
            } else
            {
                stringResult += WorldPos.Value.ToString();
            }
            stringResult += ", ";
            stringResult += "ScreenOffset: ";
            stringResult += ScreenOffset.ToString();
            stringResult += ", ";
            stringResult += "Color: ";
            stringResult += Color.ToString();
            stringResult += " }";
            return stringResult;
        }
    }
}