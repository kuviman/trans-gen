namespace TransGenTest.Model.DebugInterface
{
    /// <summary>
    /// Debug data can be drawn in the app
    /// </summary>
    public abstract class DebugData
    {
        /// <summary> Write DebugData to writer </summary>
        public abstract void WriteTo(System.IO.BinaryWriter writer);

        /// <summary> Read DebugData from reader </summary>
        public static DebugData ReadFrom(System.IO.BinaryReader reader)
        {
            switch (reader.ReadInt32())
            {
                case Log.TAG:
                    return Log.ReadFrom(reader);
                case Primitives.TAG:
                    return Primitives.ReadFrom(reader);
                case PlacedText.TAG:
                    return PlacedText.ReadFrom(reader);
                default:
                    throw new System.Exception("Unexpected tag value");
            }
        }

        /// <summary>
        /// Log some text
        /// </summary>
        public class Log : DebugData
        {
            public const int TAG = 0;
        
            /// <summary>
            /// Text to show
            /// </summary>
            public string Text { get; set; }
        
            public Log() { }
        
            public Log(string text)
            {
                this.Text = text;
            }
        
            /// <summary> Read Log from reader </summary>
            public static new Log ReadFrom(System.IO.BinaryReader reader)
            {
                var result = new Log();
                result.Text = System.Text.Encoding.UTF8.GetString(reader.ReadBytes(reader.ReadInt32()));
                return result;
            }
        
            /// <summary> Write Log to writer </summary>
            public override void WriteTo(System.IO.BinaryWriter writer)
            {
                writer.Write(TAG);
                var textData = System.Text.Encoding.UTF8.GetBytes(Text);
                writer.Write(textData.Length);
                writer.Write(textData);
            }
        
            /// <summary> Get string representation of Log </summary>
            public override string ToString() {
                string stringResult = "Log { ";
                stringResult += "Text: ";
                stringResult += "\"" + Text + "\"";
                stringResult += " }";
                return stringResult;
            }
        }

        /// <summary>
        /// Draw primitives
        /// </summary>
        public class Primitives : DebugData
        {
            public const int TAG = 1;
        
            /// <summary>
            /// Vertices
            /// </summary>
            public Model.DebugInterface.ColoredVertex[] Vertices { get; set; }
            /// <summary>
            /// Primitive type
            /// </summary>
            public Model.DebugInterface.PrimitiveType PrimitiveType { get; set; }
        
            public Primitives() { }
        
            public Primitives(Model.DebugInterface.ColoredVertex[] vertices, Model.DebugInterface.PrimitiveType primitiveType)
            {
                this.Vertices = vertices;
                this.PrimitiveType = primitiveType;
            }
        
            /// <summary> Read Primitives from reader </summary>
            public static new Primitives ReadFrom(System.IO.BinaryReader reader)
            {
                var result = new Primitives();
                result.Vertices = new Model.DebugInterface.ColoredVertex[reader.ReadInt32()];
                for (int verticesIndex = 0; verticesIndex < result.Vertices.Length; verticesIndex++)
                {
                    result.Vertices[verticesIndex] = Model.DebugInterface.ColoredVertex.ReadFrom(reader);
                }
                result.PrimitiveType = PrimitiveTypeHelper.ReadFrom(reader);
                return result;
            }
        
            /// <summary> Write Primitives to writer </summary>
            public override void WriteTo(System.IO.BinaryWriter writer)
            {
                writer.Write(TAG);
                writer.Write(Vertices.Length);
                foreach (var verticesElement in Vertices)
                {
                    verticesElement.WriteTo(writer);
                }
                writer.Write((int) (PrimitiveType));
            }
        
            /// <summary> Get string representation of Primitives </summary>
            public override string ToString() {
                string stringResult = "Primitives { ";
                stringResult += "Vertices: ";
                stringResult += "[ ";
                int verticesIndex = 0;
                foreach (var verticesElement in Vertices)
                {
                    if (verticesIndex != 0) {
                        stringResult += ", ";
                    }
                    stringResult += verticesElement.ToString();
                    verticesIndex++;
                }
                stringResult += " ]";
                stringResult += ", ";
                stringResult += "PrimitiveType: ";
                stringResult += PrimitiveType.ToString();
                stringResult += " }";
                return stringResult;
            }
        }

        /// <summary>
        /// Draw text
        /// </summary>
        public class PlacedText : DebugData
        {
            public const int TAG = 2;
        
            /// <summary>
            /// Vertex to determine text position and color
            /// </summary>
            public Model.DebugInterface.ColoredVertex Vertex { get; set; }
            /// <summary>
            /// Text
            /// </summary>
            public string Text { get; set; }
            /// <summary>
            /// Text alignment (0 means left, 0.5 means center, 1 means right)
            /// </summary>
            public float Alignment { get; set; }
            /// <summary>
            /// Font size in pixels
            /// </summary>
            public float Size { get; set; }
        
            public PlacedText() { }
        
            public PlacedText(Model.DebugInterface.ColoredVertex vertex, string text, float alignment, float size)
            {
                this.Vertex = vertex;
                this.Text = text;
                this.Alignment = alignment;
                this.Size = size;
            }
        
            /// <summary> Read PlacedText from reader </summary>
            public static new PlacedText ReadFrom(System.IO.BinaryReader reader)
            {
                var result = new PlacedText();
                result.Vertex = Model.DebugInterface.ColoredVertex.ReadFrom(reader);
                result.Text = System.Text.Encoding.UTF8.GetString(reader.ReadBytes(reader.ReadInt32()));
                result.Alignment = reader.ReadSingle();
                result.Size = reader.ReadSingle();
                return result;
            }
        
            /// <summary> Write PlacedText to writer </summary>
            public override void WriteTo(System.IO.BinaryWriter writer)
            {
                writer.Write(TAG);
                Vertex.WriteTo(writer);
                var textData = System.Text.Encoding.UTF8.GetBytes(Text);
                writer.Write(textData.Length);
                writer.Write(textData);
                writer.Write(Alignment);
                writer.Write(Size);
            }
        
            /// <summary> Get string representation of PlacedText </summary>
            public override string ToString() {
                string stringResult = "PlacedText { ";
                stringResult += "Vertex: ";
                stringResult += Vertex.ToString();
                stringResult += ", ";
                stringResult += "Text: ";
                stringResult += "\"" + Text + "\"";
                stringResult += ", ";
                stringResult += "Alignment: ";
                stringResult += Alignment.ToString();
                stringResult += ", ";
                stringResult += "Size: ";
                stringResult += Size.ToString();
                stringResult += " }";
                return stringResult;
            }
        }
    }
}