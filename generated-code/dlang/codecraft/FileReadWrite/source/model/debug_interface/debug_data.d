module model.debug_interface.debug_data;

import stream;
import std.conv;
import std.typecons : Nullable;
import model.debug_interface.colored_vertex;
import model.debug_interface.primitive_type;

/// Debug data can be drawn in the app
abstract class DebugData {
    /// Write DebugData to writer
    abstract void writeTo(Stream writer) const;

    /// Read DebugData from reader
    static DebugData readFrom(Stream reader) {
        switch (reader.readInt()) {
            case Log.TAG:
                return Log.readFrom(reader);
            case Primitives.TAG:
                return Primitives.readFrom(reader);
            case PlacedText.TAG:
                return PlacedText.readFrom(reader);
            default:
                throw new Exception("Unexpected tag value");
        }
    }

    /// Log some text
    static class Log : DebugData {
        static const int TAG = 0;
    
        /// Text to show
        string text;
    
        this() {}
    
        this(string text) {
            this.text = text;
        }
    
        /// Read Log from reader
        static Log readFrom(Stream reader) {
            string text;
            text = reader.readString();
            return new Log(text);
        }
    
        /// Write Log to writer
        override void writeTo(Stream writer) const {
            writer.write(TAG);
            writer.write(text);
        }
    }

    /// Draw primitives
    static class Primitives : DebugData {
        static const int TAG = 1;
    
        /// Vertices
        model.debug_interface.ColoredVertex[] vertices;
        /// Primitive type
        model.debug_interface.PrimitiveType primitiveType;
    
        this() {}
    
        this(model.debug_interface.ColoredVertex[] vertices, model.debug_interface.PrimitiveType primitiveType) {
            this.vertices = vertices;
            this.primitiveType = primitiveType;
        }
    
        /// Read Primitives from reader
        static Primitives readFrom(Stream reader) {
            model.debug_interface.ColoredVertex[] vertices;
            vertices = new model.debug_interface.ColoredVertex[reader.readInt()];
            for (int verticesIndex = 0; verticesIndex < vertices.length; verticesIndex++) {
                model.debug_interface.ColoredVertex verticesKey;
                verticesKey = model.debug_interface.ColoredVertex.readFrom(reader);
                vertices[verticesIndex] = verticesKey;
            }
            model.debug_interface.PrimitiveType primitiveType;
            primitiveType = readPrimitiveType(reader);
            return new Primitives(vertices, primitiveType);
        }
    
        /// Write Primitives to writer
        override void writeTo(Stream writer) const {
            writer.write(TAG);
            writer.write(cast(int)(vertices.length));
            foreach (verticesElement; vertices) {
                verticesElement.writeTo(writer);
            }
            writer.write(cast(int)(primitiveType));
        }
    }

    /// Draw text
    static class PlacedText : DebugData {
        static const int TAG = 2;
    
        /// Vertex to determine text position and color
        model.debug_interface.ColoredVertex vertex;
        /// Text
        string text;
        /// Text alignment (0 means left, 0.5 means center, 1 means right)
        float alignment;
        /// Font size in pixels
        float size;
    
        this() {}
    
        this(model.debug_interface.ColoredVertex vertex, string text, float alignment, float size) {
            this.vertex = vertex;
            this.text = text;
            this.alignment = alignment;
            this.size = size;
        }
    
        /// Read PlacedText from reader
        static PlacedText readFrom(Stream reader) {
            model.debug_interface.ColoredVertex vertex;
            vertex = model.debug_interface.ColoredVertex.readFrom(reader);
            string text;
            text = reader.readString();
            float alignment;
            alignment = reader.readFloat();
            float size;
            size = reader.readFloat();
            return new PlacedText(vertex, text, alignment, size);
        }
    
        /// Write PlacedText to writer
        override void writeTo(Stream writer) const {
            writer.write(TAG);
            vertex.writeTo(writer);
            writer.write(text);
            writer.write(alignment);
            writer.write(size);
        }
    }
}