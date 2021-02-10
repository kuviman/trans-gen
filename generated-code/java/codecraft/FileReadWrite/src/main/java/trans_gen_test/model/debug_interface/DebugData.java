package trans_gen_test.model.debug_interface;

import trans_gen_test.util.StreamUtil;

/**
 * Debug data can be drawn in the app
 */
public abstract class DebugData {
    /**
     * Write DebugData to output stream
     */
    public abstract void writeTo(java.io.OutputStream stream) throws java.io.IOException;

    /**
     * Read DebugData from input stream
     */
    public static DebugData readFrom(java.io.InputStream stream) throws java.io.IOException {
        switch (StreamUtil.readInt(stream)) {
            case Log.TAG:
                return Log.readFrom(stream);
            case Primitives.TAG:
                return Primitives.readFrom(stream);
            case PlacedText.TAG:
                return PlacedText.readFrom(stream);
            default:
                throw new java.io.IOException("Unexpected tag value");
        }
    }

    /**
     * Log some text
     */
    public static class Log extends DebugData {
        public static final int TAG = 0;
    
        /**
         * Text to show
         */
        private String text;
    
        /**
         * Text to show
         */
        public String getText() {
            return text;
        }
    
        /**
         * Text to show
         */
        public void setText(String value) {
            this.text = value;
        }
    
        public Log(String text) {
            this.text = text;
        }
    
        /**
         * Read Log from input stream
         */
        public static Log readFrom(java.io.InputStream stream) throws java.io.IOException {
            String text;
            text = StreamUtil.readString(stream);
            return new Log(text);
        }
    
        /**
         * Write Log to output stream
         */
        @Override
        public void writeTo(java.io.OutputStream stream) throws java.io.IOException {
            StreamUtil.writeInt(stream, TAG);
            StreamUtil.writeString(stream, text);
        }
    
        /**
         * Get string representation of Log
         */
        @Override
        public String toString() {
            StringBuilder stringBuilder = new StringBuilder("Log { ");
            stringBuilder.append("text: ");
            stringBuilder.append('"' + text + '"');
            stringBuilder.append(" }");
            return stringBuilder.toString();
        }
    }

    /**
     * Draw primitives
     */
    public static class Primitives extends DebugData {
        public static final int TAG = 1;
    
        /**
         * Vertices
         */
        private trans_gen_test.model.debug_interface.ColoredVertex[] vertices;
    
        /**
         * Vertices
         */
        public trans_gen_test.model.debug_interface.ColoredVertex[] getVertices() {
            return vertices;
        }
    
        /**
         * Vertices
         */
        public void setVertices(trans_gen_test.model.debug_interface.ColoredVertex[] value) {
            this.vertices = value;
        }
        /**
         * Primitive type
         */
        private trans_gen_test.model.debug_interface.PrimitiveType primitiveType;
    
        /**
         * Primitive type
         */
        public trans_gen_test.model.debug_interface.PrimitiveType getPrimitiveType() {
            return primitiveType;
        }
    
        /**
         * Primitive type
         */
        public void setPrimitiveType(trans_gen_test.model.debug_interface.PrimitiveType value) {
            this.primitiveType = value;
        }
    
        public Primitives(trans_gen_test.model.debug_interface.ColoredVertex[] vertices, trans_gen_test.model.debug_interface.PrimitiveType primitiveType) {
            this.vertices = vertices;
            this.primitiveType = primitiveType;
        }
    
        /**
         * Read Primitives from input stream
         */
        public static Primitives readFrom(java.io.InputStream stream) throws java.io.IOException {
            trans_gen_test.model.debug_interface.ColoredVertex[] vertices;
            vertices = new trans_gen_test.model.debug_interface.ColoredVertex[StreamUtil.readInt(stream)];
            for (int verticesIndex = 0; verticesIndex < vertices.length; verticesIndex++) {
                trans_gen_test.model.debug_interface.ColoredVertex verticesElement;
                verticesElement = trans_gen_test.model.debug_interface.ColoredVertex.readFrom(stream);
                vertices[verticesIndex] = verticesElement;
            }
            trans_gen_test.model.debug_interface.PrimitiveType primitiveType;
            primitiveType = trans_gen_test.model.debug_interface.PrimitiveType.readFrom(stream);
            return new Primitives(vertices, primitiveType);
        }
    
        /**
         * Write Primitives to output stream
         */
        @Override
        public void writeTo(java.io.OutputStream stream) throws java.io.IOException {
            StreamUtil.writeInt(stream, TAG);
            StreamUtil.writeInt(stream, vertices.length);
            for (trans_gen_test.model.debug_interface.ColoredVertex verticesElement : vertices) {
                verticesElement.writeTo(stream);
            }
            StreamUtil.writeInt(stream, primitiveType.tag);
        }
    
        /**
         * Get string representation of Primitives
         */
        @Override
        public String toString() {
            StringBuilder stringBuilder = new StringBuilder("Primitives { ");
            stringBuilder.append("vertices: ");
            stringBuilder.append("[ ");
            for (int verticesIndex = 0; verticesIndex < vertices.length; verticesIndex++) {
                if (verticesIndex != 0) {
                    stringBuilder.append(", ");
                }
                trans_gen_test.model.debug_interface.ColoredVertex verticesElement = vertices[verticesIndex];
                stringBuilder.append(String.valueOf(verticesElement));
            }
            stringBuilder.append(" ]");
            stringBuilder.append(", ");
            stringBuilder.append("primitiveType: ");
            stringBuilder.append(String.valueOf(primitiveType));
            stringBuilder.append(" }");
            return stringBuilder.toString();
        }
    }

    /**
     * Draw text
     */
    public static class PlacedText extends DebugData {
        public static final int TAG = 2;
    
        /**
         * Vertex to determine text position and color
         */
        private trans_gen_test.model.debug_interface.ColoredVertex vertex;
    
        /**
         * Vertex to determine text position and color
         */
        public trans_gen_test.model.debug_interface.ColoredVertex getVertex() {
            return vertex;
        }
    
        /**
         * Vertex to determine text position and color
         */
        public void setVertex(trans_gen_test.model.debug_interface.ColoredVertex value) {
            this.vertex = value;
        }
        /**
         * Text
         */
        private String text;
    
        /**
         * Text
         */
        public String getText() {
            return text;
        }
    
        /**
         * Text
         */
        public void setText(String value) {
            this.text = value;
        }
        /**
         * Text alignment (0 means left, 0.5 means center, 1 means right)
         */
        private float alignment;
    
        /**
         * Text alignment (0 means left, 0.5 means center, 1 means right)
         */
        public float getAlignment() {
            return alignment;
        }
    
        /**
         * Text alignment (0 means left, 0.5 means center, 1 means right)
         */
        public void setAlignment(float value) {
            this.alignment = value;
        }
        /**
         * Font size in pixels
         */
        private float size;
    
        /**
         * Font size in pixels
         */
        public float getSize() {
            return size;
        }
    
        /**
         * Font size in pixels
         */
        public void setSize(float value) {
            this.size = value;
        }
    
        public PlacedText(trans_gen_test.model.debug_interface.ColoredVertex vertex, String text, float alignment, float size) {
            this.vertex = vertex;
            this.text = text;
            this.alignment = alignment;
            this.size = size;
        }
    
        /**
         * Read PlacedText from input stream
         */
        public static PlacedText readFrom(java.io.InputStream stream) throws java.io.IOException {
            trans_gen_test.model.debug_interface.ColoredVertex vertex;
            vertex = trans_gen_test.model.debug_interface.ColoredVertex.readFrom(stream);
            String text;
            text = StreamUtil.readString(stream);
            float alignment;
            alignment = StreamUtil.readFloat(stream);
            float size;
            size = StreamUtil.readFloat(stream);
            return new PlacedText(vertex, text, alignment, size);
        }
    
        /**
         * Write PlacedText to output stream
         */
        @Override
        public void writeTo(java.io.OutputStream stream) throws java.io.IOException {
            StreamUtil.writeInt(stream, TAG);
            vertex.writeTo(stream);
            StreamUtil.writeString(stream, text);
            StreamUtil.writeFloat(stream, alignment);
            StreamUtil.writeFloat(stream, size);
        }
    
        /**
         * Get string representation of PlacedText
         */
        @Override
        public String toString() {
            StringBuilder stringBuilder = new StringBuilder("PlacedText { ");
            stringBuilder.append("vertex: ");
            stringBuilder.append(String.valueOf(vertex));
            stringBuilder.append(", ");
            stringBuilder.append("text: ");
            stringBuilder.append('"' + text + '"');
            stringBuilder.append(", ");
            stringBuilder.append("alignment: ");
            stringBuilder.append(String.valueOf(alignment));
            stringBuilder.append(", ");
            stringBuilder.append("size: ");
            stringBuilder.append(String.valueOf(size));
            stringBuilder.append(" }");
            return stringBuilder.toString();
        }
    }
}