package trans_gen_test.model.debug_interface

import trans_gen_test.util.StreamUtil

/**
 * Debug data can be drawn in the app
 */
abstract class DebugData {
    /**
     * Write DebugData to output stream
     */
    @Throws(java.io.IOException::class)
    abstract fun writeTo(stream: java.io.OutputStream)

    companion object {
        /**
         * Read DebugData from input stream
         */
        @Throws(java.io.IOException::class)
        fun readFrom(stream: java.io.InputStream): DebugData {
            when (StreamUtil.readInt(stream)) {
                Log.TAG -> return Log.readFrom(stream)
                Primitives.TAG -> return Primitives.readFrom(stream)
                PlacedText.TAG -> return PlacedText.readFrom(stream)
                else -> throw java.io.IOException("Unexpected tag value")
            }
        }
    }

    /**
     * Log some text
     */
    class Log : DebugData {
        /**
         * Text to show
         */
        var text: String
    
        constructor(text: String) {
            this.text = text
        }
    
        /**
         * Write Log to output stream
         */
        @Throws(java.io.IOException::class)
        override fun writeTo(stream: java.io.OutputStream) {
            StreamUtil.writeInt(stream, TAG)
            StreamUtil.writeString(stream, text)
        }
    
        /**
         * Get string representation of Log
         */
        override fun toString(): String {
            var stringBuilder = StringBuilder("Log { ")
            stringBuilder.append("text: ")
            stringBuilder.append('"' + text + '"')
            stringBuilder.append(" }")
            return stringBuilder.toString()
        }
    
        companion object {
            val TAG = 0
    
            /**
             * Read Log from input stream
             */
            @Throws(java.io.IOException::class)
            fun readFrom(stream: java.io.InputStream): Log {
                var text: String
                text = StreamUtil.readString(stream)
                return Log(text)
            }
        }
    }

    /**
     * Draw primitives
     */
    class Primitives : DebugData {
        /**
         * Vertices
         */
        var vertices: Array<trans_gen_test.model.debug_interface.ColoredVertex>
        /**
         * Primitive type
         */
        var primitiveType: trans_gen_test.model.debug_interface.PrimitiveType
    
        constructor(vertices: Array<trans_gen_test.model.debug_interface.ColoredVertex>, primitiveType: trans_gen_test.model.debug_interface.PrimitiveType) {
            this.vertices = vertices
            this.primitiveType = primitiveType
        }
    
        /**
         * Write Primitives to output stream
         */
        @Throws(java.io.IOException::class)
        override fun writeTo(stream: java.io.OutputStream) {
            StreamUtil.writeInt(stream, TAG)
            StreamUtil.writeInt(stream, vertices.size)
            for (verticesElement in vertices) {
                verticesElement.writeTo(stream)
            }
            StreamUtil.writeInt(stream, primitiveType.tag)
        }
    
        /**
         * Get string representation of Primitives
         */
        override fun toString(): String {
            var stringBuilder = StringBuilder("Primitives { ")
            stringBuilder.append("vertices: ")
            stringBuilder.append("[ ")
            var verticesIndex = 0
            for (verticesElement in vertices) {
                if (verticesIndex != 0) {
                    stringBuilder.append(", ")
                }
                stringBuilder.append(verticesElement)
                verticesIndex++
            }
            stringBuilder.append(" ]")
            stringBuilder.append(", ")
            stringBuilder.append("primitiveType: ")
            stringBuilder.append(primitiveType)
            stringBuilder.append(" }")
            return stringBuilder.toString()
        }
    
        companion object {
            val TAG = 1
    
            /**
             * Read Primitives from input stream
             */
            @Throws(java.io.IOException::class)
            fun readFrom(stream: java.io.InputStream): Primitives {
                var vertices: Array<trans_gen_test.model.debug_interface.ColoredVertex>
                vertices = Array(StreamUtil.readInt(stream), {
                    var verticesElement: trans_gen_test.model.debug_interface.ColoredVertex
                    verticesElement = trans_gen_test.model.debug_interface.ColoredVertex.readFrom(stream)
                    verticesElement
                })
                var primitiveType: trans_gen_test.model.debug_interface.PrimitiveType
                primitiveType = trans_gen_test.model.debug_interface.PrimitiveType.readFrom(stream)
                return Primitives(vertices, primitiveType)
            }
        }
    }

    /**
     * Draw text
     */
    class PlacedText : DebugData {
        /**
         * Vertex to determine text position and color
         */
        var vertex: trans_gen_test.model.debug_interface.ColoredVertex
        /**
         * Text
         */
        var text: String
        /**
         * Text alignment (0 means left, 0.5 means center, 1 means right)
         */
        var alignment: Float
        /**
         * Font size in pixels
         */
        var size: Float
    
        constructor(vertex: trans_gen_test.model.debug_interface.ColoredVertex, text: String, alignment: Float, size: Float) {
            this.vertex = vertex
            this.text = text
            this.alignment = alignment
            this.size = size
        }
    
        /**
         * Write PlacedText to output stream
         */
        @Throws(java.io.IOException::class)
        override fun writeTo(stream: java.io.OutputStream) {
            StreamUtil.writeInt(stream, TAG)
            vertex.writeTo(stream)
            StreamUtil.writeString(stream, text)
            StreamUtil.writeFloat(stream, alignment)
            StreamUtil.writeFloat(stream, size)
        }
    
        /**
         * Get string representation of PlacedText
         */
        override fun toString(): String {
            var stringBuilder = StringBuilder("PlacedText { ")
            stringBuilder.append("vertex: ")
            stringBuilder.append(vertex)
            stringBuilder.append(", ")
            stringBuilder.append("text: ")
            stringBuilder.append('"' + text + '"')
            stringBuilder.append(", ")
            stringBuilder.append("alignment: ")
            stringBuilder.append(alignment)
            stringBuilder.append(", ")
            stringBuilder.append("size: ")
            stringBuilder.append(size)
            stringBuilder.append(" }")
            return stringBuilder.toString()
        }
    
        companion object {
            val TAG = 2
    
            /**
             * Read PlacedText from input stream
             */
            @Throws(java.io.IOException::class)
            fun readFrom(stream: java.io.InputStream): PlacedText {
                var vertex: trans_gen_test.model.debug_interface.ColoredVertex
                vertex = trans_gen_test.model.debug_interface.ColoredVertex.readFrom(stream)
                var text: String
                text = StreamUtil.readString(stream)
                var alignment: Float
                alignment = StreamUtil.readFloat(stream)
                var size: Float
                size = StreamUtil.readFloat(stream)
                return PlacedText(vertex, text, alignment, size)
            }
        }
    }
}