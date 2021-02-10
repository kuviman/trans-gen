package trans_gen_test.model.debug_interface

import trans_gen_test.util.StreamUtil

/**
 * Debug data can be drawn in the app
 */
sealed trait DebugData {
    /**
     * Write DebugData to output stream
     */
    def writeTo(stream: java.io.OutputStream)
}

object DebugData {
    /**
     * Log some text
     *
     * @param text Text to show
     */
    case class Log(text: String) extends DebugData {
        /**
         * Write Log to output stream
         */
        override def writeTo(stream: java.io.OutputStream) {
            StreamUtil.writeInt(stream, Log.TAG)
            StreamUtil.writeString(stream, text)
        }
    
        /**
         * Get string representation of Log
         */
        override def toString(): String = {
            var stringBuilder = new StringBuilder("Log { ")
            stringBuilder.append("text: ")
            stringBuilder.append('"' + text + '"')
            stringBuilder.append(" }")
            stringBuilder.toString()
        }
    }
    
    object Log {
        val TAG: Int = 0
    
        /**
         * Read Log from input stream
         */
        def readFrom(stream: java.io.InputStream): Log = Log(
            StreamUtil.readString(stream)
        )
    }

    /**
     * Draw primitives
     *
     * @param vertices Vertices
     * @param primitiveType Primitive type
     */
    case class Primitives(vertices: Seq[trans_gen_test.model.debug_interface.ColoredVertex], primitiveType: trans_gen_test.model.debug_interface.PrimitiveType) extends DebugData {
        /**
         * Write Primitives to output stream
         */
        override def writeTo(stream: java.io.OutputStream) {
            StreamUtil.writeInt(stream, Primitives.TAG)
            StreamUtil.writeInt(stream, vertices.length)
            vertices.foreach { value =>
                value.writeTo(stream)
            }
            primitiveType.writeTo(stream)
        }
    
        /**
         * Get string representation of Primitives
         */
        override def toString(): String = {
            var stringBuilder = new StringBuilder("Primitives { ")
            stringBuilder.append("vertices: ")
            stringBuilder.append(vertices)
            stringBuilder.append(", ")
            stringBuilder.append("primitiveType: ")
            stringBuilder.append(primitiveType)
            stringBuilder.append(" }")
            stringBuilder.toString()
        }
    }
    
    object Primitives {
        val TAG: Int = 1
    
        /**
         * Read Primitives from input stream
         */
        def readFrom(stream: java.io.InputStream): Primitives = Primitives(
            (0 until StreamUtil.readInt(stream)).map { _ =>
                trans_gen_test.model.debug_interface.ColoredVertex.readFrom(stream)
            },
            trans_gen_test.model.debug_interface.PrimitiveType.readFrom(stream)
        )
    }

    /**
     * Draw text
     *
     * @param vertex Vertex to determine text position and color
     * @param text Text
     * @param alignment Text alignment (0 means left, 0.5 means center, 1 means right)
     * @param size Font size in pixels
     */
    case class PlacedText(vertex: trans_gen_test.model.debug_interface.ColoredVertex, text: String, alignment: Float, size: Float) extends DebugData {
        /**
         * Write PlacedText to output stream
         */
        override def writeTo(stream: java.io.OutputStream) {
            StreamUtil.writeInt(stream, PlacedText.TAG)
            vertex.writeTo(stream)
            StreamUtil.writeString(stream, text)
            StreamUtil.writeFloat(stream, alignment)
            StreamUtil.writeFloat(stream, size)
        }
    
        /**
         * Get string representation of PlacedText
         */
        override def toString(): String = {
            var stringBuilder = new StringBuilder("PlacedText { ")
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
            stringBuilder.toString()
        }
    }
    
    object PlacedText {
        val TAG: Int = 2
    
        /**
         * Read PlacedText from input stream
         */
        def readFrom(stream: java.io.InputStream): PlacedText = PlacedText(
            trans_gen_test.model.debug_interface.ColoredVertex.readFrom(stream),
            StreamUtil.readString(stream),
            StreamUtil.readFloat(stream),
            StreamUtil.readFloat(stream)
        )
    }

    /**
     * Read DebugData from input stream
     */
    def readFrom(stream: java.io.InputStream): DebugData = {
        StreamUtil.readInt(stream) match {
            case Log.TAG => Log.readFrom(stream)
            case Primitives.TAG => Primitives.readFrom(stream)
            case PlacedText.TAG => PlacedText.readFrom(stream)
            case _ => throw new java.io.IOException("Unexpected tag value")
        }
    }
}