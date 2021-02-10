package trans_gen_test.model.debug_interface

import trans_gen_test.util.StreamUtil

/**
 * Vertex for debug rendering
 *
 * @param worldPos Position in world coordinates (if none, screen position (0, 0) is used)
 * @param screenOffset Additional offset in screen coordinates
 * @param color Color to use
 */
case class ColoredVertex(worldPos: Option[trans_gen_test.Vec2Float], screenOffset: trans_gen_test.Vec2Float, color: trans_gen_test.Color) {
    /**
     * Write ColoredVertex to output stream
     */
    def writeTo(stream: java.io.OutputStream) {
        worldPos match {
            case None => StreamUtil.writeBoolean(stream, false)
            case Some(value) => {
                StreamUtil.writeBoolean(stream, true)
                value.writeTo(stream)
            }
        }
        screenOffset.writeTo(stream)
        color.writeTo(stream)
    }

    /**
     * Get string representation of ColoredVertex
     */
    override def toString(): String = {
        var stringBuilder = new StringBuilder("ColoredVertex { ")
        stringBuilder.append("worldPos: ")
        stringBuilder.append(worldPos)
        stringBuilder.append(", ")
        stringBuilder.append("screenOffset: ")
        stringBuilder.append(screenOffset)
        stringBuilder.append(", ")
        stringBuilder.append("color: ")
        stringBuilder.append(color)
        stringBuilder.append(" }")
        stringBuilder.toString()
    }
}

object ColoredVertex {
    /**
     * Read ColoredVertex from input stream
     */
    def readFrom(stream: java.io.InputStream): ColoredVertex = ColoredVertex(
        if (StreamUtil.readBoolean(stream)) Some(
            trans_gen_test.Vec2Float.readFrom(stream)
        ) else None,
        trans_gen_test.Vec2Float.readFrom(stream),
        trans_gen_test.Color.readFrom(stream)
    )
}