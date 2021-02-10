package trans_gen_test.model.debug_interface

import trans_gen_test.util.StreamUtil

/**
 * Vertex for debug rendering
 */
class ColoredVertex {
    /**
     * Position in world coordinates (if none, screen position (0, 0) is used)
     */
    var worldPos: trans_gen_test.Vec2Float?
    /**
     * Additional offset in screen coordinates
     */
    var screenOffset: trans_gen_test.Vec2Float
    /**
     * Color to use
     */
    var color: trans_gen_test.Color

    constructor(worldPos: trans_gen_test.Vec2Float?, screenOffset: trans_gen_test.Vec2Float, color: trans_gen_test.Color) {
        this.worldPos = worldPos
        this.screenOffset = screenOffset
        this.color = color
    }

    /**
     * Write ColoredVertex to output stream
     */
    @Throws(java.io.IOException::class)
    fun writeTo(stream: java.io.OutputStream) {
        val worldPosValue = worldPos
        if (worldPosValue == null) {
            StreamUtil.writeBoolean(stream, false)
        } else {
            StreamUtil.writeBoolean(stream, true)
            worldPosValue.writeTo(stream)
        }
        screenOffset.writeTo(stream)
        color.writeTo(stream)
    }

    /**
     * Get string representation of ColoredVertex
     */
    override fun toString(): String {
        var stringBuilder = StringBuilder("ColoredVertex { ")
        stringBuilder.append("worldPos: ")
        stringBuilder.append(worldPos)
        stringBuilder.append(", ")
        stringBuilder.append("screenOffset: ")
        stringBuilder.append(screenOffset)
        stringBuilder.append(", ")
        stringBuilder.append("color: ")
        stringBuilder.append(color)
        stringBuilder.append(" }")
        return stringBuilder.toString()
    }

    companion object {
        /**
         * Read ColoredVertex from input stream
         */
        @Throws(java.io.IOException::class)
        fun readFrom(stream: java.io.InputStream): ColoredVertex {
            var worldPos: trans_gen_test.Vec2Float?
            if (StreamUtil.readBoolean(stream)) {
                worldPos = trans_gen_test.Vec2Float.readFrom(stream)
            } else {
                worldPos = null
            }
            var screenOffset: trans_gen_test.Vec2Float
            screenOffset = trans_gen_test.Vec2Float.readFrom(stream)
            var color: trans_gen_test.Color
            color = trans_gen_test.Color.readFrom(stream)
            return ColoredVertex(worldPos, screenOffset, color)
        }
    }
}