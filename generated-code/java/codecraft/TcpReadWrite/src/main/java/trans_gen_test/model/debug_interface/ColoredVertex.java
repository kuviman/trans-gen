package trans_gen_test.model.debug_interface;

import trans_gen_test.util.StreamUtil;

/**
 * Vertex for debug rendering
 */
public class ColoredVertex {
    /**
     * Position in world coordinates (if none, screen position (0, 0) is used)
     */
    private trans_gen_test.Vec2Float worldPos;

    /**
     * Position in world coordinates (if none, screen position (0, 0) is used)
     */
    public trans_gen_test.Vec2Float getWorldPos() {
        return worldPos;
    }

    /**
     * Position in world coordinates (if none, screen position (0, 0) is used)
     */
    public void setWorldPos(trans_gen_test.Vec2Float value) {
        this.worldPos = value;
    }
    /**
     * Additional offset in screen coordinates
     */
    private trans_gen_test.Vec2Float screenOffset;

    /**
     * Additional offset in screen coordinates
     */
    public trans_gen_test.Vec2Float getScreenOffset() {
        return screenOffset;
    }

    /**
     * Additional offset in screen coordinates
     */
    public void setScreenOffset(trans_gen_test.Vec2Float value) {
        this.screenOffset = value;
    }
    /**
     * Color to use
     */
    private trans_gen_test.Color color;

    /**
     * Color to use
     */
    public trans_gen_test.Color getColor() {
        return color;
    }

    /**
     * Color to use
     */
    public void setColor(trans_gen_test.Color value) {
        this.color = value;
    }

    public ColoredVertex(trans_gen_test.Vec2Float worldPos, trans_gen_test.Vec2Float screenOffset, trans_gen_test.Color color) {
        this.worldPos = worldPos;
        this.screenOffset = screenOffset;
        this.color = color;
    }

    /**
     * Read ColoredVertex from input stream
     */
    public static ColoredVertex readFrom(java.io.InputStream stream) throws java.io.IOException {
        trans_gen_test.Vec2Float worldPos;
        if (StreamUtil.readBoolean(stream)) {
            worldPos = trans_gen_test.Vec2Float.readFrom(stream);
        } else {
            worldPos = null;
        }
        trans_gen_test.Vec2Float screenOffset;
        screenOffset = trans_gen_test.Vec2Float.readFrom(stream);
        trans_gen_test.Color color;
        color = trans_gen_test.Color.readFrom(stream);
        return new ColoredVertex(worldPos, screenOffset, color);
    }

    /**
     * Write ColoredVertex to output stream
     */
    public void writeTo(java.io.OutputStream stream) throws java.io.IOException {
        if (worldPos == null) {
            StreamUtil.writeBoolean(stream, false);
        } else {
            StreamUtil.writeBoolean(stream, true);
            worldPos.writeTo(stream);
        }
        screenOffset.writeTo(stream);
        color.writeTo(stream);
    }

    /**
     * Get string representation of ColoredVertex
     */
    @Override
    public String toString() {
        StringBuilder stringBuilder = new StringBuilder("ColoredVertex { ");
        stringBuilder.append("worldPos: ");
        stringBuilder.append(String.valueOf(worldPos));
        stringBuilder.append(", ");
        stringBuilder.append("screenOffset: ");
        stringBuilder.append(String.valueOf(screenOffset));
        stringBuilder.append(", ");
        stringBuilder.append("color: ");
        stringBuilder.append(String.valueOf(color));
        stringBuilder.append(" }");
        return stringBuilder.toString();
    }
}