const Color = require.main.require('./color');
const Vec2Float = require.main.require('./vec2-float');
/**
 * Vertex for debug rendering
 */
class ColoredVertex {
    /**
     * Position in world coordinates (if none, screen position (0, 0) is used)
     */
    worldPos;
    /**
     * Additional offset in screen coordinates
     */
    screenOffset;
    /**
     * Color to use
     */
    color;

    constructor(worldPos, screenOffset, color) {
        this.worldPos = worldPos;
        this.screenOffset = screenOffset;
        this.color = color;
    }

    /**
     * Read ColoredVertex from input stream
     */
    static async readFrom(stream) {
        let worldPos;
        if (await stream.readBool()) {
            worldPos = await Vec2Float.readFrom(stream);
        } else {
            worldPos = null;
        }
        let screenOffset;
        screenOffset = await Vec2Float.readFrom(stream);
        let color;
        color = await Color.readFrom(stream);
        return new ColoredVertex(worldPos, screenOffset, color);
    }

    /**
     * Write ColoredVertex to output stream
     */
    async writeTo(stream) {
        let worldPos = this.worldPos;
        if (worldPos === null) {
            await stream.writeBool(false);
        } else {
            await stream.writeBool(true);
            await worldPos.writeTo(stream);
        }
        let screenOffset = this.screenOffset;
        await screenOffset.writeTo(stream);
        let color = this.color;
        await color.writeTo(stream);
    }
}
module.exports = ColoredVertex