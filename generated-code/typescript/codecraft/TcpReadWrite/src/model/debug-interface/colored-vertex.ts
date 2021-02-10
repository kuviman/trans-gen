import { Color } from "../../color";
import { Vec2Float } from "../../vec2-float";
import { Stream } from "../../stream";

/**
 * Vertex for debug rendering
 */
export class ColoredVertex {
    /**
     * Position in world coordinates (if none, screen position (0, 0) is used)
     */
    worldPos: Vec2Float | null
    /**
     * Additional offset in screen coordinates
     */
    screenOffset: Vec2Float
    /**
     * Color to use
     */
    color: Color

    constructor(worldPos: Vec2Float | null, screenOffset: Vec2Float, color: Color) {
        this.worldPos = worldPos;
        this.screenOffset = screenOffset;
        this.color = color;
    }

    /**
     * Read ColoredVertex from input stream
     */
    static async readFrom(stream: Stream): Promise<ColoredVertex> {
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
        return new ColoredVertex(worldPos, screenOffset, color)
    }

    /**
     * Write ColoredVertex to output stream
     */
    async writeTo(stream: Stream) {
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