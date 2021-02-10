import { Stream } from "./stream";

/**
 * 2 dimensional vector.
 */
export class Vec2Float {
    /**
     * `x` coordinate of the vector
     */
    x: number
    /**
     * `y` coordinate of the vector
     */
    y: number

    constructor(x: number, y: number) {
        this.x = x;
        this.y = y;
    }

    /**
     * Read Vec2Float from input stream
     */
    static async readFrom(stream: Stream): Promise<Vec2Float> {
        let x;
        x = await stream.readFloat();
        let y;
        y = await stream.readFloat();
        return new Vec2Float(x, y)
    }

    /**
     * Write Vec2Float to output stream
     */
    async writeTo(stream: Stream) {
        let x = this.x;
        await stream.writeFloat(x);
        let y = this.y;
        await stream.writeFloat(y);
    }
}