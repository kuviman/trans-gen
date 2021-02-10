import { Stream } from "./stream";

/**
 * 2 dimensional vector.
 */
export class Vec2Int {
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
     * Read Vec2Int from input stream
     */
    static async readFrom(stream: Stream): Promise<Vec2Int> {
        let x;
        x = await stream.readInt();
        let y;
        y = await stream.readInt();
        return new Vec2Int(x, y)
    }

    /**
     * Write Vec2Int to output stream
     */
    async writeTo(stream: Stream) {
        let x = this.x;
        await stream.writeInt(x);
        let y = this.y;
        await stream.writeInt(y);
    }
}