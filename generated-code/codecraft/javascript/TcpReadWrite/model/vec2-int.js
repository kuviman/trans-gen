/**
 * 2 dimensional vector.
 */
class Vec2Int {
    /**
     * `x` coordinate of the vector
     */
    x;
    /**
     * `y` coordinate of the vector
     */
    y;

    constructor(x, y) {
        this.x = x;
        this.y = y;
    }

    /**
     * Read Vec2Int from input stream
     */
    static async readFrom(stream) {
        let x;
        x = await stream.readInt();
        let y;
        y = await stream.readInt();
        return new Vec2Int(x, y);
    }

    /**
     * Write Vec2Int to output stream
     */
    async writeTo(stream) {
        let x = this.x;
        await stream.writeInt(x);
        let y = this.y;
        await stream.writeInt(y);
    }
}
module.exports = Vec2Int