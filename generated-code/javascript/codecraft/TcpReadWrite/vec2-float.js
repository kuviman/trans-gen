/**
 * 2 dimensional vector.
 */
class Vec2Float {
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
     * Read Vec2Float from input stream
     */
    static async readFrom(stream) {
        let x;
        x = await stream.readFloat();
        let y;
        y = await stream.readFloat();
        return new Vec2Float(x, y);
    }

    /**
     * Write Vec2Float to output stream
     */
    async writeTo(stream) {
        let x = this.x;
        await stream.writeFloat(x);
        let y = this.y;
        await stream.writeFloat(y);
    }
}
module.exports = Vec2Float