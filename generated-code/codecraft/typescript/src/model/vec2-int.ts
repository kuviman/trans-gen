
import { StreamWrapper } from "../stream-wrapper";

export class Vec2Int {
    x: number
    y: number

    constructor(x: number, y: number) {
        this.x = x;
        this.y = y;
    }

    static async readFrom(stream: StreamWrapper): Promise<Vec2Int> {
        let x;
        x = await stream.readInt();
        let y;
        y = await stream.readInt();
        return new Vec2Int(x, y)
    }

    async writeTo(stream: StreamWrapper) {
        let x = this.x;
        await stream.writeInt(x);
        let y = this.y;
        await stream.writeInt(y);
    }
}
