import { StreamWrapper } from "../stream-wrapper";

export class Player {
    id: number
    score: number
    resource: number

    constructor(id: number, score: number, resource: number) {
        this.id = id;
        this.score = score;
        this.resource = resource;
    }

    static async readFrom(stream: StreamWrapper): Promise<Player> {
        let id;
        id = await stream.readInt();
        let score;
        score = await stream.readInt();
        let resource;
        resource = await stream.readInt();
        return new Player(id, score, resource)
    }

    async writeTo(stream: StreamWrapper) {
        let id = this.id;
        await stream.writeInt(id);
        let score = this.score;
        await stream.writeInt(score);
        let resource = this.resource;
        await stream.writeInt(resource);
    }
}