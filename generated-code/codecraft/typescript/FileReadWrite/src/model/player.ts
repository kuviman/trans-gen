import { Stream } from "../stream";

/**
 * Player (strategy, client)
 */
export class Player {
    /**
     * Player's ID
     */
    id: number
    /**
     * Current score
     */
    score: number
    /**
     * Current amount of resource
     */
    resource: number

    constructor(id: number, score: number, resource: number) {
        this.id = id;
        this.score = score;
        this.resource = resource;
    }

    /**
     * Read Player from input stream
     */
    static async readFrom(stream: Stream): Promise<Player> {
        let id;
        id = await stream.readInt();
        let score;
        score = await stream.readInt();
        let resource;
        resource = await stream.readInt();
        return new Player(id, score, resource)
    }

    /**
     * Write Player to output stream
     */
    async writeTo(stream: Stream) {
        let id = this.id;
        await stream.writeInt(id);
        let score = this.score;
        await stream.writeInt(score);
        let resource = this.resource;
        await stream.writeInt(resource);
    }
}