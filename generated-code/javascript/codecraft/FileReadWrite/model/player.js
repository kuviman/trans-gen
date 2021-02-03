/**
 * Player (strategy, client)
 */
class Player {
    /**
     * Player's ID
     */
    id;
    /**
     * Current score
     */
    score;
    /**
     * Current amount of resource
     */
    resource;

    constructor(id, score, resource) {
        this.id = id;
        this.score = score;
        this.resource = resource;
    }

    /**
     * Read Player from input stream
     */
    static async readFrom(stream) {
        let id;
        id = await stream.readInt();
        let score;
        score = await stream.readInt();
        let resource;
        resource = await stream.readInt();
        return new Player(id, score, resource);
    }

    /**
     * Write Player to output stream
     */
    async writeTo(stream) {
        let id = this.id;
        await stream.writeInt(id);
        let score = this.score;
        await stream.writeInt(score);
        let resource = this.resource;
        await stream.writeInt(resource);
    }
}
module.exports = Player