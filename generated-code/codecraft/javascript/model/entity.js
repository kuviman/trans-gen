const EntityType = require('./entity-type');
const Vec2Int = require('./vec2-int');
/**
 * Game entity
 */
class Entity {
    /**
     * Entity's ID. Unique for each entity
     */
    id;
    /**
     * Entity's owner player ID, if owned by a player
     */
    playerId;
    /**
     * Entity's type
     */
    entityType;
    /**
     * Entity's position (corner with minimal coordinates)
     */
    position;
    /**
     * Current health
     */
    health;
    /**
     * If entity is active, it can perform actions
     */
    active;

    constructor(id, playerId, entityType, position, health, active) {
        this.id = id;
        this.playerId = playerId;
        this.entityType = entityType;
        this.position = position;
        this.health = health;
        this.active = active;
    }

    /**
     * Read Entity from input stream
     */
    static async readFrom(stream) {
        let id;
        id = await stream.readInt();
        let playerId;
        if (await stream.readBool()) {
            playerId = await stream.readInt();
        } else {
            playerId = null;
        }
        let entityType;
        entityType = await EntityType.readFrom(stream);
        let position;
        position = await Vec2Int.readFrom(stream);
        let health;
        health = await stream.readInt();
        let active;
        active = await stream.readBool();
        return new Entity(id, playerId, entityType, position, health, active);
    }

    /**
     * Write Entity to output stream
     */
    async writeTo(stream) {
        let id = this.id;
        await stream.writeInt(id);
        let playerId = this.playerId;
        if (playerId === null) {
            await stream.writeBool(false);
        } else {
            await stream.writeBool(true);
            await stream.writeInt(playerId);
        }
        let entityType = this.entityType;
        await entityType.writeTo(stream);
        let position = this.position;
        await position.writeTo(stream);
        let health = this.health;
        await stream.writeInt(health);
        let active = this.active;
        await stream.writeBool(active);
    }
}
module.exports = Entity