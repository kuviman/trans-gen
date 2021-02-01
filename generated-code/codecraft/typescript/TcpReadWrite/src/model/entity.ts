import { EntityType } from "./entity-type";
import { Vec2Int } from "./vec2-int";
import { Stream } from "../stream";

/**
 * Game entity
 */
export class Entity {
    /**
     * Entity's ID. Unique for each entity
     */
    id: number
    /**
     * Entity's owner player ID, if owned by a player
     */
    playerId: number | null
    /**
     * Entity's type
     */
    entityType: EntityType
    /**
     * Entity's position (corner with minimal coordinates)
     */
    position: Vec2Int
    /**
     * Current health
     */
    health: number
    /**
     * If entity is active, it can perform actions
     */
    active: boolean

    constructor(id: number, playerId: number | null, entityType: EntityType, position: Vec2Int, health: number, active: boolean) {
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
    static async readFrom(stream: Stream): Promise<Entity> {
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
        return new Entity(id, playerId, entityType, position, health, active)
    }

    /**
     * Write Entity to output stream
     */
    async writeTo(stream: Stream) {
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