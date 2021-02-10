import { EntityType } from "./entity-type";
import { Vec2Int } from "../vec2-int";
import { Stream } from "../stream";

/**
 * Build action
 */
export class BuildAction {
    /**
     * Type of an entity to build
     */
    entityType: EntityType
    /**
     * Desired position of new entity
     */
    position: Vec2Int

    constructor(entityType: EntityType, position: Vec2Int) {
        this.entityType = entityType;
        this.position = position;
    }

    /**
     * Read BuildAction from input stream
     */
    static async readFrom(stream: Stream): Promise<BuildAction> {
        let entityType;
        entityType = await EntityType.readFrom(stream);
        let position;
        position = await Vec2Int.readFrom(stream);
        return new BuildAction(entityType, position)
    }

    /**
     * Write BuildAction to output stream
     */
    async writeTo(stream: Stream) {
        let entityType = this.entityType;
        await entityType.writeTo(stream);
        let position = this.position;
        await position.writeTo(stream);
    }
}