const EntityType = require.main.require('./model/entity-type');
const Vec2Int = require.main.require('./vec2-int');
/**
 * Build action
 */
class BuildAction {
    /**
     * Type of an entity to build
     */
    entityType;
    /**
     * Desired position of new entity
     */
    position;

    constructor(entityType, position) {
        this.entityType = entityType;
        this.position = position;
    }

    /**
     * Read BuildAction from input stream
     */
    static async readFrom(stream) {
        let entityType;
        entityType = await EntityType.readFrom(stream);
        let position;
        position = await Vec2Int.readFrom(stream);
        return new BuildAction(entityType, position);
    }

    /**
     * Write BuildAction to output stream
     */
    async writeTo(stream) {
        let entityType = this.entityType;
        await entityType.writeTo(stream);
        let position = this.position;
        await position.writeTo(stream);
    }
}
module.exports = BuildAction