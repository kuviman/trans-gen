package trans_gen_test.model;

import trans_gen_test.util.StreamUtil;

/**
 * Build action
 */
public class BuildAction {
    /**
     * Type of an entity to build
     */
    private trans_gen_test.model.EntityType entityType;

    /**
     * Type of an entity to build
     */
    public trans_gen_test.model.EntityType getEntityType() {
        return entityType;
    }

    /**
     * Type of an entity to build
     */
    public void setEntityType(trans_gen_test.model.EntityType value) {
        this.entityType = value;
    }
    /**
     * Desired position of new entity
     */
    private trans_gen_test.Vec2Int position;

    /**
     * Desired position of new entity
     */
    public trans_gen_test.Vec2Int getPosition() {
        return position;
    }

    /**
     * Desired position of new entity
     */
    public void setPosition(trans_gen_test.Vec2Int value) {
        this.position = value;
    }

    public BuildAction(trans_gen_test.model.EntityType entityType, trans_gen_test.Vec2Int position) {
        this.entityType = entityType;
        this.position = position;
    }

    /**
     * Read BuildAction from input stream
     */
    public static BuildAction readFrom(java.io.InputStream stream) throws java.io.IOException {
        trans_gen_test.model.EntityType entityType;
        entityType = trans_gen_test.model.EntityType.readFrom(stream);
        trans_gen_test.Vec2Int position;
        position = trans_gen_test.Vec2Int.readFrom(stream);
        return new BuildAction(entityType, position);
    }

    /**
     * Write BuildAction to output stream
     */
    public void writeTo(java.io.OutputStream stream) throws java.io.IOException {
        StreamUtil.writeInt(stream, entityType.tag);
        position.writeTo(stream);
    }

    /**
     * Get string representation of BuildAction
     */
    @Override
    public String toString() {
        StringBuilder stringBuilder = new StringBuilder("BuildAction { ");
        stringBuilder.append("entityType: ");
        stringBuilder.append(String.valueOf(entityType));
        stringBuilder.append(", ");
        stringBuilder.append("position: ");
        stringBuilder.append(String.valueOf(position));
        stringBuilder.append(" }");
        return stringBuilder.toString();
    }
}