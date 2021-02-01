package model;

import util.StreamUtil;

/**
 * Game entity
 */
public class Entity {
    /**
     * Entity's ID. Unique for each entity
     */
    private int id;

    /**
     * Entity's ID. Unique for each entity
     */
    public int getId() {
        return id;
    }

    /**
     * Entity's ID. Unique for each entity
     */
    public void setId(int value) {
        this.id = value;
    }
    /**
     * Entity's owner player ID, if owned by a player
     */
    private Integer playerId;

    /**
     * Entity's owner player ID, if owned by a player
     */
    public Integer getPlayerId() {
        return playerId;
    }

    /**
     * Entity's owner player ID, if owned by a player
     */
    public void setPlayerId(Integer value) {
        this.playerId = value;
    }
    /**
     * Entity's type
     */
    private model.EntityType entityType;

    /**
     * Entity's type
     */
    public model.EntityType getEntityType() {
        return entityType;
    }

    /**
     * Entity's type
     */
    public void setEntityType(model.EntityType value) {
        this.entityType = value;
    }
    /**
     * Entity's position (corner with minimal coordinates)
     */
    private model.Vec2Int position;

    /**
     * Entity's position (corner with minimal coordinates)
     */
    public model.Vec2Int getPosition() {
        return position;
    }

    /**
     * Entity's position (corner with minimal coordinates)
     */
    public void setPosition(model.Vec2Int value) {
        this.position = value;
    }
    /**
     * Current health
     */
    private int health;

    /**
     * Current health
     */
    public int getHealth() {
        return health;
    }

    /**
     * Current health
     */
    public void setHealth(int value) {
        this.health = value;
    }
    /**
     * If entity is active, it can perform actions
     */
    private boolean active;

    /**
     * If entity is active, it can perform actions
     */
    public boolean isActive() {
        return active;
    }

    /**
     * If entity is active, it can perform actions
     */
    public void setActive(boolean value) {
        this.active = value;
    }

    public Entity(int id, Integer playerId, model.EntityType entityType, model.Vec2Int position, int health, boolean active) {
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
    public static Entity readFrom(java.io.InputStream stream) throws java.io.IOException {
        int id;
        id = StreamUtil.readInt(stream);
        Integer playerId;
        if (StreamUtil.readBoolean(stream)) {
            playerId = StreamUtil.readInt(stream);
        } else {
            playerId = null;
        }
        model.EntityType entityType;
        entityType = model.EntityType.readFrom(stream);
        model.Vec2Int position;
        position = model.Vec2Int.readFrom(stream);
        int health;
        health = StreamUtil.readInt(stream);
        boolean active;
        active = StreamUtil.readBoolean(stream);
        return new Entity(id, playerId, entityType, position, health, active);
    }

    /**
     * Write Entity to output stream
     */
    public void writeTo(java.io.OutputStream stream) throws java.io.IOException {
        StreamUtil.writeInt(stream, id);
        if (playerId == null) {
            StreamUtil.writeBoolean(stream, false);
        } else {
            StreamUtil.writeBoolean(stream, true);
            StreamUtil.writeInt(stream, playerId);
        }
        StreamUtil.writeInt(stream, entityType.tag);
        position.writeTo(stream);
        StreamUtil.writeInt(stream, health);
        StreamUtil.writeBoolean(stream, active);
    }

    /**
     * Get string representation of Entity
     */
    @Override
    public String toString() {
        StringBuilder stringBuilder = new StringBuilder("Entity { ");
        stringBuilder.append("id: ");
        stringBuilder.append(String.valueOf(id));
        stringBuilder.append(", ");
        stringBuilder.append("playerId: ");
        stringBuilder.append(String.valueOf(playerId));
        stringBuilder.append(", ");
        stringBuilder.append("entityType: ");
        stringBuilder.append(String.valueOf(entityType));
        stringBuilder.append(", ");
        stringBuilder.append("position: ");
        stringBuilder.append(String.valueOf(position));
        stringBuilder.append(", ");
        stringBuilder.append("health: ");
        stringBuilder.append(String.valueOf(health));
        stringBuilder.append(", ");
        stringBuilder.append("active: ");
        stringBuilder.append(String.valueOf(active));
        stringBuilder.append(" }");
        return stringBuilder.toString();
    }
}