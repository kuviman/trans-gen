package model;

import util.StreamUtil;

public class Entity {
    private int id;

    public int getId() {
        return id;
    }

    public void setId(int value) {
        this.id = value;
    }
    private Integer playerId;

    public Integer getPlayerId() {
        return playerId;
    }

    public void setPlayerId(Integer value) {
        this.playerId = value;
    }
    private model.EntityType entityType;

    public model.EntityType getEntityType() {
        return entityType;
    }

    public void setEntityType(model.EntityType value) {
        this.entityType = value;
    }
    private model.Vec2Int position;

    public model.Vec2Int getPosition() {
        return position;
    }

    public void setPosition(model.Vec2Int value) {
        this.position = value;
    }
    private int health;

    public int getHealth() {
        return health;
    }

    public void setHealth(int value) {
        this.health = value;
    }
    private boolean active;

    public boolean isActive() {
        return active;
    }

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
        switch (StreamUtil.readInt(stream)) {
        case 0:
            entityType = model.EntityType.WALL;
            break;
        case 1:
            entityType = model.EntityType.HOUSE;
            break;
        case 2:
            entityType = model.EntityType.BUILDER_BASE;
            break;
        case 3:
            entityType = model.EntityType.BUILDER_UNIT;
            break;
        case 4:
            entityType = model.EntityType.MELEE_BASE;
            break;
        case 5:
            entityType = model.EntityType.MELEE_UNIT;
            break;
        case 6:
            entityType = model.EntityType.RANGED_BASE;
            break;
        case 7:
            entityType = model.EntityType.RANGED_UNIT;
            break;
        case 8:
            entityType = model.EntityType.RESOURCE;
            break;
        case 9:
            entityType = model.EntityType.TURRET;
            break;
        default:
            throw new java.io.IOException("Unexpected tag value");
        }
        model.Vec2Int position;
        position = model.Vec2Int.readFrom(stream);
        int health;
        health = StreamUtil.readInt(stream);
        boolean active;
        active = StreamUtil.readBoolean(stream);
        return new Entity(id, playerId, entityType, position, health, active);
    }

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
}