package model

import util.StreamUtil

/**
 * Game entity
 */
class Entity {
    /**
     * Entity's ID. Unique for each entity
     */
    var id: Int = 0
    /**
     * Entity's owner player ID, if owned by a player
     */
    var playerId: Int? = null
    /**
     * Entity's type
     */
    lateinit var entityType: model.EntityType
    /**
     * Entity's position (corner with minimal coordinates)
     */
    lateinit var position: model.Vec2Int
    /**
     * Current health
     */
    var health: Int = 0
    /**
     * If entity is active, it can perform actions
     */
    var active: Boolean = false

    constructor(id: Int, playerId: Int?, entityType: model.EntityType, position: model.Vec2Int, health: Int, active: Boolean) {
        this.id = id
        this.playerId = playerId
        this.entityType = entityType
        this.position = position
        this.health = health
        this.active = active
    }

    /**
     * Write Entity to output stream
     */
    @Throws(java.io.IOException::class)
    fun writeTo(stream: java.io.OutputStream) {
        StreamUtil.writeInt(stream, id)
        val playerIdValue = playerId
        if (playerIdValue == null) {
            StreamUtil.writeBoolean(stream, false)
        } else {
            StreamUtil.writeBoolean(stream, true)
            StreamUtil.writeInt(stream, playerIdValue)
        }
        StreamUtil.writeInt(stream, entityType.tag)
        position.writeTo(stream)
        StreamUtil.writeInt(stream, health)
        StreamUtil.writeBoolean(stream, active)
    }

    /**
     * Get string representation of Entity
     */
    override fun toString(): String {
        var stringBuilder = StringBuilder("Entity { ")
        stringBuilder.append("id: ")
        stringBuilder.append(id)
        stringBuilder.append(", ")
        stringBuilder.append("playerId: ")
        stringBuilder.append(playerId)
        stringBuilder.append(", ")
        stringBuilder.append("entityType: ")
        stringBuilder.append(entityType)
        stringBuilder.append(", ")
        stringBuilder.append("position: ")
        stringBuilder.append(position)
        stringBuilder.append(", ")
        stringBuilder.append("health: ")
        stringBuilder.append(health)
        stringBuilder.append(", ")
        stringBuilder.append("active: ")
        stringBuilder.append(active)
        stringBuilder.append(" }")
        return stringBuilder.toString()
    }

    companion object {
        /**
         * Read Entity from input stream
         */
        @Throws(java.io.IOException::class)
        fun readFrom(stream: java.io.InputStream): Entity {
            var id: Int
            id = StreamUtil.readInt(stream)
            var playerId: Int?
            if (StreamUtil.readBoolean(stream)) {
                playerId = StreamUtil.readInt(stream)
            } else {
                playerId = null
            }
            var entityType: model.EntityType
            entityType = model.EntityType.readFrom(stream)
            var position: model.Vec2Int
            position = model.Vec2Int.readFrom(stream)
            var health: Int
            health = StreamUtil.readInt(stream)
            var active: Boolean
            active = StreamUtil.readBoolean(stream)
            return Entity(id, playerId, entityType, position, health, active)
        }
    }
}