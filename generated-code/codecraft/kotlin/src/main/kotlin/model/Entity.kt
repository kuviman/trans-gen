package model

import util.StreamUtil

class Entity {
    var id: Int = 0
    var playerId: Int? = null
    lateinit var entityType: model.EntityType
    lateinit var position: model.Vec2Int
    var health: Int = 0
    var active: Boolean = false

    constructor(id: Int, playerId: Int?, entityType: model.EntityType, position: model.Vec2Int, health: Int, active: Boolean) {
        this.id = id
        this.playerId = playerId
        this.entityType = entityType
        this.position = position
        this.health = health
        this.active = active
    }

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

    companion object {
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