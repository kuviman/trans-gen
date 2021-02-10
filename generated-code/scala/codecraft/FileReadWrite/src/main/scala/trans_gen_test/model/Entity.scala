package trans_gen_test.model

import trans_gen_test.util.StreamUtil

/**
 * Game entity
 *
 * @param id Entity's ID. Unique for each entity
 * @param playerId Entity's owner player ID, if owned by a player
 * @param entityType Entity's type
 * @param position Entity's position (corner with minimal coordinates)
 * @param health Current health
 * @param active If entity is active, it can perform actions
 */
case class Entity(id: Int, playerId: Option[Int], entityType: trans_gen_test.model.EntityType, position: trans_gen_test.Vec2Int, health: Int, active: Boolean) {
    /**
     * Write Entity to output stream
     */
    def writeTo(stream: java.io.OutputStream) {
        StreamUtil.writeInt(stream, id)
        playerId match {
            case None => StreamUtil.writeBoolean(stream, false)
            case Some(value) => {
                StreamUtil.writeBoolean(stream, true)
                StreamUtil.writeInt(stream, value)
            }
        }
        entityType.writeTo(stream)
        position.writeTo(stream)
        StreamUtil.writeInt(stream, health)
        StreamUtil.writeBoolean(stream, active)
    }

    /**
     * Get string representation of Entity
     */
    override def toString(): String = {
        var stringBuilder = new StringBuilder("Entity { ")
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
        stringBuilder.toString()
    }
}

object Entity {
    /**
     * Read Entity from input stream
     */
    def readFrom(stream: java.io.InputStream): Entity = Entity(
        StreamUtil.readInt(stream),
        if (StreamUtil.readBoolean(stream)) Some(
            StreamUtil.readInt(stream)
        ) else None,
        trans_gen_test.model.EntityType.readFrom(stream),
        trans_gen_test.Vec2Int.readFrom(stream),
        StreamUtil.readInt(stream),
        StreamUtil.readBoolean(stream)
    )
}