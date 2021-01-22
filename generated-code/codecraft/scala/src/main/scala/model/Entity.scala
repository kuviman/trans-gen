package model

import util.StreamUtil

case class Entity(id: Int, playerId: Option[Int], entityType: model.EntityType, position: model.Vec2Int, health: Int, active: Boolean) {
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
    def readFrom(stream: java.io.InputStream): Entity = Entity(
        StreamUtil.readInt(stream),
        if (StreamUtil.readBoolean(stream)) Some(
            StreamUtil.readInt(stream)
        ) else None,
        model.EntityType.readFrom(stream),
        model.Vec2Int.readFrom(stream),
        StreamUtil.readInt(stream),
        StreamUtil.readBoolean(stream)
    )
}