package trans_gen_test.model

import trans_gen_test.util.StreamUtil

/**
 * Information available to the player
 *
 * @param myId Your player's ID
 * @param mapSize Size of the map
 * @param fogOfWar Whether fog of war is enabled
 * @param entityProperties Entity properties for each entity type
 * @param maxTickCount Max tick count for the game
 * @param maxPathfindNodes Max pathfind nodes when performing pathfinding in the game simulator
 * @param currentTick Current tick
 * @param players List of players
 * @param entities List of entities
 */
case class PlayerView(myId: Int, mapSize: Int, fogOfWar: Boolean, entityProperties: Map[trans_gen_test.model.EntityType, trans_gen_test.model.EntityProperties], maxTickCount: Int, maxPathfindNodes: Int, currentTick: Int, players: Seq[trans_gen_test.model.Player], entities: Seq[trans_gen_test.model.Entity]) {
    /**
     * Write PlayerView to output stream
     */
    def writeTo(stream: java.io.OutputStream) {
        StreamUtil.writeInt(stream, myId)
        StreamUtil.writeInt(stream, mapSize)
        StreamUtil.writeBoolean(stream, fogOfWar)
        StreamUtil.writeInt(stream, entityProperties.size)
        entityProperties.foreach { case (key, value) =>
            key.writeTo(stream)
            value.writeTo(stream)
        }
        StreamUtil.writeInt(stream, maxTickCount)
        StreamUtil.writeInt(stream, maxPathfindNodes)
        StreamUtil.writeInt(stream, currentTick)
        StreamUtil.writeInt(stream, players.length)
        players.foreach { value =>
            value.writeTo(stream)
        }
        StreamUtil.writeInt(stream, entities.length)
        entities.foreach { value =>
            value.writeTo(stream)
        }
    }

    /**
     * Get string representation of PlayerView
     */
    override def toString(): String = {
        var stringBuilder = new StringBuilder("PlayerView { ")
        stringBuilder.append("myId: ")
        stringBuilder.append(myId)
        stringBuilder.append(", ")
        stringBuilder.append("mapSize: ")
        stringBuilder.append(mapSize)
        stringBuilder.append(", ")
        stringBuilder.append("fogOfWar: ")
        stringBuilder.append(fogOfWar)
        stringBuilder.append(", ")
        stringBuilder.append("entityProperties: ")
        stringBuilder.append(entityProperties)
        stringBuilder.append(", ")
        stringBuilder.append("maxTickCount: ")
        stringBuilder.append(maxTickCount)
        stringBuilder.append(", ")
        stringBuilder.append("maxPathfindNodes: ")
        stringBuilder.append(maxPathfindNodes)
        stringBuilder.append(", ")
        stringBuilder.append("currentTick: ")
        stringBuilder.append(currentTick)
        stringBuilder.append(", ")
        stringBuilder.append("players: ")
        stringBuilder.append(players)
        stringBuilder.append(", ")
        stringBuilder.append("entities: ")
        stringBuilder.append(entities)
        stringBuilder.append(" }")
        stringBuilder.toString()
    }
}

object PlayerView {
    /**
     * Read PlayerView from input stream
     */
    def readFrom(stream: java.io.InputStream): PlayerView = PlayerView(
        StreamUtil.readInt(stream),
        StreamUtil.readInt(stream),
        StreamUtil.readBoolean(stream),
        (0 until StreamUtil.readInt(stream)).map { _ => (
            trans_gen_test.model.EntityType.readFrom(stream),
            trans_gen_test.model.EntityProperties.readFrom(stream)
        )}.toMap,
        StreamUtil.readInt(stream),
        StreamUtil.readInt(stream),
        StreamUtil.readInt(stream),
        (0 until StreamUtil.readInt(stream)).map { _ =>
            trans_gen_test.model.Player.readFrom(stream)
        },
        (0 until StreamUtil.readInt(stream)).map { _ =>
            trans_gen_test.model.Entity.readFrom(stream)
        }
    )
}