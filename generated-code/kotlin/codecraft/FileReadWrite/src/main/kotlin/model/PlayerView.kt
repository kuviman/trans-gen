package model

import util.StreamUtil

/**
 * Information available to the player
 */
class PlayerView {
    /**
     * Your player's ID
     */
    var myId: Int = 0
    /**
     * Size of the map
     */
    var mapSize: Int = 0
    /**
     * Whether fog of war is enabled
     */
    var fogOfWar: Boolean = false
    /**
     * Entity properties for each entity type
     */
    lateinit var entityProperties: MutableMap<model.EntityType, model.EntityProperties>
    /**
     * Max tick count for the game
     */
    var maxTickCount: Int = 0
    /**
     * Max pathfind nodes when performing pathfinding in the game simulator
     */
    var maxPathfindNodes: Int = 0
    /**
     * Current tick
     */
    var currentTick: Int = 0
    /**
     * List of players
     */
    lateinit var players: Array<model.Player>
    /**
     * List of entities
     */
    lateinit var entities: Array<model.Entity>

    constructor(myId: Int, mapSize: Int, fogOfWar: Boolean, entityProperties: MutableMap<model.EntityType, model.EntityProperties>, maxTickCount: Int, maxPathfindNodes: Int, currentTick: Int, players: Array<model.Player>, entities: Array<model.Entity>) {
        this.myId = myId
        this.mapSize = mapSize
        this.fogOfWar = fogOfWar
        this.entityProperties = entityProperties
        this.maxTickCount = maxTickCount
        this.maxPathfindNodes = maxPathfindNodes
        this.currentTick = currentTick
        this.players = players
        this.entities = entities
    }

    /**
     * Write PlayerView to output stream
     */
    @Throws(java.io.IOException::class)
    fun writeTo(stream: java.io.OutputStream) {
        StreamUtil.writeInt(stream, myId)
        StreamUtil.writeInt(stream, mapSize)
        StreamUtil.writeBoolean(stream, fogOfWar)
        StreamUtil.writeInt(stream, entityProperties.size)
        for (entityPropertiesEntry in entityProperties) {
            val entityPropertiesKey = entityPropertiesEntry.key
            StreamUtil.writeInt(stream, entityPropertiesKey.tag)
            val entityPropertiesValue = entityPropertiesEntry.value
            entityPropertiesValue.writeTo(stream)
        }
        StreamUtil.writeInt(stream, maxTickCount)
        StreamUtil.writeInt(stream, maxPathfindNodes)
        StreamUtil.writeInt(stream, currentTick)
        StreamUtil.writeInt(stream, players.size)
        for (playersElement in players) {
            playersElement.writeTo(stream)
        }
        StreamUtil.writeInt(stream, entities.size)
        for (entitiesElement in entities) {
            entitiesElement.writeTo(stream)
        }
    }

    /**
     * Get string representation of PlayerView
     */
    override fun toString(): String {
        var stringBuilder = StringBuilder("PlayerView { ")
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
        stringBuilder.append("[ ")
        var playersIndex = 0
        for (playersElement in players) {
            if (playersIndex != 0) {
                stringBuilder.append(", ")
            }
            stringBuilder.append(playersElement)
            playersIndex++
        }
        stringBuilder.append(" ]")
        stringBuilder.append(", ")
        stringBuilder.append("entities: ")
        stringBuilder.append("[ ")
        var entitiesIndex = 0
        for (entitiesElement in entities) {
            if (entitiesIndex != 0) {
                stringBuilder.append(", ")
            }
            stringBuilder.append(entitiesElement)
            entitiesIndex++
        }
        stringBuilder.append(" ]")
        stringBuilder.append(" }")
        return stringBuilder.toString()
    }

    companion object {
        /**
         * Read PlayerView from input stream
         */
        @Throws(java.io.IOException::class)
        fun readFrom(stream: java.io.InputStream): PlayerView {
            var myId: Int
            myId = StreamUtil.readInt(stream)
            var mapSize: Int
            mapSize = StreamUtil.readInt(stream)
            var fogOfWar: Boolean
            fogOfWar = StreamUtil.readBoolean(stream)
            var entityProperties: MutableMap<model.EntityType, model.EntityProperties>
            val entityPropertiesSize = StreamUtil.readInt(stream)
            entityProperties = mutableMapOf();
            for (entityPropertiesIndex in 0 until entityPropertiesSize) {
                var entityPropertiesKey: model.EntityType
                entityPropertiesKey = model.EntityType.readFrom(stream)
                var entityPropertiesValue: model.EntityProperties
                entityPropertiesValue = model.EntityProperties.readFrom(stream)
                entityProperties.put(entityPropertiesKey, entityPropertiesValue)
            }
            var maxTickCount: Int
            maxTickCount = StreamUtil.readInt(stream)
            var maxPathfindNodes: Int
            maxPathfindNodes = StreamUtil.readInt(stream)
            var currentTick: Int
            currentTick = StreamUtil.readInt(stream)
            var players: Array<model.Player>
            players = Array(StreamUtil.readInt(stream), {
                var playersElement: model.Player
                playersElement = model.Player.readFrom(stream)
                playersElement
            })
            var entities: Array<model.Entity>
            entities = Array(StreamUtil.readInt(stream), {
                var entitiesElement: model.Entity
                entitiesElement = model.Entity.readFrom(stream)
                entitiesElement
            })
            return PlayerView(myId, mapSize, fogOfWar, entityProperties, maxTickCount, maxPathfindNodes, currentTick, players, entities)
        }
    }
}