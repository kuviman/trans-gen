package model

import util.StreamUtil

class PlayerView {
    var myId: Int = 0
    var mapSize: Int = 0
    var fogOfWar: Boolean = false
    lateinit var entityProperties: MutableMap<model.EntityType, model.EntityProperties>
    var maxTickCount: Int = 0
    var maxPathfindNodes: Int = 0
    var currentTick: Int = 0
    lateinit var players: Array<model.Player>
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

    companion object {
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
                when (StreamUtil.readInt(stream)) {
                0 -> entityPropertiesKey = model.EntityType.WALL
                1 -> entityPropertiesKey = model.EntityType.HOUSE
                2 -> entityPropertiesKey = model.EntityType.BUILDER_BASE
                3 -> entityPropertiesKey = model.EntityType.BUILDER_UNIT
                4 -> entityPropertiesKey = model.EntityType.MELEE_BASE
                5 -> entityPropertiesKey = model.EntityType.MELEE_UNIT
                6 -> entityPropertiesKey = model.EntityType.RANGED_BASE
                7 -> entityPropertiesKey = model.EntityType.RANGED_UNIT
                8 -> entityPropertiesKey = model.EntityType.RESOURCE
                9 -> entityPropertiesKey = model.EntityType.TURRET
                else -> throw java.io.IOException("Unexpected tag value")
                }
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