import { Entity } from "./entity";
import { EntityProperties } from "./entity-properties";
import { EntityType } from "./entity-type";
import { Player } from "./player";
import { Stream } from "../stream";

/**
 * Information available to the player
 */
export class PlayerView {
    /**
     * Your player's ID
     */
    myId: number
    /**
     * Size of the map
     */
    mapSize: number
    /**
     * Whether fog of war is enabled
     */
    fogOfWar: boolean
    /**
     * Entity properties for each entity type
     */
    entityProperties: Map<EntityType, EntityProperties>
    /**
     * Max tick count for the game
     */
    maxTickCount: number
    /**
     * Max pathfind nodes when performing pathfinding in the game simulator
     */
    maxPathfindNodes: number
    /**
     * Current tick
     */
    currentTick: number
    /**
     * List of players
     */
    players: Array<Player>
    /**
     * List of entities
     */
    entities: Array<Entity>

    constructor(myId: number, mapSize: number, fogOfWar: boolean, entityProperties: Map<EntityType, EntityProperties>, maxTickCount: number, maxPathfindNodes: number, currentTick: number, players: Array<Player>, entities: Array<Entity>) {
        this.myId = myId;
        this.mapSize = mapSize;
        this.fogOfWar = fogOfWar;
        this.entityProperties = entityProperties;
        this.maxTickCount = maxTickCount;
        this.maxPathfindNodes = maxPathfindNodes;
        this.currentTick = currentTick;
        this.players = players;
        this.entities = entities;
    }

    /**
     * Read PlayerView from input stream
     */
    static async readFrom(stream: Stream): Promise<PlayerView> {
        let myId;
        myId = await stream.readInt();
        let mapSize;
        mapSize = await stream.readInt();
        let fogOfWar;
        fogOfWar = await stream.readBool();
        let entityProperties;
        entityProperties = new Map();
        for (let entityPropertiesCount = await stream.readInt(); entityPropertiesCount > 0; entityPropertiesCount--) {
            let entityPropertiesKey;
            let entityPropertiesValue;
            entityPropertiesKey = await EntityType.readFrom(stream);
            entityPropertiesValue = await EntityProperties.readFrom(stream);
            entityProperties.set(entityPropertiesKey, entityPropertiesValue)
        }
        let maxTickCount;
        maxTickCount = await stream.readInt();
        let maxPathfindNodes;
        maxPathfindNodes = await stream.readInt();
        let currentTick;
        currentTick = await stream.readInt();
        let players;
        players = [];
        for (let playersCount = await stream.readInt(); playersCount > 0; playersCount--) {
            let playersElement;
            playersElement = await Player.readFrom(stream);
            players.push(playersElement);
        }
        let entities;
        entities = [];
        for (let entitiesCount = await stream.readInt(); entitiesCount > 0; entitiesCount--) {
            let entitiesElement;
            entitiesElement = await Entity.readFrom(stream);
            entities.push(entitiesElement);
        }
        return new PlayerView(myId, mapSize, fogOfWar, entityProperties, maxTickCount, maxPathfindNodes, currentTick, players, entities)
    }

    /**
     * Write PlayerView to output stream
     */
    async writeTo(stream: Stream) {
        let myId = this.myId;
        await stream.writeInt(myId);
        let mapSize = this.mapSize;
        await stream.writeInt(mapSize);
        let fogOfWar = this.fogOfWar;
        await stream.writeBool(fogOfWar);
        let entityProperties = this.entityProperties;
        await stream.writeInt(entityProperties.size);
        for (let [entityPropertiesKey, entityPropertiesValue] of entityProperties) {
            await entityPropertiesKey.writeTo(stream);
            await entityPropertiesValue.writeTo(stream);
        }
        let maxTickCount = this.maxTickCount;
        await stream.writeInt(maxTickCount);
        let maxPathfindNodes = this.maxPathfindNodes;
        await stream.writeInt(maxPathfindNodes);
        let currentTick = this.currentTick;
        await stream.writeInt(currentTick);
        let players = this.players;
        await stream.writeInt(players.length);
        for (let playersElement of players) {
            await playersElement.writeTo(stream);
        }
        let entities = this.entities;
        await stream.writeInt(entities.length);
        for (let entitiesElement of entities) {
            await entitiesElement.writeTo(stream);
        }
    }
}