<?php

require_once 'Entity.php';
require_once 'EntityProperties.php';
require_once 'EntityType.php';
require_once 'Player.php';

/**
 * Information available to the player
 */
class PlayerView
{
    /**
     * Your player's ID
     */
    public $myId;
    /**
     * Size of the map
     */
    public $mapSize;
    /**
     * Whether fog of war is enabled
     */
    public $fogOfWar;
    /**
     * Entity properties for each entity type
     */
    public $entityProperties;
    /**
     * Max tick count for the game
     */
    public $maxTickCount;
    /**
     * Max pathfind nodes when performing pathfinding in the game simulator
     */
    public $maxPathfindNodes;
    /**
     * Current tick
     */
    public $currentTick;
    /**
     * List of players
     */
    public $players;
    /**
     * List of entities
     */
    public $entities;

    function __construct($myId, $mapSize, $fogOfWar, $entityProperties, $maxTickCount, $maxPathfindNodes, $currentTick, $players, $entities)
    {
        $this->myId = $myId;
        $this->mapSize = $mapSize;
        $this->fogOfWar = $fogOfWar;
        $this->entityProperties = $entityProperties;
        $this->maxTickCount = $maxTickCount;
        $this->maxPathfindNodes = $maxPathfindNodes;
        $this->currentTick = $currentTick;
        $this->players = $players;
        $this->entities = $entities;
    }

    /**
     * Read PlayerView from input stream
     */
    public static function readFrom($stream)
    {
        $myId = $stream->readInt32();
        $mapSize = $stream->readInt32();
        $fogOfWar = $stream->readBool();
        $entityProperties = [];
        $entityPropertiesSize = $stream->readInt32();
        for ($entityPropertiesIndex = 0; $entityPropertiesIndex < $entityPropertiesSize; $entityPropertiesIndex++) {
            $entityPropertiesKey = EntityType::readFrom($stream);
            $entityPropertiesValue = EntityProperties::readFrom($stream);
            $entityProperties[$entityPropertiesKey] = $entityPropertiesValue;
        }
        $maxTickCount = $stream->readInt32();
        $maxPathfindNodes = $stream->readInt32();
        $currentTick = $stream->readInt32();
        $players = [];
        $playersSize = $stream->readInt32();
        for ($playersIndex = 0; $playersIndex < $playersSize; $playersIndex++) {
            $playersElement = Player::readFrom($stream);
            $players[] = $playersElement;
        }
        $entities = [];
        $entitiesSize = $stream->readInt32();
        for ($entitiesIndex = 0; $entitiesIndex < $entitiesSize; $entitiesIndex++) {
            $entitiesElement = Entity::readFrom($stream);
            $entities[] = $entitiesElement;
        }
        return new PlayerView($myId, $mapSize, $fogOfWar, $entityProperties, $maxTickCount, $maxPathfindNodes, $currentTick, $players, $entities);
    }

    /**
     * Write PlayerView to output stream
     */
    public function writeTo($stream)
    {
        $stream->writeInt32($this->myId);
        $stream->writeInt32($this->mapSize);
        $stream->writeBool($this->fogOfWar);
        $stream->writeInt32(count($this->entityProperties));
        foreach ($this->entityProperties as $key => $value) {
            $stream->writeInt32($key);
            $value->writeTo($stream);
        }
        $stream->writeInt32($this->maxTickCount);
        $stream->writeInt32($this->maxPathfindNodes);
        $stream->writeInt32($this->currentTick);
        $stream->writeInt32(count($this->players));
        foreach ($this->players as $element) {
            $element->writeTo($stream);
        }
        $stream->writeInt32(count($this->entities));
        foreach ($this->entities as $element) {
            $element->writeTo($stream);
        }
    }
}