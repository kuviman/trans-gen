<?php

namespace Model {
    require_once 'Model/Entity.php';
    require_once 'Model/EntityProperties.php';
    require_once 'Model/EntityType.php';
    require_once 'Model/Player.php';
    require_once 'Stream.php';

    /**
     * Information available to the player
     */
    class PlayerView
    {
        /**
         * Your player's ID
         */
        public int $myId;
        /**
         * Size of the map
         */
        public int $mapSize;
        /**
         * Whether fog of war is enabled
         */
        public bool $fogOfWar;
        /**
         * Entity properties for each entity type
         */
        public array $entityProperties;
        /**
         * Max tick count for the game
         */
        public int $maxTickCount;
        /**
         * Max pathfind nodes when performing pathfinding in the game simulator
         */
        public int $maxPathfindNodes;
        /**
         * Current tick
         */
        public int $currentTick;
        /**
         * List of players
         */
        public array $players;
        /**
         * List of entities
         */
        public array $entities;
    
        function __construct(int $myId, int $mapSize, bool $fogOfWar, array $entityProperties, int $maxTickCount, int $maxPathfindNodes, int $currentTick, array $players, array $entities)
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
        public static function readFrom(\InputStream $stream): PlayerView
        {
            $myId = $stream->readInt32();
            $mapSize = $stream->readInt32();
            $fogOfWar = $stream->readBool();
            $entityProperties = [];
            $entityPropertiesSize = $stream->readInt32();
            for ($entityPropertiesIndex = 0; $entityPropertiesIndex < $entityPropertiesSize; $entityPropertiesIndex++) {
                $entityPropertiesKey = \Model\EntityType::readFrom($stream);
                $entityPropertiesValue = \Model\EntityProperties::readFrom($stream);
                $entityProperties[$entityPropertiesKey] = $entityPropertiesValue;
            }
            $maxTickCount = $stream->readInt32();
            $maxPathfindNodes = $stream->readInt32();
            $currentTick = $stream->readInt32();
            $players = [];
            $playersSize = $stream->readInt32();
            for ($playersIndex = 0; $playersIndex < $playersSize; $playersIndex++) {
                $playersElement = \Model\Player::readFrom($stream);
                $players[] = $playersElement;
            }
            $entities = [];
            $entitiesSize = $stream->readInt32();
            for ($entitiesIndex = 0; $entitiesIndex < $entitiesSize; $entitiesIndex++) {
                $entitiesElement = \Model\Entity::readFrom($stream);
                $entities[] = $entitiesElement;
            }
            return new PlayerView($myId, $mapSize, $fogOfWar, $entityProperties, $maxTickCount, $maxPathfindNodes, $currentTick, $players, $entities);
        }
        
        /**
         * Write PlayerView to output stream
         */
        public function writeTo(\OutputStream $stream): void
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
}