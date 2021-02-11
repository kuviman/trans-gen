<?php

namespace Model {
    require_once 'Model/EntityType.php';
    require_once 'Stream.php';
    require_once 'Vec2Int.php';

    /**
     * Game entity
     */
    class Entity
    {
        /**
         * Entity's ID. Unique for each entity
         */
        public int $id;
        /**
         * Entity's owner player ID, if owned by a player
         */
        public ?int $playerId;
        /**
         * Entity's type
         */
        public int $entityType;
        /**
         * Entity's position (corner with minimal coordinates)
         */
        public \Vec2Int $position;
        /**
         * Current health
         */
        public int $health;
        /**
         * If entity is active, it can perform actions
         */
        public bool $active;
    
        function __construct(int $id, ?int $playerId, int $entityType, \Vec2Int $position, int $health, bool $active)
        {
            $this->id = $id;
            $this->playerId = $playerId;
            $this->entityType = $entityType;
            $this->position = $position;
            $this->health = $health;
            $this->active = $active;
        }
    
        /**
         * Read Entity from input stream
         */
        public static function readFrom(\InputStream $stream): Entity
        {
            $id = $stream->readInt32();
            if ($stream->readBool()) {
                $playerId = $stream->readInt32();
            } else {
                $playerId = NULL;
            }
            $entityType = \Model\EntityType::readFrom($stream);
            $position = \Vec2Int::readFrom($stream);
            $health = $stream->readInt32();
            $active = $stream->readBool();
            return new Entity($id, $playerId, $entityType, $position, $health, $active);
        }
        
        /**
         * Write Entity to output stream
         */
        public function writeTo(\OutputStream $stream): void
        {
            $stream->writeInt32($this->id);
            if (is_null($this->playerId)) {
                $stream->writeBool(false);
            } else {
                $stream->writeBool(true);
                $stream->writeInt32($this->playerId);
            }
            $stream->writeInt32($this->entityType);
            $this->position->writeTo($stream);
            $stream->writeInt32($this->health);
            $stream->writeBool($this->active);
        }
    }
}