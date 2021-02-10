<?php

namespace Model {
    require_once 'Model/EntityType.php';
    require_once 'Vec2Int.php';

    /**
     * Game entity
     */
    class Entity
    {
        /**
         * Entity's ID. Unique for each entity
         */
        public $id;
        /**
         * Entity's owner player ID, if owned by a player
         */
        public $playerId;
        /**
         * Entity's type
         */
        public $entityType;
        /**
         * Entity's position (corner with minimal coordinates)
         */
        public $position;
        /**
         * Current health
         */
        public $health;
        /**
         * If entity is active, it can perform actions
         */
        public $active;
    
        function __construct($id, $playerId, $entityType, $position, $health, $active)
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
        public static function readFrom($stream)
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
        public function writeTo($stream)
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