<?php

namespace Model {
    require_once 'Model/AttackProperties.php';
    require_once 'Model/BuildProperties.php';
    require_once 'Model/RepairProperties.php';
    require_once 'Stream.php';

    /**
     * Entity properties
     */
    class EntityProperties
    {
        /**
         * Size. Entity has a form of a square with side of this length
         */
        public int $size;
        /**
         * Score for building this entity
         */
        public int $buildScore;
        /**
         * Score for destroying this entity
         */
        public int $destroyScore;
        /**
         * Whether this entity can move
         */
        public bool $canMove;
        /**
         * Number of population points this entity provides, if active
         */
        public int $populationProvide;
        /**
         * Number of population points this entity uses
         */
        public int $populationUse;
        /**
         * Maximum health points
         */
        public int $maxHealth;
        /**
         * Cost to build this first entity of this type. If this is a unit (entity can move), the cost is increased by 1 for each existing unit of this type
         */
        public int $initialCost;
        /**
         * If fog of war is enabled, maximum distance at which other entities are considered visible
         */
        public int $sightRange;
        /**
         * Amount of resource added to enemy able to collect resource on dealing damage for 1 health point
         */
        public int $resourcePerHealth;
        /**
         * Build properties, if entity can build
         */
        public ?\Model\BuildProperties $build;
        /**
         * Attack properties, if entity can attack
         */
        public ?\Model\AttackProperties $attack;
        /**
         * Repair properties, if entity can repair
         */
        public ?\Model\RepairProperties $repair;
    
        function __construct(int $size, int $buildScore, int $destroyScore, bool $canMove, int $populationProvide, int $populationUse, int $maxHealth, int $initialCost, int $sightRange, int $resourcePerHealth, ?\Model\BuildProperties $build, ?\Model\AttackProperties $attack, ?\Model\RepairProperties $repair)
        {
            $this->size = $size;
            $this->buildScore = $buildScore;
            $this->destroyScore = $destroyScore;
            $this->canMove = $canMove;
            $this->populationProvide = $populationProvide;
            $this->populationUse = $populationUse;
            $this->maxHealth = $maxHealth;
            $this->initialCost = $initialCost;
            $this->sightRange = $sightRange;
            $this->resourcePerHealth = $resourcePerHealth;
            $this->build = $build;
            $this->attack = $attack;
            $this->repair = $repair;
        }
    
        /**
         * Read EntityProperties from input stream
         */
        public static function readFrom(\InputStream $stream): EntityProperties
        {
            $size = $stream->readInt32();
            $buildScore = $stream->readInt32();
            $destroyScore = $stream->readInt32();
            $canMove = $stream->readBool();
            $populationProvide = $stream->readInt32();
            $populationUse = $stream->readInt32();
            $maxHealth = $stream->readInt32();
            $initialCost = $stream->readInt32();
            $sightRange = $stream->readInt32();
            $resourcePerHealth = $stream->readInt32();
            if ($stream->readBool()) {
                $build = \Model\BuildProperties::readFrom($stream);
            } else {
                $build = NULL;
            }
            if ($stream->readBool()) {
                $attack = \Model\AttackProperties::readFrom($stream);
            } else {
                $attack = NULL;
            }
            if ($stream->readBool()) {
                $repair = \Model\RepairProperties::readFrom($stream);
            } else {
                $repair = NULL;
            }
            return new EntityProperties($size, $buildScore, $destroyScore, $canMove, $populationProvide, $populationUse, $maxHealth, $initialCost, $sightRange, $resourcePerHealth, $build, $attack, $repair);
        }
        
        /**
         * Write EntityProperties to output stream
         */
        public function writeTo(\OutputStream $stream): void
        {
            $stream->writeInt32($this->size);
            $stream->writeInt32($this->buildScore);
            $stream->writeInt32($this->destroyScore);
            $stream->writeBool($this->canMove);
            $stream->writeInt32($this->populationProvide);
            $stream->writeInt32($this->populationUse);
            $stream->writeInt32($this->maxHealth);
            $stream->writeInt32($this->initialCost);
            $stream->writeInt32($this->sightRange);
            $stream->writeInt32($this->resourcePerHealth);
            if (is_null($this->build)) {
                $stream->writeBool(false);
            } else {
                $stream->writeBool(true);
                $this->build->writeTo($stream);
            }
            if (is_null($this->attack)) {
                $stream->writeBool(false);
            } else {
                $stream->writeBool(true);
                $this->attack->writeTo($stream);
            }
            if (is_null($this->repair)) {
                $stream->writeBool(false);
            } else {
                $stream->writeBool(true);
                $this->repair->writeTo($stream);
            }
        }
    }
}