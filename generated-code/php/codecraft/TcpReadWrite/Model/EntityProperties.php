<?php

namespace Model {
    require_once 'Model/AttackProperties.php';
    require_once 'Model/BuildProperties.php';
    require_once 'Model/RepairProperties.php';

    /**
     * Entity properties
     */
    class EntityProperties
    {
        /**
         * Size. Entity has a form of a square with side of this length
         */
        public $size;
        /**
         * Score for building this entity
         */
        public $buildScore;
        /**
         * Score for destroying this entity
         */
        public $destroyScore;
        /**
         * Whether this entity can move
         */
        public $canMove;
        /**
         * Number of population points this entity provides, if active
         */
        public $populationProvide;
        /**
         * Number of population points this entity uses
         */
        public $populationUse;
        /**
         * Maximum health points
         */
        public $maxHealth;
        /**
         * Cost to build this first entity of this type. If this is a unit (entity can move), the cost is increased by 1 for each existing unit of this type
         */
        public $initialCost;
        /**
         * If fog of war is enabled, maximum distance at which other entities are considered visible
         */
        public $sightRange;
        /**
         * Amount of resource added to enemy able to collect resource on dealing damage for 1 health point
         */
        public $resourcePerHealth;
        /**
         * Build properties, if entity can build
         */
        public $build;
        /**
         * Attack properties, if entity can attack
         */
        public $attack;
        /**
         * Repair properties, if entity can repair
         */
        public $repair;
    
        function __construct($size, $buildScore, $destroyScore, $canMove, $populationProvide, $populationUse, $maxHealth, $initialCost, $sightRange, $resourcePerHealth, $build, $attack, $repair)
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
        public static function readFrom($stream)
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
        public function writeTo($stream)
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