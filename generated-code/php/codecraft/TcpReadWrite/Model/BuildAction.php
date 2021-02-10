<?php

namespace Model {
    require_once 'Model/EntityType.php';
    require_once 'Vec2Int.php';

    /**
     * Build action
     */
    class BuildAction
    {
        /**
         * Type of an entity to build
         */
        public $entityType;
        /**
         * Desired position of new entity
         */
        public $position;
    
        function __construct($entityType, $position)
        {
            $this->entityType = $entityType;
            $this->position = $position;
        }
    
        /**
         * Read BuildAction from input stream
         */
        public static function readFrom($stream)
        {
            $entityType = \Model\EntityType::readFrom($stream);
            $position = \Vec2Int::readFrom($stream);
            return new BuildAction($entityType, $position);
        }
        
        /**
         * Write BuildAction to output stream
         */
        public function writeTo($stream)
        {
            $stream->writeInt32($this->entityType);
            $this->position->writeTo($stream);
        }
    }
}