<?php

namespace Model {
    require_once 'Model/EntityType.php';

    /**
     * Auto attack options
     */
    class AutoAttack
    {
        /**
         * Maximum distance to pathfind
         */
        public $pathfindRange;
        /**
         * List of target entity types to try to attack. If empty, all types but resource are considered
         */
        public $validTargets;
    
        function __construct($pathfindRange, $validTargets)
        {
            $this->pathfindRange = $pathfindRange;
            $this->validTargets = $validTargets;
        }
    
        /**
         * Read AutoAttack from input stream
         */
        public static function readFrom($stream)
        {
            $pathfindRange = $stream->readInt32();
            $validTargets = [];
            $validTargetsSize = $stream->readInt32();
            for ($validTargetsIndex = 0; $validTargetsIndex < $validTargetsSize; $validTargetsIndex++) {
                $validTargetsElement = \Model\EntityType::readFrom($stream);
                $validTargets[] = $validTargetsElement;
            }
            return new AutoAttack($pathfindRange, $validTargets);
        }
        
        /**
         * Write AutoAttack to output stream
         */
        public function writeTo($stream)
        {
            $stream->writeInt32($this->pathfindRange);
            $stream->writeInt32(count($this->validTargets));
            foreach ($this->validTargets as $element) {
                $stream->writeInt32($element);
            }
        }
    }
}