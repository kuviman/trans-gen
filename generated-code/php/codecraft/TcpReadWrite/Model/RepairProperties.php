<?php

namespace Model {
    require_once 'Model/EntityType.php';
    require_once 'Stream.php';

    /**
     * Entity's repair properties
     */
    class RepairProperties
    {
        /**
         * Valid target entity types
         */
        public array $validTargets;
        /**
         * Health restored in one tick
         */
        public int $power;
    
        function __construct(array $validTargets, int $power)
        {
            $this->validTargets = $validTargets;
            $this->power = $power;
        }
    
        /**
         * Read RepairProperties from input stream
         */
        public static function readFrom(\InputStream $stream): RepairProperties
        {
            $validTargets = [];
            $validTargetsSize = $stream->readInt32();
            for ($validTargetsIndex = 0; $validTargetsIndex < $validTargetsSize; $validTargetsIndex++) {
                $validTargetsElement = \Model\EntityType::readFrom($stream);
                $validTargets[] = $validTargetsElement;
            }
            $power = $stream->readInt32();
            return new RepairProperties($validTargets, $power);
        }
        
        /**
         * Write RepairProperties to output stream
         */
        public function writeTo(\OutputStream $stream): void
        {
            $stream->writeInt32(count($this->validTargets));
            foreach ($this->validTargets as $element) {
                $stream->writeInt32($element);
            }
            $stream->writeInt32($this->power);
        }
    }
}