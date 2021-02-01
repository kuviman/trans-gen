<?php

require_once 'EntityType.php';

/**
 * Entity's repair properties
 */
class RepairProperties
{
    /**
     * Valid target entity types
     */
    public $validTargets;
    /**
     * Health restored in one tick
     */
    public $power;

    function __construct($validTargets, $power)
    {
        $this->validTargets = $validTargets;
        $this->power = $power;
    }

    /**
     * Read RepairProperties from input stream
     */
    public static function readFrom($stream)
    {
        $validTargets = [];
        $validTargetsSize = $stream->readInt32();
        for ($validTargetsIndex = 0; $validTargetsIndex < $validTargetsSize; $validTargetsIndex++) {
            $validTargetsElement = EntityType::readFrom($stream);
            $validTargets[] = $validTargetsElement;
        }
        $power = $stream->readInt32();
        return new RepairProperties($validTargets, $power);
    }

    /**
     * Write RepairProperties to output stream
     */
    public function writeTo($stream)
    {
        $stream->writeInt32(count($this->validTargets));
        foreach ($this->validTargets as $element) {
            $stream->writeInt32($element);
        }
        $stream->writeInt32($this->power);
    }
}