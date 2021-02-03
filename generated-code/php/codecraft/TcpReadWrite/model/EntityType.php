<?php

/**
 * Entity type
 */
abstract class EntityType
{
    /**
     * Wall, can be used to prevent enemy from moving through
     */
    const WALL = 0;

    /**
     * House, used to increase population
     */
    const HOUSE = 1;

    /**
     * Base for recruiting new builder units
     */
    const BUILDER_BASE = 2;

    /**
     * Builder unit can build buildings
     */
    const BUILDER_UNIT = 3;

    /**
     * Base for recruiting new melee units
     */
    const MELEE_BASE = 4;

    /**
     * Melee unit
     */
    const MELEE_UNIT = 5;

    /**
     * Base for recruiting new ranged units
     */
    const RANGED_BASE = 6;

    /**
     * Ranged unit
     */
    const RANGED_UNIT = 7;

    /**
     * Resource can be harvested
     */
    const RESOURCE = 8;

    /**
     * Ranged attacking building
     */
    const TURRET = 9;

    /**
     * Read EntityType from input stream
     */
    public static function readFrom($stream)
    {
        $result = $stream->readInt32();
        if (0 <= $result && $result < 10) {
            return $result;
        }
        throw new Exception('Unexpected tag value');
    }
}