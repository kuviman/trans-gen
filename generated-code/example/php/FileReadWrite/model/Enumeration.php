<?php

/**
 * Example enumeration
 */
abstract class Enumeration
{
    /**
     * First option
     */
    const VALUE_ONE = 0;

    /**
     * Second option
     */
    const VALUE_TWO = 1;

    /**
     * Read Enumeration from input stream
     */
    public static function readFrom($stream)
    {
        $result = $stream->readInt32();
        if (0 <= $result && $result < 2) {
            return $result;
        }
        throw new Exception('Unexpected tag value');
    }
}