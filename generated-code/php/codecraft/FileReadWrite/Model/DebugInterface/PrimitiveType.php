<?php

namespace Model\DebugInterface {
    /**
     * Primitive type for debug rendering
     */
    abstract class PrimitiveType
    {
        /**
         * Lines, number of vertices should be divisible by 2
         */
        const LINES = 0;

        /**
         * Triangles, number of vertices should be divisible by 3
         */
        const TRIANGLES = 1;

        /**
         * Read PrimitiveType from input stream
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
}