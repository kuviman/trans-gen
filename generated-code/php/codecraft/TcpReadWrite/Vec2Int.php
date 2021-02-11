<?php

namespace  {
    require_once 'Stream.php';

    /**
     * 2 dimensional vector.
     */
    class Vec2Int
    {
        /**
         * `x` coordinate of the vector
         */
        public int $x;
        /**
         * `y` coordinate of the vector
         */
        public int $y;
    
        function __construct(int $x, int $y)
        {
            $this->x = $x;
            $this->y = $y;
        }
    
        /**
         * Read Vec2Int from input stream
         */
        public static function readFrom(\InputStream $stream): Vec2Int
        {
            $x = $stream->readInt32();
            $y = $stream->readInt32();
            return new Vec2Int($x, $y);
        }
        
        /**
         * Write Vec2Int to output stream
         */
        public function writeTo(\OutputStream $stream): void
        {
            $stream->writeInt32($this->x);
            $stream->writeInt32($this->y);
        }
    }
}