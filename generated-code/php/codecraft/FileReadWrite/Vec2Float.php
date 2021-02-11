<?php

namespace  {
    require_once 'Stream.php';

    /**
     * 2 dimensional vector.
     */
    class Vec2Float
    {
        /**
         * `x` coordinate of the vector
         */
        public float $x;
        /**
         * `y` coordinate of the vector
         */
        public float $y;
    
        function __construct(float $x, float $y)
        {
            $this->x = $x;
            $this->y = $y;
        }
    
        /**
         * Read Vec2Float from input stream
         */
        public static function readFrom(\InputStream $stream): Vec2Float
        {
            $x = $stream->readFloat32();
            $y = $stream->readFloat32();
            return new Vec2Float($x, $y);
        }
        
        /**
         * Write Vec2Float to output stream
         */
        public function writeTo(\OutputStream $stream): void
        {
            $stream->writeFloat32($this->x);
            $stream->writeFloat32($this->y);
        }
    }
}