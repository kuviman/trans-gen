<?php

namespace  {
    

    /**
     * 2 dimensional vector.
     */
    class Vec2Float
    {
        /**
         * `x` coordinate of the vector
         */
        public $x;
        /**
         * `y` coordinate of the vector
         */
        public $y;
    
        function __construct($x, $y)
        {
            $this->x = $x;
            $this->y = $y;
        }
    
        /**
         * Read Vec2Float from input stream
         */
        public static function readFrom($stream)
        {
            $x = $stream->readFloat32();
            $y = $stream->readFloat32();
            return new Vec2Float($x, $y);
        }
    
        /**
         * Write Vec2Float to output stream
         */
        public function writeTo($stream)
        {
            $stream->writeFloat32($this->x);
            $stream->writeFloat32($this->y);
        }
    }
}