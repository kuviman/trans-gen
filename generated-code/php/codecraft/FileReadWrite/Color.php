<?php

namespace  {
    

    /**
     * RGBA Color
     */
    class Color
    {
        /**
         * Red component
         */
        public $r;
        /**
         * Green component
         */
        public $g;
        /**
         * Blue component
         */
        public $b;
        /**
         * Alpha (opacity) component
         */
        public $a;
    
        function __construct($r, $g, $b, $a)
        {
            $this->r = $r;
            $this->g = $g;
            $this->b = $b;
            $this->a = $a;
        }
    
        /**
         * Read Color from input stream
         */
        public static function readFrom($stream)
        {
            $r = $stream->readFloat32();
            $g = $stream->readFloat32();
            $b = $stream->readFloat32();
            $a = $stream->readFloat32();
            return new Color($r, $g, $b, $a);
        }
    
        /**
         * Write Color to output stream
         */
        public function writeTo($stream)
        {
            $stream->writeFloat32($this->r);
            $stream->writeFloat32($this->g);
            $stream->writeFloat32($this->b);
            $stream->writeFloat32($this->a);
        }
    }
}