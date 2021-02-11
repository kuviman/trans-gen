<?php

namespace  {
    require_once 'Stream.php';

    /**
     * RGBA Color
     */
    class Color
    {
        /**
         * Red component
         */
        public float $r;
        /**
         * Green component
         */
        public float $g;
        /**
         * Blue component
         */
        public float $b;
        /**
         * Alpha (opacity) component
         */
        public float $a;
    
        function __construct(float $r, float $g, float $b, float $a)
        {
            $this->r = $r;
            $this->g = $g;
            $this->b = $b;
            $this->a = $a;
        }
    
        /**
         * Read Color from input stream
         */
        public static function readFrom(\InputStream $stream): Color
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
        public function writeTo(\OutputStream $stream): void
        {
            $stream->writeFloat32($this->r);
            $stream->writeFloat32($this->g);
            $stream->writeFloat32($this->b);
            $stream->writeFloat32($this->a);
        }
    }
}