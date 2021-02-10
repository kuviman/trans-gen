<?php

namespace  {
    

    /**
     * Example structure
     */
    class Structure
    {
        /**
         * Text
         */
        public $text;
        /**
         * 32-bit float
         */
        public $floatNumber;
        /**
         * 64-bit float
         */
        public $doubleNumber;
    
        function __construct($text, $floatNumber, $doubleNumber)
        {
            $this->text = $text;
            $this->floatNumber = $floatNumber;
            $this->doubleNumber = $doubleNumber;
        }
    
        /**
         * Read Structure from input stream
         */
        public static function readFrom($stream)
        {
            $text = $stream->readString();
            $floatNumber = $stream->readFloat32();
            $doubleNumber = $stream->readDouble();
            return new Structure($text, $floatNumber, $doubleNumber);
        }
    
        /**
         * Write Structure to output stream
         */
        public function writeTo($stream)
        {
            $stream->writeString($this->text);
            $stream->writeFloat32($this->floatNumber);
            $stream->writeDouble($this->doubleNumber);
        }
    }
}