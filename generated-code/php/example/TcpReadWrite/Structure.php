<?php

namespace  {
    require_once 'Stream.php';

    /**
     * Example structure
     */
    class Structure
    {
        /**
         * Text
         */
        public string $text;
        /**
         * 32-bit float
         */
        public float $floatNumber;
        /**
         * 64-bit float
         */
        public float $doubleNumber;
    
        function __construct(string $text, float $floatNumber, float $doubleNumber)
        {
            $this->text = $text;
            $this->floatNumber = $floatNumber;
            $this->doubleNumber = $doubleNumber;
        }
    
        /**
         * Read Structure from input stream
         */
        public static function readFrom(\InputStream $stream): Structure
        {
            $text = $stream->readString();
            $floatNumber = $stream->readFloat32();
            $doubleNumber = $stream->readDouble();
            return new Structure($text, $floatNumber, $doubleNumber);
        }
        
        /**
         * Write Structure to output stream
         */
        public function writeTo(\OutputStream $stream): void
        {
            $stream->writeString($this->text);
            $stream->writeFloat32($this->floatNumber);
            $stream->writeDouble($this->doubleNumber);
        }
    }
}