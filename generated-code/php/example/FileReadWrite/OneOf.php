<?php

namespace  {
    
    

    /**
     * Oneof example
     */
    abstract class OneOf
    {
        /**
         * Write OneOf to output stream
         */
        abstract function writeTo($stream);

        /**
         * Read OneOf from input stream
         */
        static function readFrom($stream)
        {
            $tag = $stream->readInt32();
            if ($tag == \OneOf\OptionOne::TAG) {
                return \OneOf\OptionOne::readFrom($stream);
            }
            if ($tag == \OneOf\OptionTwo::TAG) {
                return \OneOf\OptionTwo::readFrom($stream);
            }
            throw new Exception('Unexpected tag value');
        }
    }
}

namespace OneOf {
    /**
     * First option
     */
    class OptionOne extends \OneOf
    {
        const TAG = 0;
    
        /**
         * List of integers
         */
        public $vecInt;
        /**
         * Long integer
         */
        public $longInt;
    
        function __construct($vecInt, $longInt)
        {
            $this->vecInt = $vecInt;
            $this->longInt = $longInt;
        }
    
        /**
         * Read OptionOne from input stream
         */
        public static function readFrom($stream)
        {
            $vecInt = [];
            $vecIntSize = $stream->readInt32();
            for ($vecIntIndex = 0; $vecIntIndex < $vecIntSize; $vecIntIndex++) {
                $vecIntElement = $stream->readInt32();
                $vecInt[] = $vecIntElement;
            }
            $longInt = $stream->readInt64();
            return new OptionOne($vecInt, $longInt);
        }
        
        /**
         * Write OptionOne to output stream
         */
        public function writeTo($stream)
        {
            $stream->writeInt32(OptionOne::TAG);
            $stream->writeInt32(count($this->vecInt));
            foreach ($this->vecInt as $element) {
                $stream->writeInt32($element);
            }
            $stream->writeInt64($this->longInt);
        }
    }

    /**
     * Second option
     */
    class OptionTwo extends \OneOf
    {
        const TAG = 1;
    
        /**
         * usize
         */
        public $value;
    
        function __construct($value)
        {
            $this->value = $value;
        }
    
        /**
         * Read OptionTwo from input stream
         */
        public static function readFrom($stream)
        {
            $value = $stream->readInt32();
            return new OptionTwo($value);
        }
        
        /**
         * Write OptionTwo to output stream
         */
        public function writeTo($stream)
        {
            $stream->writeInt32(OptionTwo::TAG);
            $stream->writeInt32($this->value);
        }
    }
}