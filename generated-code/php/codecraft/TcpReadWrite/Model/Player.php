<?php

namespace Model {
    

    /**
     * Player (strategy, client)
     */
    class Player
    {
        /**
         * Player's ID
         */
        public $id;
        /**
         * Current score
         */
        public $score;
        /**
         * Current amount of resource
         */
        public $resource;
    
        function __construct($id, $score, $resource)
        {
            $this->id = $id;
            $this->score = $score;
            $this->resource = $resource;
        }
    
        /**
         * Read Player from input stream
         */
        public static function readFrom($stream)
        {
            $id = $stream->readInt32();
            $score = $stream->readInt32();
            $resource = $stream->readInt32();
            return new Player($id, $score, $resource);
        }
    
        /**
         * Write Player to output stream
         */
        public function writeTo($stream)
        {
            $stream->writeInt32($this->id);
            $stream->writeInt32($this->score);
            $stream->writeInt32($this->resource);
        }
    }
}