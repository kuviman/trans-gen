<?php

namespace Model {
    require_once 'Stream.php';

    /**
     * Player (strategy, client)
     */
    class Player
    {
        /**
         * Player's ID
         */
        public int $id;
        /**
         * Current score
         */
        public int $score;
        /**
         * Current amount of resource
         */
        public int $resource;
    
        function __construct(int $id, int $score, int $resource)
        {
            $this->id = $id;
            $this->score = $score;
            $this->resource = $resource;
        }
    
        /**
         * Read Player from input stream
         */
        public static function readFrom(\InputStream $stream): Player
        {
            $id = $stream->readInt32();
            $score = $stream->readInt32();
            $resource = $stream->readInt32();
            return new Player($id, $score, $resource);
        }
        
        /**
         * Write Player to output stream
         */
        public function writeTo(\OutputStream $stream): void
        {
            $stream->writeInt32($this->id);
            $stream->writeInt32($this->score);
            $stream->writeInt32($this->resource);
        }
    }
}