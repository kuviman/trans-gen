<?php

namespace Model {
    require_once 'Stream.php';
    require_once 'Vec2Int.php';

    /**
     * Move action
     */
    class MoveAction
    {
        /**
         * Target position
         */
        public \Vec2Int $target;
        /**
         * Whether to try find closest position, if path to target is not found
         */
        public bool $findClosestPosition;
        /**
         * Whether to destroy other entities on the way
         */
        public bool $breakThrough;
    
        function __construct(\Vec2Int $target, bool $findClosestPosition, bool $breakThrough)
        {
            $this->target = $target;
            $this->findClosestPosition = $findClosestPosition;
            $this->breakThrough = $breakThrough;
        }
    
        /**
         * Read MoveAction from input stream
         */
        public static function readFrom(\InputStream $stream): MoveAction
        {
            $target = \Vec2Int::readFrom($stream);
            $findClosestPosition = $stream->readBool();
            $breakThrough = $stream->readBool();
            return new MoveAction($target, $findClosestPosition, $breakThrough);
        }
        
        /**
         * Write MoveAction to output stream
         */
        public function writeTo(\OutputStream $stream): void
        {
            $this->target->writeTo($stream);
            $stream->writeBool($this->findClosestPosition);
            $stream->writeBool($this->breakThrough);
        }
    }
}