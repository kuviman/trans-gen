<?php

namespace Model {
    require_once 'Vec2Int.php';

    /**
     * Move action
     */
    class MoveAction
    {
        /**
         * Target position
         */
        public $target;
        /**
         * Whether to try find closest position, if path to target is not found
         */
        public $findClosestPosition;
        /**
         * Whether to destroy other entities on the way
         */
        public $breakThrough;
    
        function __construct($target, $findClosestPosition, $breakThrough)
        {
            $this->target = $target;
            $this->findClosestPosition = $findClosestPosition;
            $this->breakThrough = $breakThrough;
        }
    
        /**
         * Read MoveAction from input stream
         */
        public static function readFrom($stream)
        {
            $target = \Vec2Int::readFrom($stream);
            $findClosestPosition = $stream->readBool();
            $breakThrough = $stream->readBool();
            return new MoveAction($target, $findClosestPosition, $breakThrough);
        }
        
        /**
         * Write MoveAction to output stream
         */
        public function writeTo($stream)
        {
            $this->target->writeTo($stream);
            $stream->writeBool($this->findClosestPosition);
            $stream->writeBool($this->breakThrough);
        }
    }
}