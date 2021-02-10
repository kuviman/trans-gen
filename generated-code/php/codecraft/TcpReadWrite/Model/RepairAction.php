<?php

namespace Model {
    

    /**
     * Repair action
     */
    class RepairAction
    {
        /**
         * Target entity's ID
         */
        public $target;
    
        function __construct($target)
        {
            $this->target = $target;
        }
    
        /**
         * Read RepairAction from input stream
         */
        public static function readFrom($stream)
        {
            $target = $stream->readInt32();
            return new RepairAction($target);
        }
    
        /**
         * Write RepairAction to output stream
         */
        public function writeTo($stream)
        {
            $stream->writeInt32($this->target);
        }
    }
}