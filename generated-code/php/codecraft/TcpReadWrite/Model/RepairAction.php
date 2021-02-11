<?php

namespace Model {
    require_once 'Stream.php';

    /**
     * Repair action
     */
    class RepairAction
    {
        /**
         * Target entity's ID
         */
        public int $target;
    
        function __construct(int $target)
        {
            $this->target = $target;
        }
    
        /**
         * Read RepairAction from input stream
         */
        public static function readFrom(\InputStream $stream): RepairAction
        {
            $target = $stream->readInt32();
            return new RepairAction($target);
        }
        
        /**
         * Write RepairAction to output stream
         */
        public function writeTo(\OutputStream $stream): void
        {
            $stream->writeInt32($this->target);
        }
    }
}