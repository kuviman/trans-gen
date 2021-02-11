<?php

namespace Model {
    require_once 'Model/EntityAction.php';
    require_once 'Stream.php';

    /**
     * Player's action
     */
    class Action
    {
        /**
         * New actions for entities. If entity does not get new action, if will continue to perform previously set one
         */
        public array $entityActions;
    
        function __construct(array $entityActions)
        {
            $this->entityActions = $entityActions;
        }
    
        /**
         * Read Action from input stream
         */
        public static function readFrom(\InputStream $stream): Action
        {
            $entityActions = [];
            $entityActionsSize = $stream->readInt32();
            for ($entityActionsIndex = 0; $entityActionsIndex < $entityActionsSize; $entityActionsIndex++) {
                $entityActionsKey = $stream->readInt32();
                $entityActionsValue = \Model\EntityAction::readFrom($stream);
                $entityActions[$entityActionsKey] = $entityActionsValue;
            }
            return new Action($entityActions);
        }
        
        /**
         * Write Action to output stream
         */
        public function writeTo(\OutputStream $stream): void
        {
            $stream->writeInt32(count($this->entityActions));
            foreach ($this->entityActions as $key => $value) {
                $stream->writeInt32($key);
                $value->writeTo($stream);
            }
        }
    }
}