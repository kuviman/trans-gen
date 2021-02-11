<?php

namespace Model {
    require_once 'Model/AttackAction.php';
    require_once 'Model/BuildAction.php';
    require_once 'Model/MoveAction.php';
    require_once 'Model/RepairAction.php';
    require_once 'Stream.php';

    /**
     * Entity's action
     */
    class EntityAction
    {
        /**
         * Move action
         */
        public ?\Model\MoveAction $moveAction;
        /**
         * Build action
         */
        public ?\Model\BuildAction $buildAction;
        /**
         * Attack action
         */
        public ?\Model\AttackAction $attackAction;
        /**
         * Repair action
         */
        public ?\Model\RepairAction $repairAction;
    
        function __construct(?\Model\MoveAction $moveAction, ?\Model\BuildAction $buildAction, ?\Model\AttackAction $attackAction, ?\Model\RepairAction $repairAction)
        {
            $this->moveAction = $moveAction;
            $this->buildAction = $buildAction;
            $this->attackAction = $attackAction;
            $this->repairAction = $repairAction;
        }
    
        /**
         * Read EntityAction from input stream
         */
        public static function readFrom(\InputStream $stream): EntityAction
        {
            if ($stream->readBool()) {
                $moveAction = \Model\MoveAction::readFrom($stream);
            } else {
                $moveAction = NULL;
            }
            if ($stream->readBool()) {
                $buildAction = \Model\BuildAction::readFrom($stream);
            } else {
                $buildAction = NULL;
            }
            if ($stream->readBool()) {
                $attackAction = \Model\AttackAction::readFrom($stream);
            } else {
                $attackAction = NULL;
            }
            if ($stream->readBool()) {
                $repairAction = \Model\RepairAction::readFrom($stream);
            } else {
                $repairAction = NULL;
            }
            return new EntityAction($moveAction, $buildAction, $attackAction, $repairAction);
        }
        
        /**
         * Write EntityAction to output stream
         */
        public function writeTo(\OutputStream $stream): void
        {
            if (is_null($this->moveAction)) {
                $stream->writeBool(false);
            } else {
                $stream->writeBool(true);
                $this->moveAction->writeTo($stream);
            }
            if (is_null($this->buildAction)) {
                $stream->writeBool(false);
            } else {
                $stream->writeBool(true);
                $this->buildAction->writeTo($stream);
            }
            if (is_null($this->attackAction)) {
                $stream->writeBool(false);
            } else {
                $stream->writeBool(true);
                $this->attackAction->writeTo($stream);
            }
            if (is_null($this->repairAction)) {
                $stream->writeBool(false);
            } else {
                $stream->writeBool(true);
                $this->repairAction->writeTo($stream);
            }
        }
    }
}