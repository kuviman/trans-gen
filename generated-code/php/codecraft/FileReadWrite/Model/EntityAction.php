<?php

namespace Model {
    require_once 'Model/AttackAction.php';
    require_once 'Model/BuildAction.php';
    require_once 'Model/MoveAction.php';
    require_once 'Model/RepairAction.php';

    /**
     * Entity's action
     */
    class EntityAction
    {
        /**
         * Move action
         */
        public $moveAction;
        /**
         * Build action
         */
        public $buildAction;
        /**
         * Attack action
         */
        public $attackAction;
        /**
         * Repair action
         */
        public $repairAction;
    
        function __construct($moveAction, $buildAction, $attackAction, $repairAction)
        {
            $this->moveAction = $moveAction;
            $this->buildAction = $buildAction;
            $this->attackAction = $attackAction;
            $this->repairAction = $repairAction;
        }
    
        /**
         * Read EntityAction from input stream
         */
        public static function readFrom($stream)
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
        public function writeTo($stream)
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