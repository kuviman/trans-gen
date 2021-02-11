<?php

namespace Model {
    require_once 'Model/AutoAttack.php';
    require_once 'Stream.php';

    /**
     * Attack action
     */
    class AttackAction
    {
        /**
         * If specified, target entity's ID
         */
        public ?int $target;
        /**
         * If specified, configures auto attacking
         */
        public ?\Model\AutoAttack $autoAttack;
    
        function __construct(?int $target, ?\Model\AutoAttack $autoAttack)
        {
            $this->target = $target;
            $this->autoAttack = $autoAttack;
        }
    
        /**
         * Read AttackAction from input stream
         */
        public static function readFrom(\InputStream $stream): AttackAction
        {
            if ($stream->readBool()) {
                $target = $stream->readInt32();
            } else {
                $target = NULL;
            }
            if ($stream->readBool()) {
                $autoAttack = \Model\AutoAttack::readFrom($stream);
            } else {
                $autoAttack = NULL;
            }
            return new AttackAction($target, $autoAttack);
        }
        
        /**
         * Write AttackAction to output stream
         */
        public function writeTo(\OutputStream $stream): void
        {
            if (is_null($this->target)) {
                $stream->writeBool(false);
            } else {
                $stream->writeBool(true);
                $stream->writeInt32($this->target);
            }
            if (is_null($this->autoAttack)) {
                $stream->writeBool(false);
            } else {
                $stream->writeBool(true);
                $this->autoAttack->writeTo($stream);
            }
        }
    }
}