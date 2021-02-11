<?php

namespace Model {
    require_once 'Stream.php';

    /**
     * Entity's attack properties
     */
    class AttackProperties
    {
        /**
         * Maximum attack range
         */
        public int $attackRange;
        /**
         * Damage dealt in one tick
         */
        public int $damage;
        /**
         * If true, dealing damage will collect resource from target
         */
        public bool $collectResource;
    
        function __construct(int $attackRange, int $damage, bool $collectResource)
        {
            $this->attackRange = $attackRange;
            $this->damage = $damage;
            $this->collectResource = $collectResource;
        }
    
        /**
         * Read AttackProperties from input stream
         */
        public static function readFrom(\InputStream $stream): AttackProperties
        {
            $attackRange = $stream->readInt32();
            $damage = $stream->readInt32();
            $collectResource = $stream->readBool();
            return new AttackProperties($attackRange, $damage, $collectResource);
        }
        
        /**
         * Write AttackProperties to output stream
         */
        public function writeTo(\OutputStream $stream): void
        {
            $stream->writeInt32($this->attackRange);
            $stream->writeInt32($this->damage);
            $stream->writeBool($this->collectResource);
        }
    }
}