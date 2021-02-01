<?php



/**
 * Entity's attack properties
 */
class AttackProperties
{
    /**
     * Maximum attack range
     */
    public $attackRange;
    /**
     * Damage dealt in one tick
     */
    public $damage;
    /**
     * If true, dealing damage will collect resource from target
     */
    public $collectResource;

    function __construct($attackRange, $damage, $collectResource)
    {
        $this->attackRange = $attackRange;
        $this->damage = $damage;
        $this->collectResource = $collectResource;
    }

    /**
     * Read AttackProperties from input stream
     */
    public static function readFrom($stream)
    {
        $attackRange = $stream->readInt32();
        $damage = $stream->readInt32();
        $collectResource = $stream->readBool();
        return new AttackProperties($attackRange, $damage, $collectResource);
    }

    /**
     * Write AttackProperties to output stream
     */
    public function writeTo($stream)
    {
        $stream->writeInt32($this->attackRange);
        $stream->writeInt32($this->damage);
        $stream->writeBool($this->collectResource);
    }
}