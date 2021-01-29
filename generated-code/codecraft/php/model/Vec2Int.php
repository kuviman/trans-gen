<?php



/**
 * 2 dimensional vector.
 */
class Vec2Int
{
    /**
     * `x` coordinate of the vector
     */
    public $x;
    /**
     * `y` coordinate of the vector
     */
    public $y;

    function __construct($x, $y)
    {
        $this->x = $x;
        $this->y = $y;
    }

    /**
     * Read Vec2Int from input stream
     */
    public static function readFrom($stream)
    {
        $x = $stream->readInt32();
        $y = $stream->readInt32();
        return new Vec2Int($x, $y);
    }

    /**
     * Write Vec2Int to output stream
     */
    public function writeTo($stream)
    {
        $stream->writeInt32($this->x);
        $stream->writeInt32($this->y);
    }
}