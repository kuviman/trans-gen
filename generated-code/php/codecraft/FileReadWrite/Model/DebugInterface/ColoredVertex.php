<?php

namespace Model\DebugInterface {
    require_once 'Color.php';
    require_once 'Vec2Float.php';

    /**
     * Vertex for debug rendering
     */
    class ColoredVertex
    {
        /**
         * Position in world coordinates (if none, screen position (0, 0) is used)
         */
        public $worldPos;
        /**
         * Additional offset in screen coordinates
         */
        public $screenOffset;
        /**
         * Color to use
         */
        public $color;
    
        function __construct($worldPos, $screenOffset, $color)
        {
            $this->worldPos = $worldPos;
            $this->screenOffset = $screenOffset;
            $this->color = $color;
        }
    
        /**
         * Read ColoredVertex from input stream
         */
        public static function readFrom($stream)
        {
            if ($stream->readBool()) {
                $worldPos = \Vec2Float::readFrom($stream);
            } else {
                $worldPos = NULL;
            }
            $screenOffset = \Vec2Float::readFrom($stream);
            $color = \Color::readFrom($stream);
            return new ColoredVertex($worldPos, $screenOffset, $color);
        }
        
        /**
         * Write ColoredVertex to output stream
         */
        public function writeTo($stream)
        {
            if (is_null($this->worldPos)) {
                $stream->writeBool(false);
            } else {
                $stream->writeBool(true);
                $this->worldPos->writeTo($stream);
            }
            $this->screenOffset->writeTo($stream);
            $this->color->writeTo($stream);
        }
    }
}