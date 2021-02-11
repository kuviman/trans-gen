<?php

namespace Model\DebugInterface {
    require_once 'Color.php';
    require_once 'Stream.php';
    require_once 'Vec2Float.php';

    /**
     * Vertex for debug rendering
     */
    class ColoredVertex
    {
        /**
         * Position in world coordinates (if none, screen position (0, 0) is used)
         */
        public ?\Vec2Float $worldPos;
        /**
         * Additional offset in screen coordinates
         */
        public \Vec2Float $screenOffset;
        /**
         * Color to use
         */
        public \Color $color;
    
        function __construct(?\Vec2Float $worldPos, \Vec2Float $screenOffset, \Color $color)
        {
            $this->worldPos = $worldPos;
            $this->screenOffset = $screenOffset;
            $this->color = $color;
        }
    
        /**
         * Read ColoredVertex from input stream
         */
        public static function readFrom(\InputStream $stream): ColoredVertex
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
        public function writeTo(\OutputStream $stream): void
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