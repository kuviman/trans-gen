<?php

namespace Model\DebugInterface {

    require_once 'Model/DebugInterface/ColoredVertex.php';
    require_once 'Model/DebugInterface/PrimitiveType.php';

    /**
     * Debug data can be drawn in the app
     */
    abstract class DebugData
    {
        /**
         * Write DebugData to output stream
         */
        abstract function writeTo($stream);

        /**
         * Read DebugData from input stream
         */
        static function readFrom($stream)
        {
            $tag = $stream->readInt32();
            if ($tag == \Model\DebugInterface\DebugData\Log::TAG) {
                return \Model\DebugInterface\DebugData\Log::readFrom($stream);
            }
            if ($tag == \Model\DebugInterface\DebugData\Primitives::TAG) {
                return \Model\DebugInterface\DebugData\Primitives::readFrom($stream);
            }
            if ($tag == \Model\DebugInterface\DebugData\PlacedText::TAG) {
                return \Model\DebugInterface\DebugData\PlacedText::readFrom($stream);
            }
            throw new Exception('Unexpected tag value');
        }
    }
}

namespace Model\DebugInterface\DebugData {
    /**
     * Log some text
     */
    class Log extends \Model\DebugInterface\DebugData
    {
        const TAG = 0;
    
        /**
         * Text to show
         */
        public $text;
    
        function __construct($text)
        {
            $this->text = $text;
        }
    
        /**
         * Read Log from input stream
         */
        public static function readFrom($stream)
        {
            $text = $stream->readString();
            return new Log($text);
        }
    
        /**
         * Write Log to output stream
         */
        public function writeTo($stream)
        {
            $stream->writeInt32(Log::TAG);
            $stream->writeString($this->text);
        }
    }

    /**
     * Draw primitives
     */
    class Primitives extends \Model\DebugInterface\DebugData
    {
        const TAG = 1;
    
        /**
         * Vertices
         */
        public $vertices;
        /**
         * Primitive type
         */
        public $primitiveType;
    
        function __construct($vertices, $primitiveType)
        {
            $this->vertices = $vertices;
            $this->primitiveType = $primitiveType;
        }
    
        /**
         * Read Primitives from input stream
         */
        public static function readFrom($stream)
        {
            $vertices = [];
            $verticesSize = $stream->readInt32();
            for ($verticesIndex = 0; $verticesIndex < $verticesSize; $verticesIndex++) {
                $verticesElement = \Model\DebugInterface\ColoredVertex::readFrom($stream);
                $vertices[] = $verticesElement;
            }
            $primitiveType = \Model\DebugInterface\PrimitiveType::readFrom($stream);
            return new Primitives($vertices, $primitiveType);
        }
    
        /**
         * Write Primitives to output stream
         */
        public function writeTo($stream)
        {
            $stream->writeInt32(Primitives::TAG);
            $stream->writeInt32(count($this->vertices));
            foreach ($this->vertices as $element) {
                $element->writeTo($stream);
            }
            $stream->writeInt32($this->primitiveType);
        }
    }

    /**
     * Draw text
     */
    class PlacedText extends \Model\DebugInterface\DebugData
    {
        const TAG = 2;
    
        /**
         * Vertex to determine text position and color
         */
        public $vertex;
        /**
         * Text
         */
        public $text;
        /**
         * Text alignment (0 means left, 0.5 means center, 1 means right)
         */
        public $alignment;
        /**
         * Font size in pixels
         */
        public $size;
    
        function __construct($vertex, $text, $alignment, $size)
        {
            $this->vertex = $vertex;
            $this->text = $text;
            $this->alignment = $alignment;
            $this->size = $size;
        }
    
        /**
         * Read PlacedText from input stream
         */
        public static function readFrom($stream)
        {
            $vertex = \Model\DebugInterface\ColoredVertex::readFrom($stream);
            $text = $stream->readString();
            $alignment = $stream->readFloat32();
            $size = $stream->readFloat32();
            return new PlacedText($vertex, $text, $alignment, $size);
        }
    
        /**
         * Write PlacedText to output stream
         */
        public function writeTo($stream)
        {
            $stream->writeInt32(PlacedText::TAG);
            $this->vertex->writeTo($stream);
            $stream->writeString($this->text);
            $stream->writeFloat32($this->alignment);
            $stream->writeFloat32($this->size);
        }
    }
}