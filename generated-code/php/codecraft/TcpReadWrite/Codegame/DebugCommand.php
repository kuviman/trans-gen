<?php

namespace Codegame {
    require_once 'Model/DebugInterface/DebugData.php';
    require_once 'Stream.php';

    /**
     * Debug commands that can be sent while debugging with the app
     */
    abstract class DebugCommand
    {
        /**
         * Write DebugCommand to output stream
         */
        abstract function writeTo(\OutputStream $stream): void;

        /**
         * Read DebugCommand from input stream
         */
        static function readFrom(\InputStream $stream): DebugCommand
        {
            $tag = $stream->readInt32();
            if ($tag == \Codegame\DebugCommand\Add::TAG) {
                return \Codegame\DebugCommand\Add::readFrom($stream);
            }
            if ($tag == \Codegame\DebugCommand\Clear::TAG) {
                return \Codegame\DebugCommand\Clear::readFrom($stream);
            }
            if ($tag == \Codegame\DebugCommand\SetAutoFlush::TAG) {
                return \Codegame\DebugCommand\SetAutoFlush::readFrom($stream);
            }
            if ($tag == \Codegame\DebugCommand\Flush::TAG) {
                return \Codegame\DebugCommand\Flush::readFrom($stream);
            }
            throw new Exception('Unexpected tag value');
        }
    }
}

namespace Codegame\DebugCommand {
    /**
     * Add debug data to current tick
     */
    class Add extends \Codegame\DebugCommand
    {
        const TAG = 0;
    
        /**
         * Data to add
         */
        public \Model\DebugInterface\DebugData $debugData;
    
        function __construct(\Model\DebugInterface\DebugData $debugData)
        {
            $this->debugData = $debugData;
        }
    
        /**
         * Read Add from input stream
         */
        public static function readFrom(\InputStream $stream): Add
        {
            $debugData = \Model\DebugInterface\DebugData::readFrom($stream);
            return new Add($debugData);
        }
        
        /**
         * Write Add to output stream
         */
        public function writeTo(\OutputStream $stream): void
        {
            $stream->writeInt32(Add::TAG);
            $this->debugData->writeTo($stream);
        }
    }

    /**
     * Clear current tick's debug data
     */
    class Clear extends \Codegame\DebugCommand
    {
        const TAG = 1;
    
    
        function __construct()
        {
        }
    
        /**
         * Read Clear from input stream
         */
        public static function readFrom(\InputStream $stream): Clear
        {
            return new Clear();
        }
        
        /**
         * Write Clear to output stream
         */
        public function writeTo(\OutputStream $stream): void
        {
            $stream->writeInt32(Clear::TAG);
        }
    }

    /**
     * Enable/disable auto performing of commands
     */
    class SetAutoFlush extends \Codegame\DebugCommand
    {
        const TAG = 2;
    
        /**
         * Enable/disable autoflush
         */
        public bool $enable;
    
        function __construct(bool $enable)
        {
            $this->enable = $enable;
        }
    
        /**
         * Read SetAutoFlush from input stream
         */
        public static function readFrom(\InputStream $stream): SetAutoFlush
        {
            $enable = $stream->readBool();
            return new SetAutoFlush($enable);
        }
        
        /**
         * Write SetAutoFlush to output stream
         */
        public function writeTo(\OutputStream $stream): void
        {
            $stream->writeInt32(SetAutoFlush::TAG);
            $stream->writeBool($this->enable);
        }
    }

    /**
     * Perform all previously sent commands
     */
    class Flush extends \Codegame\DebugCommand
    {
        const TAG = 3;
    
    
        function __construct()
        {
        }
    
        /**
         * Read Flush from input stream
         */
        public static function readFrom(\InputStream $stream): Flush
        {
            return new Flush();
        }
        
        /**
         * Write Flush to output stream
         */
        public function writeTo(\OutputStream $stream): void
        {
            $stream->writeInt32(Flush::TAG);
        }
    }
}