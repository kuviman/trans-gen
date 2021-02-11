<?php

namespace Codegame {
    require_once 'Codegame/DebugCommand.php';
    require_once 'Model/Action.php';
    require_once 'Stream.php';

    /**
     * Message sent from client
     */
    abstract class ClientMessage
    {
        /**
         * Write ClientMessage to output stream
         */
        abstract function writeTo(\OutputStream $stream): void;

        /**
         * Read ClientMessage from input stream
         */
        static function readFrom(\InputStream $stream): ClientMessage
        {
            $tag = $stream->readInt32();
            if ($tag == \Codegame\ClientMessage\DebugMessage::TAG) {
                return \Codegame\ClientMessage\DebugMessage::readFrom($stream);
            }
            if ($tag == \Codegame\ClientMessage\ActionMessage::TAG) {
                return \Codegame\ClientMessage\ActionMessage::readFrom($stream);
            }
            if ($tag == \Codegame\ClientMessage\DebugUpdateDone::TAG) {
                return \Codegame\ClientMessage\DebugUpdateDone::readFrom($stream);
            }
            if ($tag == \Codegame\ClientMessage\RequestDebugState::TAG) {
                return \Codegame\ClientMessage\RequestDebugState::readFrom($stream);
            }
            throw new Exception('Unexpected tag value');
        }
    }
}

namespace Codegame\ClientMessage {
    /**
     * Ask app to perform new debug command
     */
    class DebugMessage extends \Codegame\ClientMessage
    {
        const TAG = 0;
    
        /**
         * Command to perform
         */
        public \Codegame\DebugCommand $command;
    
        function __construct(\Codegame\DebugCommand $command)
        {
            $this->command = $command;
        }
    
        /**
         * Read DebugMessage from input stream
         */
        public static function readFrom(\InputStream $stream): DebugMessage
        {
            $command = \Codegame\DebugCommand::readFrom($stream);
            return new DebugMessage($command);
        }
        
        /**
         * Write DebugMessage to output stream
         */
        public function writeTo(\OutputStream $stream): void
        {
            $stream->writeInt32(DebugMessage::TAG);
            $this->command->writeTo($stream);
        }
    }

    /**
     * Reply for ServerMessage::GetAction
     */
    class ActionMessage extends \Codegame\ClientMessage
    {
        const TAG = 1;
    
        /**
         * Player's action
         */
        public \Model\Action $action;
    
        function __construct(\Model\Action $action)
        {
            $this->action = $action;
        }
    
        /**
         * Read ActionMessage from input stream
         */
        public static function readFrom(\InputStream $stream): ActionMessage
        {
            $action = \Model\Action::readFrom($stream);
            return new ActionMessage($action);
        }
        
        /**
         * Write ActionMessage to output stream
         */
        public function writeTo(\OutputStream $stream): void
        {
            $stream->writeInt32(ActionMessage::TAG);
            $this->action->writeTo($stream);
        }
    }

    /**
     * Signifies finish of the debug update
     */
    class DebugUpdateDone extends \Codegame\ClientMessage
    {
        const TAG = 2;
    
    
        function __construct()
        {
        }
    
        /**
         * Read DebugUpdateDone from input stream
         */
        public static function readFrom(\InputStream $stream): DebugUpdateDone
        {
            return new DebugUpdateDone();
        }
        
        /**
         * Write DebugUpdateDone to output stream
         */
        public function writeTo(\OutputStream $stream): void
        {
            $stream->writeInt32(DebugUpdateDone::TAG);
        }
    }

    /**
     * Request debug state from the app
     */
    class RequestDebugState extends \Codegame\ClientMessage
    {
        const TAG = 3;
    
    
        function __construct()
        {
        }
    
        /**
         * Read RequestDebugState from input stream
         */
        public static function readFrom(\InputStream $stream): RequestDebugState
        {
            return new RequestDebugState();
        }
        
        /**
         * Write RequestDebugState to output stream
         */
        public function writeTo(\OutputStream $stream): void
        {
            $stream->writeInt32(RequestDebugState::TAG);
        }
    }
}