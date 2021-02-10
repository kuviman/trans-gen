<?php

namespace Codegame {
    
    require_once 'Codegame/ClientMessage.php';
    require_once 'Codegame/ServerMessage.php';

    /**
     * Client or server message
     */
    abstract class MessageGameModel
    {
        /**
         * Write MessageGameModel to output stream
         */
        abstract function writeTo($stream);

        /**
         * Read MessageGameModel from input stream
         */
        static function readFrom($stream)
        {
            $tag = $stream->readInt32();
            if ($tag == \Codegame\MessageGameModel\Client::TAG) {
                return \Codegame\MessageGameModel\Client::readFrom($stream);
            }
            if ($tag == \Codegame\MessageGameModel\Server::TAG) {
                return \Codegame\MessageGameModel\Server::readFrom($stream);
            }
            throw new Exception('Unexpected tag value');
        }
    }
}

namespace Codegame\MessageGameModel {
    /**
     * Client message
     */
    class Client extends \Codegame\MessageGameModel
    {
        const TAG = 0;
    
        /**
         * Message
         */
        public $message;
    
        function __construct($message)
        {
            $this->message = $message;
        }
    
        /**
         * Read Client from input stream
         */
        public static function readFrom($stream)
        {
            $message = \Codegame\ClientMessage::readFrom($stream);
            return new Client($message);
        }
        
        /**
         * Write Client to output stream
         */
        public function writeTo($stream)
        {
            $stream->writeInt32(Client::TAG);
            $this->message->writeTo($stream);
        }
    }

    /**
     * Server message
     */
    class Server extends \Codegame\MessageGameModel
    {
        const TAG = 1;
    
        /**
         * Message
         */
        public $message;
    
        function __construct($message)
        {
            $this->message = $message;
        }
    
        /**
         * Read Server from input stream
         */
        public static function readFrom($stream)
        {
            $message = \Codegame\ServerMessage::readFrom($stream);
            return new Server($message);
        }
        
        /**
         * Write Server to output stream
         */
        public function writeTo($stream)
        {
            $stream->writeInt32(Server::TAG);
            $this->message->writeTo($stream);
        }
    }
}