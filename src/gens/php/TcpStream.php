<?php

require_once 'Stream.php';

class TcpStream
{
    public $inputStream;
    public $outputStream;
    function __construct($host, $port)
    {
        $socket = socket_create(AF_INET, SOCK_STREAM, SOL_TCP);
        if (!socket_set_option($socket, SOL_TCP, TCP_NODELAY, 1)) {
            throw new Exception("Failed to set TCP_NODELAY");
        }
        if (!socket_connect($socket, $host, $port)) {
            throw new Exception("Failed to connect");
        }
        $this->inputStream = new TcpInputStream($socket);
        $this->outputStream = new TcpOutputStream($socket);
    }
}

class TcpInputStream extends InputStream
{
    private $socket;
    function __construct($socket)
    {
        $this->socket = $socket;
    }
    public function read($byteCount)
    {
        return socket_read($this->socket, $byteCount);
    }
}

class TcpOutputStream extends OutputStream
{
    private $socket;
    function __construct($socket)
    {
        $this->socket = $socket;
    }
    public function write($bytes)
    {
        socket_write($this->socket, $bytes);
    }
    public function flush()
    {
    }
}
