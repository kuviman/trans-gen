<?php

require_once 'Stream.php';

define('BUFFER_SIZE', 102_400);

class BufferedInputStream extends InputStream
{
    private $inner;
    private $buffer;
    private $bufferPos;
    function __construct($inner)
    {
        $this->inner = $inner;
        $this->buffer = '';
        $this->bufferPos = 0;
    }
    function readAtMost($byteCount)
    {
        if ($this->bufferPos == strlen($this->buffer)) {
            $this->buffer = $this->inner->readAtMost(BUFFER_SIZE);
            $this->bufferPos = 0;
        }
        $chunk = substr($this->buffer, $this->bufferPos, $byteCount);
        $this->bufferPos += strlen($chunk);
        return $chunk;
    }
}

class BufferedOutputStream extends OutputStream
{
    private $inner;
    private $buffer;
    function __construct($inner)
    {
        $this->inner = $inner;
        $this->buffer = '';
    }
    function write($bytes)
    {
        $this->buffer .= $bytes;
    }
    function flush()
    {
        $this->inner->write($this->buffer);
        $this->inner->flush();
        $this->buffer = '';
    }
}
