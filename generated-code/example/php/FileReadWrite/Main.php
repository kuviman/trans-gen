<?php

require_once 'Model.php';
require_once 'Stream.php';

if (count($argv) != 3) {
    throw new Exception('Pass input and output as parameters');
}

class FileInputStream extends InputStream
{
    private $stream;
    function __construct($path)
    {
        $this->stream = fopen($path, "rb");
    }
    function __destruct()
    {
        fclose($this->stream);
    }
    public function read($byteCount)
    {
        $data = fread($this->stream, $byteCount);
        assert(strlen($data) == $byteCount);
        return $data;
    }
}

class FileOutputStream extends OutputStream
{
    private $stream;
    function __construct($path)
    {
        $this->stream = fopen($path, "wb");
    }
    function __destruct()
    {
        $this->flush();
        fclose($this->stream);
    }
    public function write($bytes)
    {
        fwrite($this->stream, $bytes);
    }
    public function flush()
    {
        fflush($this->stream);
    }
}

$inputFile = $argv[1];
$outputFile = $argv[2];

$input = Example::readFrom(new FileInputStream($inputFile));
print_r($input);
$input->writeTo(new FileOutputStream($outputFile));