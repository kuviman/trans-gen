<?php

require_once 'Example.php';
require_once 'Stream.php';
require_once 'BufferedStream.php';

class FileInputStream extends InputStream
{
    private $stream;
    function __construct(string $path)
    {
        $this->stream = fopen($path, "rb");
    }
    function __destruct()
    {
        fclose($this->stream);
    }
    public function readAtMost(int $byteCount): string
    {
        return fread($this->stream, $byteCount);
    }
}

class FileOutputStream extends OutputStream
{
    private $stream;
    function __construct(string $path)
    {
        $this->stream = fopen($path, "wb");
    }
    function __destruct()
    {
        $this->flush();
        fclose($this->stream);
    }
    public function write(string $bytes): void
    {
        fwrite($this->stream, $bytes);
    }
    public function flush(): void
    {
        fflush($this->stream);
    }
}

$inputFile = $argv[1];
$outputFile = $argv[2];
$repeat = intval($argv[3]);

for ($i = 0; $i < $repeat; $i++) {
    $input = \Example::readFrom(new BufferedInputStream(new FileInputStream($inputFile)));
    if ($repeat == 1) {
        print_r($input);
    }
    $outputStream = new BufferedOutputStream(new FileOutputStream($outputFile));
    $input->writeTo($outputStream);
    $outputStream->flush();
}