<?php

require_once 'Model.php';
require_once 'TcpStream.php';

if (count($argv) != 3) {
    throw new Exception('Pass host and as parameters');
}

$host = $argv[1];
$port = intval($argv[2]);

$tcpStream = new TcpStream($host, $port);

$input = PlayerView::readFrom($tcpStream->inputStream);
print_r($input);
$input->writeTo($tcpStream->outputStream);