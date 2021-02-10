<?php

require_once 'Codegame/MessageGameModel.php';
require_once 'TcpStream.php';

$host = $argv[1];
$port = intval($argv[2]);
$stdout = $argv[3] == "true";

$tcpStream = new TcpStream($host, $port);
while ($tcpStream->inputStream->readBool()) {
    $input = \Codegame\MessageGameModel::readFrom($tcpStream->inputStream);
    if ($stdout) {
        print_r($input);
    }
    $input->writeTo($tcpStream->outputStream);
    $tcpStream->outputStream->flush();
}