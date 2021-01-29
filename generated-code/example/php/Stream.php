<?php

define('LITTLE_ENDIAN', pack('L', 1) === pack('V', 1));
assert(strlen(pack('g', 0.0)) == 4);
assert(strlen(pack('e', 0.0)) == 8);

abstract class InputStream
{
    abstract function read($byteBount);
    function readBool()
    {
        $byte = unpack('C', $this->read(1))[1];
        if ($byte == 0) {
            return false;
        } elseif ($byte == 1) {
            return true;
        } else {
            throw new Exception('bool should be 0 or 1');
        }
    }
    function readInt32()
    {
        $bytes = $this->read(4);
        if (!LITTLE_ENDIAN) {
            $bytes = strrev($bytes);
        }
        return unpack('l', $bytes)[1];
    }
    function readInt64()
    {
        $bytes = $this->read(8);
        if (!LITTLE_ENDIAN) {
            $bytes = strrev($bytes);
        }
        return unpack('q', $bytes)[1];
    }
    function readFloat32()
    {
        return unpack('g', $this->read(4))[1];
    }
    function readDouble()
    {
        return unpack('e', $this->read(8))[1];
    }
    function readString()
    {
        return $this->read($this->readInt32());
    }
}

abstract class OutputStream
{
    abstract function write($bytes);
    abstract function flush();
    function writeBool($value)
    {
        $this->write(pack('C', $value ? 1 : 0));
    }
    function writeInt32($value)
    {
        $bytes = pack('l', $value);
        if (!LITTLE_ENDIAN) {
            $bytes = strrev($bytes);
        }
        $this->write($bytes);
    }
    function writeInt64($value)
    {
        $bytes = pack('q', $value);
        if (!LITTLE_ENDIAN) {
            $bytes = strrev($bytes);
        }
        $this->write($bytes);
    }
    function writeFloat32($value)
    {
        $bytes = pack('g', $value);
        if (!LITTLE_ENDIAN) {
            $bytes = strrev($bytes);
        }
        $this->write($bytes);
    }
    function writeDouble($value)
    {
        $bytes = pack('e', $value);
        if (!LITTLE_ENDIAN) {
            $bytes = strrev($bytes);
        }
        $this->write($bytes);
    }
    function writeString($value)
    {
        $this->writeInt32(strlen($value));
        $this->write($value);
    }
}
