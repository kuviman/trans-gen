<?php

require_once 'Enumeration.php';
require_once 'OneOf.php';
require_once 'Structure.php';

/**
 * Example
 */
class Example
{
    /**
     * OneOf
     */
    public $oneOf;
    /**
     * Dictionary
     */
    public $hashMap;
    /**
     * Optional int
     */
    public $optionalInt;
    /**
     * Optional boolean
     */
    public $optionalBool;
    /**
     * Optional OneOf
     */
    public $optionalOneOf;
    /**
     * Optional struct
     */
    public $optionalStruct;
    /**
     * Optional enum
     */
    public $optionalEnum;

    function __construct($oneOf, $hashMap, $optionalInt, $optionalBool, $optionalOneOf, $optionalStruct, $optionalEnum)
    {
        $this->oneOf = $oneOf;
        $this->hashMap = $hashMap;
        $this->optionalInt = $optionalInt;
        $this->optionalBool = $optionalBool;
        $this->optionalOneOf = $optionalOneOf;
        $this->optionalStruct = $optionalStruct;
        $this->optionalEnum = $optionalEnum;
    }

    /**
     * Read Example from input stream
     */
    public static function readFrom($stream)
    {
        $oneOf = OneOf::readFrom($stream);
        $hashMap = [];
        $hashMapSize = $stream->readInt32();
        for ($hashMapIndex = 0; $hashMapIndex < $hashMapSize; $hashMapIndex++) {
            $hashMapKey = Enumeration::readFrom($stream);
            $hashMapValue = $stream->readInt32();
            $hashMap[$hashMapKey] = $hashMapValue;
        }
        if ($stream->readBool()) {
            $optionalInt = $stream->readInt32();
        } else {
            $optionalInt = NULL;
        }
        if ($stream->readBool()) {
            $optionalBool = $stream->readBool();
        } else {
            $optionalBool = NULL;
        }
        if ($stream->readBool()) {
            $optionalOneOf = OneOf::readFrom($stream);
        } else {
            $optionalOneOf = NULL;
        }
        if ($stream->readBool()) {
            $optionalStruct = Structure::readFrom($stream);
        } else {
            $optionalStruct = NULL;
        }
        if ($stream->readBool()) {
            $optionalEnum = Enumeration::readFrom($stream);
        } else {
            $optionalEnum = NULL;
        }
        return new Example($oneOf, $hashMap, $optionalInt, $optionalBool, $optionalOneOf, $optionalStruct, $optionalEnum);
    }

    /**
     * Write Example to output stream
     */
    public function writeTo($stream)
    {
        $this->oneOf->writeTo($stream);
        $stream->writeInt32(count($this->hashMap));
        foreach ($this->hashMap as $key => $value) {
            $stream->writeInt32($key);
            $stream->writeInt32($value);
        }
        if (is_null($this->optionalInt)) {
            $stream->writeBool(false);
        } else {
            $stream->writeBool(true);
            $stream->writeInt32($this->optionalInt);
        }
        if (is_null($this->optionalBool)) {
            $stream->writeBool(false);
        } else {
            $stream->writeBool(true);
            $stream->writeBool($this->optionalBool);
        }
        if (is_null($this->optionalOneOf)) {
            $stream->writeBool(false);
        } else {
            $stream->writeBool(true);
            $this->optionalOneOf->writeTo($stream);
        }
        if (is_null($this->optionalStruct)) {
            $stream->writeBool(false);
        } else {
            $stream->writeBool(true);
            $this->optionalStruct->writeTo($stream);
        }
        if (is_null($this->optionalEnum)) {
            $stream->writeBool(false);
        } else {
            $stream->writeBool(true);
            $stream->writeInt32($this->optionalEnum);
        }
    }
}