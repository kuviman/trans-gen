<?php

namespace Model {
    require_once 'Model/EntityType.php';
    require_once 'Stream.php';

    /**
     * Entity's build properties
     */
    class BuildProperties
    {
        /**
         * Valid new entity types
         */
        public array $options;
        /**
         * Initial health of new entity. If absent, it will have full health
         */
        public ?int $initHealth;
    
        function __construct(array $options, ?int $initHealth)
        {
            $this->options = $options;
            $this->initHealth = $initHealth;
        }
    
        /**
         * Read BuildProperties from input stream
         */
        public static function readFrom(\InputStream $stream): BuildProperties
        {
            $options = [];
            $optionsSize = $stream->readInt32();
            for ($optionsIndex = 0; $optionsIndex < $optionsSize; $optionsIndex++) {
                $optionsElement = \Model\EntityType::readFrom($stream);
                $options[] = $optionsElement;
            }
            if ($stream->readBool()) {
                $initHealth = $stream->readInt32();
            } else {
                $initHealth = NULL;
            }
            return new BuildProperties($options, $initHealth);
        }
        
        /**
         * Write BuildProperties to output stream
         */
        public function writeTo(\OutputStream $stream): void
        {
            $stream->writeInt32(count($this->options));
            foreach ($this->options as $element) {
                $stream->writeInt32($element);
            }
            if (is_null($this->initHealth)) {
                $stream->writeBool(false);
            } else {
                $stream->writeBool(true);
                $stream->writeInt32($this->initHealth);
            }
        }
    }
}