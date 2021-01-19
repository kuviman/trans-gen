require 'stringio'
require_relative 'stream_wrapper'
require_relative 'model'

class FileWrapper
    def initialize(file)
        @file = file
    end

    def read_bytes(byte_count)
        @file.read(byte_count)
    end

    def write_bytes(data)
        @file.write(data)
    end

    def flush
        @file.flush
    end
end

if ARGV.length != 2
    raise "Pass input and output as parameters"
end
input_file = ARGV[0]
output_file = ARGV[1]

input = nil
open(input_file, "rb") { |file|
    stream = StreamWrapper.new(FileWrapper.new(file))
    input = Structure.read_from(stream)
}

open(output_file, "wb") { |file|
    stream = StreamWrapper.new(FileWrapper.new(file))
    input.write_to(stream)
    stream.flush
}