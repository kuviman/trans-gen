require 'stringio'
require_relative 'stream'
require_relative 'example'

class FileStream < Stream
    def initialize(path, mode)
        @file = open(path, mode)
    end

    def close
        @file.close
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

input_file = ARGV[0]
output_file = ARGV[1]
repeat = ARGV[2].to_i

for i in 1..repeat
    input_stream = FileStream.new(input_file, "rb")
    input = Example.read_from(input_stream)
    input_stream.close
    if repeat == 1
        puts input
    end
    output_stream = FileStream.new(output_file, "wb")
    input.write_to(output_stream)
    output_stream.flush
    output_stream.close
end