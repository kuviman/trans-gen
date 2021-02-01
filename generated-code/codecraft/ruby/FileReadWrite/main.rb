require 'stringio'
require_relative 'stream'
require_relative 'model'

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

if ARGV.length != 2
    raise "Pass input and output as parameters"
end
input_file = ARGV[0]
output_file = ARGV[1]

input_stream = FileStream.new(input_file, "rb")
input = PlayerView.read_from(input_stream)
input_stream.close

puts input

output_stream = FileStream.new(output_file, "wb")
input.write_to(output_stream)
output_stream.flush
output_stream.close