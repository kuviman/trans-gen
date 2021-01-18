require_relative 'enumeration'
require_relative 'one_of'

class Structure
    attr_accessor :one_of_one
    attr_accessor :one_of_two
    attr_accessor :hash_map
    attr_accessor :text
    attr_accessor :float_number
    attr_accessor :double_number

    def initialize(one_of_one, one_of_two, hash_map, text, float_number, double_number)
        @one_of_one = one_of_one
        @one_of_two = one_of_two
        @hash_map = hash_map
        @text = text
        @float_number = float_number
        @double_number = double_number
    end

    def self.read_from(stream)
        one_of_one = OneOf.read_from(stream)
        one_of_two = OneOf.read_from(stream)
        hash_map = Hash.new
        stream.read_int().times do |_|
            hash_map_key = stream.read_int()
            if hash_map_key < 0 || hash_map_key >= 2
                raise "Unexpected tag value"
            end
            hash_map_value = stream.read_int()
            hash_map[hash_map_key] = hash_map_value
        end
        text = stream.read_string()
        float_number = stream.read_float()
        double_number = stream.read_double()
        Structure.new(one_of_one, one_of_two, hash_map, text, float_number, double_number)
    end

    def write_to(stream)
        @one_of_one.write_to(stream)
        @one_of_two.write_to(stream)
        stream.write_int(@hash_map.length())
        @hash_map.each do |hash_map_key, hash_map_value|
            stream.write_int(hash_map_key)
            stream.write_int(hash_map_value)
        end
        stream.write_string(@text)
        stream.write_float(@float_number)
        stream.write_double(@double_number)
    end
end
