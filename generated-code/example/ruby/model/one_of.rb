class OneOf
    def self.read_from(stream)
        tag = stream.read_int()
        if tag == OneOf::OptionOne::TAG
            return OneOf::OptionOne.read_from(stream)
        end
        if tag == OneOf::OptionTwo::TAG
            return OneOf::OptionTwo.read_from(stream)
        end
        raise "Unexpected tag value"
    end

    class OptionOne
        TAG = 0
        attr_accessor :vec_i32
        attr_accessor :long_int
        def initialize(vec_i32, long_int)
            @vec_i32 = vec_i32
            @long_int = long_int
        end
        def self.read_from(stream)
            vec_i32 = []
            stream.read_int().times do |_|
                vec_i32_element = stream.read_int()
                vec_i32.push(vec_i32_element)
            end
            long_int = stream.read_long()
            OptionOne.new(vec_i32, long_int)
        end
        def write_to(stream)
            stream.write_int(TAG)
            stream.write_int(@vec_i32.length())
            @vec_i32.each do |element|
                stream.write_int(element)
            end
            stream.write_long(@long_int)
        end
    end
    class OptionTwo
        TAG = 1
        attr_accessor :value
        def initialize(value)
            @value = value
        end
        def self.read_from(stream)
            value = stream.read_int()
            OptionTwo.new(value)
        end
        def write_to(stream)
            stream.write_int(TAG)
            stream.write_int(@value)
        end
    end
end
