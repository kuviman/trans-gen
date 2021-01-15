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
        attr_accessor :value
        def initialize(value)
            @value = value
        end
        def self.read_from(stream)
            value = []
            stream.read_int().times do |_|
                value_element = stream.read_int()
                value.push(value_element)
            end
            OptionOne.new(value)
        end
        def write_to(stream)
            stream.write_int(TAG)
            stream.write_int(@value.length())
            @value.each do |element|
                stream.write_int(element)
            end
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
