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
    
        attr_accessor :vec_int
        attr_accessor :long_int
    
        def initialize(vec_int, long_int)
            @vec_int = vec_int
            @long_int = long_int
        end
    
        def self.read_from(stream)
            vec_int = []
            stream.read_int().times do |_|
                vec_int_element = stream.read_int()
                vec_int.push(vec_int_element)
            end
            long_int = stream.read_long()
            OptionOne.new(vec_int, long_int)
        end
    
        def write_to(stream)
            stream.write_int(TAG)
            stream.write_int(@vec_int.length())
            @vec_int.each do |vec_int_element|
                stream.write_int(vec_int_element)
            end
            stream.write_long(@long_int)
        end
    
        def to_s
            string_result = "OptionOne { "
            string_result += "vec_int: "
            string_result += "[ "
            vec_int_index = 0
            @vec_int.each do |vec_int_element|
                if vec_int_index != 0
                    string_result += ", "
                end
                string_result += vec_int_element.to_s
                vec_int_index += 1
            end
            string_result += " ]"
            string_result += ", "
            string_result += "long_int: "
            string_result += @long_int.to_s
            string_result += " }"
            string_result
        end
    
        def to_str
            to_s
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
    
        def to_s
            string_result = "OptionTwo { "
            string_result += "value: "
            string_result += @value.to_s
            string_result += " }"
            string_result
        end
    
        def to_str
            to_s
        end
    end
end