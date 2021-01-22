module Enumeration
    VALUE_ONE = 0
    VALUE_TWO = 1

    def self.read_from(stream)
        result = stream.read_int()
        if result < 0 || result >= 2
            raise "Unexpected tag value"
        end
        result
    end

    def self.to_s(value)
        if value == VALUE_ONE
            return "VALUE_ONE"
        end
        if value == VALUE_TWO
            return "VALUE_TWO"
        end
        raise "Impossible happened"
    end

    def self.to_str(value)
        self.to_s(value)
    end
end