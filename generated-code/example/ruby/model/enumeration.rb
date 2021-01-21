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
end