module Model
module DebugInterface

# Primitive type for debug rendering
module PrimitiveType
    # Lines, number of vertices should be divisible by 2
    LINES = 0
    # Triangles, number of vertices should be divisible by 3
    TRIANGLES = 1

    # Read PrimitiveType from input stream
    def self.read_from(stream)
        result = stream.read_int()
        if result < 0 || result >= 2
            raise "Unexpected tag value"
        end
        result
    end

    def self.to_s(value)
        if value == LINES
            return "LINES"
        end
        if value == TRIANGLES
            return "TRIANGLES"
        end
        raise "Impossible happened"
    end

    def self.to_str(value)
        self.to_s(value)
    end
end

end
end