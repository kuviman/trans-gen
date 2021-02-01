# 2 dimensional vector.
class Vec2Int
    # `x` coordinate of the vector
    attr_accessor :x
    # `y` coordinate of the vector
    attr_accessor :y

    def initialize(x, y)
        @x = x
        @y = y
    end

    # Read Vec2Int from input stream
    def self.read_from(stream)
        x = stream.read_int()
        y = stream.read_int()
        Vec2Int.new(x, y)
    end

    # Write Vec2Int to output stream
    def write_to(stream)
        stream.write_int(@x)
        stream.write_int(@y)
    end

    def to_s
        string_result = "Vec2Int { "
        string_result += "x: "
        string_result += @x.to_s
        string_result += ", "
        string_result += "y: "
        string_result += @y.to_s
        string_result += " }"
        string_result
    end

    def to_str
        to_s
    end
end