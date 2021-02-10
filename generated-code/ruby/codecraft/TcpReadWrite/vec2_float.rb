# 2 dimensional vector.
class Vec2Float
    # `x` coordinate of the vector
    attr_accessor :x
    # `y` coordinate of the vector
    attr_accessor :y

    def initialize(x, y)
        @x = x
        @y = y
    end

    # Read Vec2Float from input stream
    def self.read_from(stream)
        x = stream.read_float()
        y = stream.read_float()
        Vec2Float.new(x, y)
    end

    # Write Vec2Float to output stream
    def write_to(stream)
        stream.write_float(@x)
        stream.write_float(@y)
    end

    def to_s
        string_result = "Vec2Float { "
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