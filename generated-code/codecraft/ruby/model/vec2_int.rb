class Vec2Int
    attr_accessor :x
    attr_accessor :y

    def initialize(x, y)
        @x = x
        @y = y
    end

    def self.read_from(stream)
        x = stream.read_int()
        y = stream.read_int()
        Vec2Int.new(x, y)
    end

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