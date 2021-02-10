# RGBA Color
class Color
    # Red component
    attr_accessor :r
    # Green component
    attr_accessor :g
    # Blue component
    attr_accessor :b
    # Alpha (opacity) component
    attr_accessor :a

    def initialize(r, g, b, a)
        @r = r
        @g = g
        @b = b
        @a = a
    end

    # Read Color from input stream
    def self.read_from(stream)
        r = stream.read_float()
        g = stream.read_float()
        b = stream.read_float()
        a = stream.read_float()
        Color.new(r, g, b, a)
    end

    # Write Color to output stream
    def write_to(stream)
        stream.write_float(@r)
        stream.write_float(@g)
        stream.write_float(@b)
        stream.write_float(@a)
    end

    def to_s
        string_result = "Color { "
        string_result += "r: "
        string_result += @r.to_s
        string_result += ", "
        string_result += "g: "
        string_result += @g.to_s
        string_result += ", "
        string_result += "b: "
        string_result += @b.to_s
        string_result += ", "
        string_result += "a: "
        string_result += @a.to_s
        string_result += " }"
        string_result
    end

    def to_str
        to_s
    end
end