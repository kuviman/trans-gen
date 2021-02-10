require './color'
require './vec2_float'

module Model
module DebugInterface

# Vertex for debug rendering
class ColoredVertex
    # Position in world coordinates (if none, screen position (0, 0) is used)
    attr_accessor :world_pos
    # Additional offset in screen coordinates
    attr_accessor :screen_offset
    # Color to use
    attr_accessor :color

    def initialize(world_pos, screen_offset, color)
        @world_pos = world_pos
        @screen_offset = screen_offset
        @color = color
    end

    # Read ColoredVertex from input stream
    def self.read_from(stream)
        if stream.read_bool()
            world_pos = Vec2Float.read_from(stream)
        else
            world_pos = nil
        end
        screen_offset = Vec2Float.read_from(stream)
        color = Color.read_from(stream)
        ColoredVertex.new(world_pos, screen_offset, color)
    end

    # Write ColoredVertex to output stream
    def write_to(stream)
        if @world_pos.nil?
            stream.write_bool(false)
        else
            stream.write_bool(true)
            @world_pos.write_to(stream)
        end
        @screen_offset.write_to(stream)
        @color.write_to(stream)
    end

    def to_s
        string_result = "ColoredVertex { "
        string_result += "world_pos: "
        if @world_pos.nil?
            string_result += "nil"
        else
            string_result += @world_pos.to_s
        end
        string_result += ", "
        string_result += "screen_offset: "
        string_result += @screen_offset.to_s
        string_result += ", "
        string_result += "color: "
        string_result += @color.to_s
        string_result += " }"
        string_result
    end

    def to_str
        to_s
    end
end

end
end