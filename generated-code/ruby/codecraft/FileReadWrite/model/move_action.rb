require './vec2_int'

module Model

# Move action
class MoveAction
    # Target position
    attr_accessor :target
    # Whether to try find closest position, if path to target is not found
    attr_accessor :find_closest_position
    # Whether to destroy other entities on the way
    attr_accessor :break_through

    def initialize(target, find_closest_position, break_through)
        @target = target
        @find_closest_position = find_closest_position
        @break_through = break_through
    end

    # Read MoveAction from input stream
    def self.read_from(stream)
        target = Vec2Int.read_from(stream)
        find_closest_position = stream.read_bool()
        break_through = stream.read_bool()
        MoveAction.new(target, find_closest_position, break_through)
    end

    # Write MoveAction to output stream
    def write_to(stream)
        @target.write_to(stream)
        stream.write_bool(@find_closest_position)
        stream.write_bool(@break_through)
    end

    def to_s
        string_result = "MoveAction { "
        string_result += "target: "
        string_result += @target.to_s
        string_result += ", "
        string_result += "find_closest_position: "
        string_result += @find_closest_position.to_s
        string_result += ", "
        string_result += "break_through: "
        string_result += @break_through.to_s
        string_result += " }"
        string_result
    end

    def to_str
        to_s
    end
end

end