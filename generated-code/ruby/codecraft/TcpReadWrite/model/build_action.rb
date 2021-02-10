require './model/entity_type'
require './vec2_int'

module Model

# Build action
class BuildAction
    # Type of an entity to build
    attr_accessor :entity_type
    # Desired position of new entity
    attr_accessor :position

    def initialize(entity_type, position)
        @entity_type = entity_type
        @position = position
    end

    # Read BuildAction from input stream
    def self.read_from(stream)
        entity_type = Model::EntityType.read_from(stream)
        position = Vec2Int.read_from(stream)
        BuildAction.new(entity_type, position)
    end

    # Write BuildAction to output stream
    def write_to(stream)
        stream.write_int(@entity_type)
        @position.write_to(stream)
    end

    def to_s
        string_result = "BuildAction { "
        string_result += "entity_type: "
        string_result += EntityType.to_s(@entity_type)
        string_result += ", "
        string_result += "position: "
        string_result += @position.to_s
        string_result += " }"
        string_result
    end

    def to_str
        to_s
    end
end

end