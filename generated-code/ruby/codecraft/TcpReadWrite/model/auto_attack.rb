require './model/entity_type'

module Model

# Auto attack options
class AutoAttack
    # Maximum distance to pathfind
    attr_accessor :pathfind_range
    # List of target entity types to try to attack. If empty, all types but resource are considered
    attr_accessor :valid_targets

    def initialize(pathfind_range, valid_targets)
        @pathfind_range = pathfind_range
        @valid_targets = valid_targets
    end

    # Read AutoAttack from input stream
    def self.read_from(stream)
        pathfind_range = stream.read_int()
        valid_targets = []
        stream.read_int().times do |_|
            valid_targets_element = Model::EntityType.read_from(stream)
            valid_targets.push(valid_targets_element)
        end
        AutoAttack.new(pathfind_range, valid_targets)
    end

    # Write AutoAttack to output stream
    def write_to(stream)
        stream.write_int(@pathfind_range)
        stream.write_int(@valid_targets.length())
        @valid_targets.each do |valid_targets_element|
            stream.write_int(valid_targets_element)
        end
    end

    def to_s
        string_result = "AutoAttack { "
        string_result += "pathfind_range: "
        string_result += @pathfind_range.to_s
        string_result += ", "
        string_result += "valid_targets: "
        string_result += "[ "
        valid_targets_index = 0
        @valid_targets.each do |valid_targets_element|
            if valid_targets_index != 0
                string_result += ", "
            end
            string_result += EntityType.to_s(valid_targets_element)
            valid_targets_index += 1
        end
        string_result += " ]"
        string_result += " }"
        string_result
    end

    def to_str
        to_s
    end
end

end