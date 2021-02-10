require './model/attack_action'
require './model/build_action'
require './model/move_action'
require './model/repair_action'

module Model

# Entity's action
class EntityAction
    # Move action
    attr_accessor :move_action
    # Build action
    attr_accessor :build_action
    # Attack action
    attr_accessor :attack_action
    # Repair action
    attr_accessor :repair_action

    def initialize(move_action, build_action, attack_action, repair_action)
        @move_action = move_action
        @build_action = build_action
        @attack_action = attack_action
        @repair_action = repair_action
    end

    # Read EntityAction from input stream
    def self.read_from(stream)
        if stream.read_bool()
            move_action = Model::MoveAction.read_from(stream)
        else
            move_action = nil
        end
        if stream.read_bool()
            build_action = Model::BuildAction.read_from(stream)
        else
            build_action = nil
        end
        if stream.read_bool()
            attack_action = Model::AttackAction.read_from(stream)
        else
            attack_action = nil
        end
        if stream.read_bool()
            repair_action = Model::RepairAction.read_from(stream)
        else
            repair_action = nil
        end
        EntityAction.new(move_action, build_action, attack_action, repair_action)
    end

    # Write EntityAction to output stream
    def write_to(stream)
        if @move_action.nil?
            stream.write_bool(false)
        else
            stream.write_bool(true)
            @move_action.write_to(stream)
        end
        if @build_action.nil?
            stream.write_bool(false)
        else
            stream.write_bool(true)
            @build_action.write_to(stream)
        end
        if @attack_action.nil?
            stream.write_bool(false)
        else
            stream.write_bool(true)
            @attack_action.write_to(stream)
        end
        if @repair_action.nil?
            stream.write_bool(false)
        else
            stream.write_bool(true)
            @repair_action.write_to(stream)
        end
    end

    def to_s
        string_result = "EntityAction { "
        string_result += "move_action: "
        if @move_action.nil?
            string_result += "nil"
        else
            string_result += @move_action.to_s
        end
        string_result += ", "
        string_result += "build_action: "
        if @build_action.nil?
            string_result += "nil"
        else
            string_result += @build_action.to_s
        end
        string_result += ", "
        string_result += "attack_action: "
        if @attack_action.nil?
            string_result += "nil"
        else
            string_result += @attack_action.to_s
        end
        string_result += ", "
        string_result += "repair_action: "
        if @repair_action.nil?
            string_result += "nil"
        else
            string_result += @repair_action.to_s
        end
        string_result += " }"
        string_result
    end

    def to_str
        to_s
    end
end

end