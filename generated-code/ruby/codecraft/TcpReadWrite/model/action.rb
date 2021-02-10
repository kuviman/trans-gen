require './model/entity_action'

module Model

# Player's action
class Action
    # New actions for entities. If entity does not get new action, if will continue to perform previously set one
    attr_accessor :entity_actions

    def initialize(entity_actions)
        @entity_actions = entity_actions
    end

    # Read Action from input stream
    def self.read_from(stream)
        entity_actions = Hash.new
        stream.read_int().times do |_|
            entity_actions_key = stream.read_int()
            entity_actions_value = Model::EntityAction.read_from(stream)
            entity_actions[entity_actions_key] = entity_actions_value
        end
        Action.new(entity_actions)
    end

    # Write Action to output stream
    def write_to(stream)
        stream.write_int(@entity_actions.length())
        @entity_actions.each do |entity_actions_key, entity_actions_value|
            stream.write_int(entity_actions_key)
            entity_actions_value.write_to(stream)
        end
    end

    def to_s
        string_result = "Action { "
        string_result += "entity_actions: "
        string_result += "{ "
        entity_actions_index = 0
        @entity_actions.each do |entity_actions_key, entity_actions_value|
            if entity_actions_index != 0
                string_result += ", "
            end
            string_result += entity_actions_key.to_s
            string_result += " => "
            string_result += entity_actions_value.to_s
            entity_actions_index += 1
        end
        string_result += " }"
        string_result += " }"
        string_result
    end

    def to_str
        to_s
    end
end

end