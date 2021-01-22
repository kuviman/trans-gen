require_relative 'entity'
require_relative 'entity_properties'
require_relative 'entity_type'
require_relative 'player'

class PlayerView
    attr_accessor :my_id
    attr_accessor :map_size
    attr_accessor :fog_of_war
    attr_accessor :entity_properties
    attr_accessor :max_tick_count
    attr_accessor :max_pathfind_nodes
    attr_accessor :current_tick
    attr_accessor :players
    attr_accessor :entities

    def initialize(my_id, map_size, fog_of_war, entity_properties, max_tick_count, max_pathfind_nodes, current_tick, players, entities)
        @my_id = my_id
        @map_size = map_size
        @fog_of_war = fog_of_war
        @entity_properties = entity_properties
        @max_tick_count = max_tick_count
        @max_pathfind_nodes = max_pathfind_nodes
        @current_tick = current_tick
        @players = players
        @entities = entities
    end

    def self.read_from(stream)
        my_id = stream.read_int()
        map_size = stream.read_int()
        fog_of_war = stream.read_bool()
        entity_properties = Hash.new
        stream.read_int().times do |_|
            entity_properties_key = EntityType.read_from(stream)
            entity_properties_value = EntityProperties.read_from(stream)
            entity_properties[entity_properties_key] = entity_properties_value
        end
        max_tick_count = stream.read_int()
        max_pathfind_nodes = stream.read_int()
        current_tick = stream.read_int()
        players = []
        stream.read_int().times do |_|
            players_element = Player.read_from(stream)
            players.push(players_element)
        end
        entities = []
        stream.read_int().times do |_|
            entities_element = Entity.read_from(stream)
            entities.push(entities_element)
        end
        PlayerView.new(my_id, map_size, fog_of_war, entity_properties, max_tick_count, max_pathfind_nodes, current_tick, players, entities)
    end

    def write_to(stream)
        stream.write_int(@my_id)
        stream.write_int(@map_size)
        stream.write_bool(@fog_of_war)
        stream.write_int(@entity_properties.length())
        @entity_properties.each do |entity_properties_key, entity_properties_value|
            stream.write_int(entity_properties_key)
            entity_properties_value.write_to(stream)
        end
        stream.write_int(@max_tick_count)
        stream.write_int(@max_pathfind_nodes)
        stream.write_int(@current_tick)
        stream.write_int(@players.length())
        @players.each do |players_element|
            players_element.write_to(stream)
        end
        stream.write_int(@entities.length())
        @entities.each do |entities_element|
            entities_element.write_to(stream)
        end
    end

    def to_s
        string_result = "PlayerView { "
        string_result += "my_id: "
        string_result += @my_id.to_s
        string_result += ", "
        string_result += "map_size: "
        string_result += @map_size.to_s
        string_result += ", "
        string_result += "fog_of_war: "
        string_result += @fog_of_war.to_s
        string_result += ", "
        string_result += "entity_properties: "
        string_result += "{ "
        entity_properties_index = 0
        @entity_properties.each do |entity_properties_key, entity_properties_value|
            if entity_properties_index != 0
                string_result += ", "
            end
            string_result += EntityType.to_s(entity_properties_key)
            string_result += " => "
            string_result += entity_properties_value.to_s
            entity_properties_index += 1
        end
        string_result += " }"
        string_result += ", "
        string_result += "max_tick_count: "
        string_result += @max_tick_count.to_s
        string_result += ", "
        string_result += "max_pathfind_nodes: "
        string_result += @max_pathfind_nodes.to_s
        string_result += ", "
        string_result += "current_tick: "
        string_result += @current_tick.to_s
        string_result += ", "
        string_result += "players: "
        string_result += "[ "
        players_index = 0
        @players.each do |players_element|
            if players_index != 0
                string_result += ", "
            end
            string_result += players_element.to_s
            players_index += 1
        end
        string_result += " ]"
        string_result += ", "
        string_result += "entities: "
        string_result += "[ "
        entities_index = 0
        @entities.each do |entities_element|
            if entities_index != 0
                string_result += ", "
            end
            string_result += entities_element.to_s
            entities_index += 1
        end
        string_result += " ]"
        string_result += " }"
        string_result
    end

    def to_str
        to_s
    end
end