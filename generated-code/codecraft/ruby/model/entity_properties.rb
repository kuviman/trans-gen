require_relative 'attack_properties'
require_relative 'build_properties'
require_relative 'repair_properties'

class EntityProperties
    attr_accessor :size
    attr_accessor :build_score
    attr_accessor :destroy_score
    attr_accessor :can_move
    attr_accessor :population_provide
    attr_accessor :population_use
    attr_accessor :max_health
    attr_accessor :initial_cost
    attr_accessor :sight_range
    attr_accessor :resource_per_health
    attr_accessor :build
    attr_accessor :attack
    attr_accessor :repair

    def initialize(size, build_score, destroy_score, can_move, population_provide, population_use, max_health, initial_cost, sight_range, resource_per_health, build, attack, repair)
        @size = size
        @build_score = build_score
        @destroy_score = destroy_score
        @can_move = can_move
        @population_provide = population_provide
        @population_use = population_use
        @max_health = max_health
        @initial_cost = initial_cost
        @sight_range = sight_range
        @resource_per_health = resource_per_health
        @build = build
        @attack = attack
        @repair = repair
    end

    def self.read_from(stream)
        size = stream.read_int()
        build_score = stream.read_int()
        destroy_score = stream.read_int()
        can_move = stream.read_bool()
        population_provide = stream.read_int()
        population_use = stream.read_int()
        max_health = stream.read_int()
        initial_cost = stream.read_int()
        sight_range = stream.read_int()
        resource_per_health = stream.read_int()
        if stream.read_bool()
            build = BuildProperties.read_from(stream)
        else
            build = nil
        end
        if stream.read_bool()
            attack = AttackProperties.read_from(stream)
        else
            attack = nil
        end
        if stream.read_bool()
            repair = RepairProperties.read_from(stream)
        else
            repair = nil
        end
        EntityProperties.new(size, build_score, destroy_score, can_move, population_provide, population_use, max_health, initial_cost, sight_range, resource_per_health, build, attack, repair)
    end

    def write_to(stream)
        stream.write_int(@size)
        stream.write_int(@build_score)
        stream.write_int(@destroy_score)
        stream.write_bool(@can_move)
        stream.write_int(@population_provide)
        stream.write_int(@population_use)
        stream.write_int(@max_health)
        stream.write_int(@initial_cost)
        stream.write_int(@sight_range)
        stream.write_int(@resource_per_health)
        if @build.nil?
            stream.write_bool(false)
        else
            stream.write_bool(true)
            @build.write_to(stream)
        end
        if @attack.nil?
            stream.write_bool(false)
        else
            stream.write_bool(true)
            @attack.write_to(stream)
        end
        if @repair.nil?
            stream.write_bool(false)
        else
            stream.write_bool(true)
            @repair.write_to(stream)
        end
    end

    def to_s
        string_result = "EntityProperties { "
        string_result += "size: "
        string_result += @size.to_s
        string_result += ", "
        string_result += "build_score: "
        string_result += @build_score.to_s
        string_result += ", "
        string_result += "destroy_score: "
        string_result += @destroy_score.to_s
        string_result += ", "
        string_result += "can_move: "
        string_result += @can_move.to_s
        string_result += ", "
        string_result += "population_provide: "
        string_result += @population_provide.to_s
        string_result += ", "
        string_result += "population_use: "
        string_result += @population_use.to_s
        string_result += ", "
        string_result += "max_health: "
        string_result += @max_health.to_s
        string_result += ", "
        string_result += "initial_cost: "
        string_result += @initial_cost.to_s
        string_result += ", "
        string_result += "sight_range: "
        string_result += @sight_range.to_s
        string_result += ", "
        string_result += "resource_per_health: "
        string_result += @resource_per_health.to_s
        string_result += ", "
        string_result += "build: "
        if @build.nil?
            string_result += "nil"
        else
            string_result += @build.to_s
        end
        string_result += ", "
        string_result += "attack: "
        if @attack.nil?
            string_result += "nil"
        else
            string_result += @attack.to_s
        end
        string_result += ", "
        string_result += "repair: "
        if @repair.nil?
            string_result += "nil"
        else
            string_result += @repair.to_s
        end
        string_result += " }"
        string_result
    end

    def to_str
        to_s
    end
end