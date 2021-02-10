require './model/entity_type'

module Model

# Entity's build properties
class BuildProperties
    # Valid new entity types
    attr_accessor :options
    # Initial health of new entity. If absent, it will have full health
    attr_accessor :init_health

    def initialize(options, init_health)
        @options = options
        @init_health = init_health
    end

    # Read BuildProperties from input stream
    def self.read_from(stream)
        options = []
        stream.read_int().times do |_|
            options_element = Model::EntityType.read_from(stream)
            options.push(options_element)
        end
        if stream.read_bool()
            init_health = stream.read_int()
        else
            init_health = nil
        end
        BuildProperties.new(options, init_health)
    end

    # Write BuildProperties to output stream
    def write_to(stream)
        stream.write_int(@options.length())
        @options.each do |options_element|
            stream.write_int(options_element)
        end
        if @init_health.nil?
            stream.write_bool(false)
        else
            stream.write_bool(true)
            stream.write_int(@init_health)
        end
    end

    def to_s
        string_result = "BuildProperties { "
        string_result += "options: "
        string_result += "[ "
        options_index = 0
        @options.each do |options_element|
            if options_index != 0
                string_result += ", "
            end
            string_result += EntityType.to_s(options_element)
            options_index += 1
        end
        string_result += " ]"
        string_result += ", "
        string_result += "init_health: "
        if @init_health.nil?
            string_result += "nil"
        else
            string_result += @init_health.to_s
        end
        string_result += " }"
        string_result
    end

    def to_str
        to_s
    end
end

end