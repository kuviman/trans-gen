module Model

# Entity's attack properties
class AttackProperties
    # Maximum attack range
    attr_accessor :attack_range
    # Damage dealt in one tick
    attr_accessor :damage
    # If true, dealing damage will collect resource from target
    attr_accessor :collect_resource

    def initialize(attack_range, damage, collect_resource)
        @attack_range = attack_range
        @damage = damage
        @collect_resource = collect_resource
    end

    # Read AttackProperties from input stream
    def self.read_from(stream)
        attack_range = stream.read_int()
        damage = stream.read_int()
        collect_resource = stream.read_bool()
        AttackProperties.new(attack_range, damage, collect_resource)
    end

    # Write AttackProperties to output stream
    def write_to(stream)
        stream.write_int(@attack_range)
        stream.write_int(@damage)
        stream.write_bool(@collect_resource)
    end

    def to_s
        string_result = "AttackProperties { "
        string_result += "attack_range: "
        string_result += @attack_range.to_s
        string_result += ", "
        string_result += "damage: "
        string_result += @damage.to_s
        string_result += ", "
        string_result += "collect_resource: "
        string_result += @collect_resource.to_s
        string_result += " }"
        string_result
    end

    def to_str
        to_s
    end
end

end