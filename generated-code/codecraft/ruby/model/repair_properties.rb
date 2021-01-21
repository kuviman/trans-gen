require_relative 'entity_type'

class RepairProperties
    attr_accessor :valid_targets
    attr_accessor :power

    def initialize(valid_targets, power)
        @valid_targets = valid_targets
        @power = power
    end

    def self.read_from(stream)
        valid_targets = []
        stream.read_int().times do |_|
            valid_targets_element = EntityType.read_from(stream)
            valid_targets.push(valid_targets_element)
        end
        power = stream.read_int()
        RepairProperties.new(valid_targets, power)
    end

    def write_to(stream)
        stream.write_int(@valid_targets.length())
        @valid_targets.each do |valid_targets_element|
            stream.write_int(valid_targets_element)
        end
        stream.write_int(@power)
    end
end