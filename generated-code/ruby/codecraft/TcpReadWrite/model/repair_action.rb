module Model

# Repair action
class RepairAction
    # Target entity's ID
    attr_accessor :target

    def initialize(target)
        @target = target
    end

    # Read RepairAction from input stream
    def self.read_from(stream)
        target = stream.read_int()
        RepairAction.new(target)
    end

    # Write RepairAction to output stream
    def write_to(stream)
        stream.write_int(@target)
    end

    def to_s
        string_result = "RepairAction { "
        string_result += "target: "
        string_result += @target.to_s
        string_result += " }"
        string_result
    end

    def to_str
        to_s
    end
end

end