require './model/auto_attack'

module Model

# Attack action
class AttackAction
    # If specified, target entity's ID
    attr_accessor :target
    # If specified, configures auto attacking
    attr_accessor :auto_attack

    def initialize(target, auto_attack)
        @target = target
        @auto_attack = auto_attack
    end

    # Read AttackAction from input stream
    def self.read_from(stream)
        if stream.read_bool()
            target = stream.read_int()
        else
            target = nil
        end
        if stream.read_bool()
            auto_attack = Model::AutoAttack.read_from(stream)
        else
            auto_attack = nil
        end
        AttackAction.new(target, auto_attack)
    end

    # Write AttackAction to output stream
    def write_to(stream)
        if @target.nil?
            stream.write_bool(false)
        else
            stream.write_bool(true)
            stream.write_int(@target)
        end
        if @auto_attack.nil?
            stream.write_bool(false)
        else
            stream.write_bool(true)
            @auto_attack.write_to(stream)
        end
    end

    def to_s
        string_result = "AttackAction { "
        string_result += "target: "
        if @target.nil?
            string_result += "nil"
        else
            string_result += @target.to_s
        end
        string_result += ", "
        string_result += "auto_attack: "
        if @auto_attack.nil?
            string_result += "nil"
        else
            string_result += @auto_attack.to_s
        end
        string_result += " }"
        string_result
    end

    def to_str
        to_s
    end
end

end