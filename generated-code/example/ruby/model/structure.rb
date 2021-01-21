class Structure
    attr_accessor :text
    attr_accessor :float_number
    attr_accessor :double_number

    def initialize(text, float_number, double_number)
        @text = text
        @float_number = float_number
        @double_number = double_number
    end

    def self.read_from(stream)
        text = stream.read_string()
        float_number = stream.read_float()
        double_number = stream.read_double()
        Structure.new(text, float_number, double_number)
    end

    def write_to(stream)
        stream.write_string(@text)
        stream.write_float(@float_number)
        stream.write_double(@double_number)
    end
end