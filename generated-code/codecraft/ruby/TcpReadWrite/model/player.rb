# Player (strategy, client)
class Player
    # Player's ID
    attr_accessor :id
    # Current score
    attr_accessor :score
    # Current amount of resource
    attr_accessor :resource

    def initialize(id, score, resource)
        @id = id
        @score = score
        @resource = resource
    end

    # Read Player from input stream
    def self.read_from(stream)
        id = stream.read_int()
        score = stream.read_int()
        resource = stream.read_int()
        Player.new(id, score, resource)
    end

    # Write Player to output stream
    def write_to(stream)
        stream.write_int(@id)
        stream.write_int(@score)
        stream.write_int(@resource)
    end

    def to_s
        string_result = "Player { "
        string_result += "id: "
        string_result += @id.to_s
        string_result += ", "
        string_result += "score: "
        string_result += @score.to_s
        string_result += ", "
        string_result += "resource: "
        string_result += @resource.to_s
        string_result += " }"
        string_result
    end

    def to_str
        to_s
    end
end