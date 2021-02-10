require './model/debug_interface/colored_vertex'
require './model/debug_interface/primitive_type'

module Model
module DebugInterface

# Debug data can be drawn in the app
class DebugData
    # Read DebugData from input stream
    def self.read_from(stream)
        tag = stream.read_int()
        if tag == DebugData::Log::TAG
            return DebugData::Log.read_from(stream)
        end
        if tag == DebugData::Primitives::TAG
            return DebugData::Primitives.read_from(stream)
        end
        if tag == DebugData::PlacedText::TAG
            return DebugData::PlacedText.read_from(stream)
        end
        raise "Unexpected tag value"
    end

    # Log some text
    class Log
        TAG = 0
    
        # Text to show
        attr_accessor :text
    
        def initialize(text)
            @text = text
        end
    
        # Read Log from input stream
        def self.read_from(stream)
            text = stream.read_string()
            Log.new(text)
        end
    
        # Write Log to output stream
        def write_to(stream)
            stream.write_int(TAG)
            stream.write_string(@text)
        end
    
        def to_s
            string_result = "Log { "
            string_result += "text: "
            string_result += @text.dump
            string_result += " }"
            string_result
        end
    
        def to_str
            to_s
        end
    end
    # Draw primitives
    class Primitives
        TAG = 1
    
        # Vertices
        attr_accessor :vertices
        # Primitive type
        attr_accessor :primitive_type
    
        def initialize(vertices, primitive_type)
            @vertices = vertices
            @primitive_type = primitive_type
        end
    
        # Read Primitives from input stream
        def self.read_from(stream)
            vertices = []
            stream.read_int().times do |_|
                vertices_element = Model::DebugInterface::ColoredVertex.read_from(stream)
                vertices.push(vertices_element)
            end
            primitive_type = Model::DebugInterface::PrimitiveType.read_from(stream)
            Primitives.new(vertices, primitive_type)
        end
    
        # Write Primitives to output stream
        def write_to(stream)
            stream.write_int(TAG)
            stream.write_int(@vertices.length())
            @vertices.each do |vertices_element|
                vertices_element.write_to(stream)
            end
            stream.write_int(@primitive_type)
        end
    
        def to_s
            string_result = "Primitives { "
            string_result += "vertices: "
            string_result += "[ "
            vertices_index = 0
            @vertices.each do |vertices_element|
                if vertices_index != 0
                    string_result += ", "
                end
                string_result += vertices_element.to_s
                vertices_index += 1
            end
            string_result += " ]"
            string_result += ", "
            string_result += "primitive_type: "
            string_result += PrimitiveType.to_s(@primitive_type)
            string_result += " }"
            string_result
        end
    
        def to_str
            to_s
        end
    end
    # Draw text
    class PlacedText
        TAG = 2
    
        # Vertex to determine text position and color
        attr_accessor :vertex
        # Text
        attr_accessor :text
        # Text alignment (0 means left, 0.5 means center, 1 means right)
        attr_accessor :alignment
        # Font size in pixels
        attr_accessor :size
    
        def initialize(vertex, text, alignment, size)
            @vertex = vertex
            @text = text
            @alignment = alignment
            @size = size
        end
    
        # Read PlacedText from input stream
        def self.read_from(stream)
            vertex = Model::DebugInterface::ColoredVertex.read_from(stream)
            text = stream.read_string()
            alignment = stream.read_float()
            size = stream.read_float()
            PlacedText.new(vertex, text, alignment, size)
        end
    
        # Write PlacedText to output stream
        def write_to(stream)
            stream.write_int(TAG)
            @vertex.write_to(stream)
            stream.write_string(@text)
            stream.write_float(@alignment)
            stream.write_float(@size)
        end
    
        def to_s
            string_result = "PlacedText { "
            string_result += "vertex: "
            string_result += @vertex.to_s
            string_result += ", "
            string_result += "text: "
            string_result += @text.dump
            string_result += ", "
            string_result += "alignment: "
            string_result += @alignment.to_s
            string_result += ", "
            string_result += "size: "
            string_result += @size.to_s
            string_result += " }"
            string_result
        end
    
        def to_str
            to_s
        end
    end
end

end
end