from model.debug_interface.colored_vertex import ColoredVertex
from model.debug_interface.primitive_type import PrimitiveType
from stream_wrapper import StreamWrapper
from typing import List

class DebugData:
    """Debug data can be drawn in the app"""

    @staticmethod
    def read_from(stream: StreamWrapper) -> "DebugData":
        """Read DebugData from input stream
        """
        tag = stream.read_int()
        if tag == Log.TAG:
            return DebugData.Log.read_from(stream)
        if tag == Primitives.TAG:
            return DebugData.Primitives.read_from(stream)
        if tag == PlacedText.TAG:
            return DebugData.PlacedText.read_from(stream)
        raise Exception("Unexpected tag value")

class Log(DebugData):
    """Log some text"""

    TAG = 0

    __slots__ = ("text",)

    text: str

    def __init__(self, text: str):
        self.text = text
        """Text to show"""

    @staticmethod
    def read_from(stream: StreamWrapper) -> "Log":
        """Read Log from input stream
        """
        text = stream.read_string()
        return Log(text)
    
    def write_to(self, stream: StreamWrapper):
        """Write Log to output stream
        """
        stream.write_int(self.TAG)
        stream.write_string(self.text)
    
    def __repr__(self):
        return "Log(" + \
            repr(self.text) + \
            ")"

DebugData.Log = Log

class Primitives(DebugData):
    """Draw primitives"""

    TAG = 1

    __slots__ = ("vertices","primitive_type",)

    vertices: List[ColoredVertex]
    primitive_type: PrimitiveType

    def __init__(self, vertices: List[ColoredVertex], primitive_type: PrimitiveType):
        self.vertices = vertices
        """Vertices"""
        self.primitive_type = primitive_type
        """Primitive type"""

    @staticmethod
    def read_from(stream: StreamWrapper) -> "Primitives":
        """Read Primitives from input stream
        """
        vertices = []
        for _ in range(stream.read_int()):
            vertices_element = ColoredVertex.read_from(stream)
            vertices.append(vertices_element)
        primitive_type = PrimitiveType(stream.read_int())
        return Primitives(vertices, primitive_type)
    
    def write_to(self, stream: StreamWrapper):
        """Write Primitives to output stream
        """
        stream.write_int(self.TAG)
        stream.write_int(len(self.vertices))
        for element in self.vertices:
            element.write_to(stream)
        stream.write_int(self.primitive_type)
    
    def __repr__(self):
        return "Primitives(" + \
            repr(self.vertices) + \
            ", " + \
            repr(self.primitive_type) + \
            ")"

DebugData.Primitives = Primitives

class PlacedText(DebugData):
    """Draw text"""

    TAG = 2

    __slots__ = ("vertex","text","alignment","size",)

    vertex: ColoredVertex
    text: str
    alignment: float
    size: float

    def __init__(self, vertex: ColoredVertex, text: str, alignment: float, size: float):
        self.vertex = vertex
        """Vertex to determine text position and color"""
        self.text = text
        """Text"""
        self.alignment = alignment
        """Text alignment (0 means left, 0.5 means center, 1 means right)"""
        self.size = size
        """Font size in pixels"""

    @staticmethod
    def read_from(stream: StreamWrapper) -> "PlacedText":
        """Read PlacedText from input stream
        """
        vertex = ColoredVertex.read_from(stream)
        text = stream.read_string()
        alignment = stream.read_float()
        size = stream.read_float()
        return PlacedText(vertex, text, alignment, size)
    
    def write_to(self, stream: StreamWrapper):
        """Write PlacedText to output stream
        """
        stream.write_int(self.TAG)
        self.vertex.write_to(stream)
        stream.write_string(self.text)
        stream.write_float(self.alignment)
        stream.write_float(self.size)
    
    def __repr__(self):
        return "PlacedText(" + \
            repr(self.vertex) + \
            ", " + \
            repr(self.text) + \
            ", " + \
            repr(self.alignment) + \
            ", " + \
            repr(self.size) + \
            ")"

DebugData.PlacedText = PlacedText