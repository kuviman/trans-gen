from color import Color
from stream_wrapper import StreamWrapper
from typing import Optional
from vec2_float import Vec2Float

class ColoredVertex:
    """Vertex for debug rendering"""

    __slots__ = ("world_pos","screen_offset","color",)

    world_pos: Optional[Vec2Float]
    screen_offset: Vec2Float
    color: Color

    def __init__(self, world_pos: Optional[Vec2Float], screen_offset: Vec2Float, color: Color):
        self.world_pos = world_pos
        """Position in world coordinates (if none, screen position (0, 0) is used)"""
        self.screen_offset = screen_offset
        """Additional offset in screen coordinates"""
        self.color = color
        """Color to use"""

    @staticmethod
    def read_from(stream: StreamWrapper) -> "ColoredVertex":
        """Read ColoredVertex from input stream
        """
        if stream.read_bool():
            world_pos = Vec2Float.read_from(stream)
        else:
            world_pos = None
        screen_offset = Vec2Float.read_from(stream)
        color = Color.read_from(stream)
        return ColoredVertex(world_pos, screen_offset, color)
    
    def write_to(self, stream: StreamWrapper):
        """Write ColoredVertex to output stream
        """
        if self.world_pos is None:
            stream.write_bool(False)
        else:
            stream.write_bool(True)
            self.world_pos.write_to(stream)
        self.screen_offset.write_to(stream)
        self.color.write_to(stream)
    
    def __repr__(self):
        return "ColoredVertex(" + \
            repr(self.world_pos) + \
            ", " + \
            repr(self.screen_offset) + \
            ", " + \
            repr(self.color) + \
            ")"