from model.attack_action import AttackAction
from model.build_action import BuildAction
from model.move_action import MoveAction
from model.repair_action import RepairAction
from stream_wrapper import StreamWrapper
from typing import Optional

class EntityAction:
    """Entity's action"""

    __slots__ = ("move_action","build_action","attack_action","repair_action",)

    move_action: Optional[MoveAction]
    build_action: Optional[BuildAction]
    attack_action: Optional[AttackAction]
    repair_action: Optional[RepairAction]

    def __init__(self, move_action: Optional[MoveAction], build_action: Optional[BuildAction], attack_action: Optional[AttackAction], repair_action: Optional[RepairAction]):
        self.move_action = move_action
        """Move action"""
        self.build_action = build_action
        """Build action"""
        self.attack_action = attack_action
        """Attack action"""
        self.repair_action = repair_action
        """Repair action"""

    @staticmethod
    def read_from(stream: StreamWrapper) -> "EntityAction":
        """Read EntityAction from input stream
        """
        if stream.read_bool():
            move_action = MoveAction.read_from(stream)
        else:
            move_action = None
        if stream.read_bool():
            build_action = BuildAction.read_from(stream)
        else:
            build_action = None
        if stream.read_bool():
            attack_action = AttackAction.read_from(stream)
        else:
            attack_action = None
        if stream.read_bool():
            repair_action = RepairAction.read_from(stream)
        else:
            repair_action = None
        return EntityAction(move_action, build_action, attack_action, repair_action)
    
    def write_to(self, stream: StreamWrapper):
        """Write EntityAction to output stream
        """
        if self.move_action is None:
            stream.write_bool(False)
        else:
            stream.write_bool(True)
            self.move_action.write_to(stream)
        if self.build_action is None:
            stream.write_bool(False)
        else:
            stream.write_bool(True)
            self.build_action.write_to(stream)
        if self.attack_action is None:
            stream.write_bool(False)
        else:
            stream.write_bool(True)
            self.attack_action.write_to(stream)
        if self.repair_action is None:
            stream.write_bool(False)
        else:
            stream.write_bool(True)
            self.repair_action.write_to(stream)
    
    def __repr__(self):
        return "EntityAction(" + \
            repr(self.move_action) + \
            ", " + \
            repr(self.build_action) + \
            ", " + \
            repr(self.attack_action) + \
            ", " + \
            repr(self.repair_action) + \
            ")"