from model.auto_attack import AutoAttack
from stream_wrapper import StreamWrapper
from typing import Optional

class AttackAction:
    """Attack action"""

    __slots__ = ("target","auto_attack",)

    target: Optional[int]
    auto_attack: Optional[AutoAttack]

    def __init__(self, target: Optional[int], auto_attack: Optional[AutoAttack]):
        self.target = target
        """If specified, target entity's ID"""
        self.auto_attack = auto_attack
        """If specified, configures auto attacking"""

    @staticmethod
    def read_from(stream: StreamWrapper) -> "AttackAction":
        """Read AttackAction from input stream
        """
        if stream.read_bool():
            target = stream.read_int()
        else:
            target = None
        if stream.read_bool():
            auto_attack = AutoAttack.read_from(stream)
        else:
            auto_attack = None
        return AttackAction(target, auto_attack)
    
    def write_to(self, stream: StreamWrapper):
        """Write AttackAction to output stream
        """
        if self.target is None:
            stream.write_bool(False)
        else:
            stream.write_bool(True)
            stream.write_int(self.target)
        if self.auto_attack is None:
            stream.write_bool(False)
        else:
            stream.write_bool(True)
            self.auto_attack.write_to(stream)
    
    def __repr__(self):
        return "AttackAction(" + \
            repr(self.target) + \
            ", " + \
            repr(self.auto_attack) + \
            ")"