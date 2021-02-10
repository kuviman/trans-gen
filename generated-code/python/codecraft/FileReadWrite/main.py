from codegame.message_game_model import MessageGameModel
from stream_wrapper import StreamWrapper
import sys


if __name__ == "__main__":
    input_file = sys.argv[1]
    output_file = sys.argv[2]
    repeat = int(sys.argv[3])

    for i in range(repeat):
        input = MessageGameModel.read_from(StreamWrapper(open(input_file, 'rb')))
        if repeat == 1:
            print(repr(input))
        input.write_to(StreamWrapper(open(output_file, 'wb')))