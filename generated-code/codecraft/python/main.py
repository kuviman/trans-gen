import model
from stream_wrapper import StreamWrapper
import sys


if __name__ == "__main__":
    if len(sys.argv) != 3:
        raise Exception("Pass input and output as parameters")

    input_file = sys.argv[1]
    output_file = sys.argv[2]

    input = model.PlayerView.read_from(StreamWrapper(open(input_file, 'rb')))
    input.write_to(StreamWrapper(open(output_file, 'wb')))