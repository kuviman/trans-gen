#if os(Linux)
	import Glibc
#else
	import Darwin
#endif

class FileInputStream : InputStream {
    init(_ path: String) {
		file = fopen(path, "rb")
    }

	func readBytes(_ byteCount: Int) -> [Byte] {
		var buffer = [Byte](repeating: 0x0, count: byteCount)
		fread(&buffer, 1, byteCount, file)
		return buffer
    }

	deinit {
		fclose(file)
	}

	private let file: UnsafeMutablePointer<FILE>
}

class FileOutputStream : OutputStream {
    init(_ path: String) {
		file = fopen(path, "wb")
    }

	func writeBytes(_ data: [Byte]) {
		fwrite(data, 1, data.count, file)
    }

    func flush() {
		fflush(file)
    }

	deinit {
		fclose(file)
	}

	private let file: UnsafeMutablePointer<FILE>
}

if CommandLine.arguments.count != 3 {
	fatalError("Pass input and output as parameters")
}

let inputFile = CommandLine.arguments[1]
let inputStream = FileInputStream(inputFile)
let input = PlayerView.readFrom(inputStream)

print(input)

let outputFile = CommandLine.arguments[2]
let outputStream = FileOutputStream(outputFile)
input.writeTo(outputStream)
outputStream.flush()