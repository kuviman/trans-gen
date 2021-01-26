#if os(Linux)
	import Glibc
#else
	import Darwin
#endif

class FileInputStream : InputStream {
    init(_ path: String) {
		#if os(Linux)
			file = Glibc.fopen(path, "rb")
		#else
			file = Darwin.fopen(path, "rb")
		#endif
    }

	func readBytes(_ byteCount: Int) -> [Byte] {
		var buffer = [Byte](repeating: 0x0, count: byteCount)
		#if os(Linux)
			Glibc.fread(&buffer, 1, byteCount, file)
		#else
			Darwin.fread(&buffer, 1, byteCount, file)
		#endif
		return buffer
    }

	deinit {
		#if os(Linux)
			Glibc.fclose(file)
		#else
			Darwin.fclose(file)
		#endif
	}

	#if os(Linux)
		private let file: UnsafeMutablePointer<Glibc.FILE>
	#else
		private let file: UnsafeMutablePointer<Darwin.FILE>
	#endif
};

class FileOutputStream : OutputStream {
    init(_ path: String) {
		#if os(Linux)
			file = Glibc.fopen(path, "wb")
		#else
			file = Darwin.fopen(path, "wb")
		#endif
    }

	func writeBytes(_ data: [Byte]) {
		#if os(Linux)
			Glibc.fwrite(data, 1, data.count, file)
		#else
			Darwin.fwrite(data, 1, data.count, file)
		#endif
    }

    func flush() {
		#if os(Linux)
			Glibc.fflush(file)
		#else
			Darwin.fflush(file)
		#endif
    }

	deinit {
		#if os(Linux)
			Glibc.fclose(file)
		#else
			Darwin.fclose(file)
		#endif
	}

	#if os(Linux)
		private let file: UnsafeMutablePointer<Glibc.FILE>
	#else
		private let file: UnsafeMutablePointer<Darwin.FILE>
	#endif
};

if CommandLine.arguments.count != 3 {
	fatalError("Pass input and output as parameters")
}

let inputFile = CommandLine.arguments[1];
let inputStream = FileInputStream(inputFile)
let input = Example.readFrom(inputStream)

print(input)

let outputFile = CommandLine.arguments[2];
let outputStream = FileOutputStream(outputFile)
input.writeTo(outputStream)
outputStream.flush()