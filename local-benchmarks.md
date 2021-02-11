# Local benchmarks

May be outdated.

Also see benchmarks done on GitHub Actions on [the testing branch](https://github.com/kuviman/trans-gen/tree/testing)

## Linux i7 7700HQ

| Language | codecraft::FileReadWrite | codecraft::TcpReadWrite | example::FileReadWrite | example::TcpReadWrite |
| - | --- | --- | --- | --- |
| C# | 714 us | 50 ms | 459 us | 567 us |
| C++ | 223 us | 3 ms | 26 us | 135 us |
| D | 678 us | 19 ms | 30 us | 362 us |
| F# | 1 ms | 50 ms | 581 us | 397 us |
| Go | 939 us | 3 ms | 431 us | 217 us |
| Java | 1 ms | 5 ms | 974 us | 174 us |
| JavaScript | 21 ms | 39 ms | 1 ms | 744 us |
| Kotlin | 2 ms | 3 ms | 1 ms | 203 us |
| PHP | 4 ms | 22 ms | 196 us | 404 us |
| Python | 3 ms | 7 ms | 229 us | 405 us |
| Ruby | 8 ms | 7 ms | 501 us | 194 us |
| Rust | 130 us | 3 ms | 16 us | 128 us |
| Scala | 3 ms | 5 ms | 2 ms | 566 us |
| Swift | 836 us | 20 ms | 57 us | 381 us |
| TypeScript | 68 ms | 103 ms | 1 ms | 1 ms |

## Windows Ryzen 3600

| Language | codecraft::FileReadWrite | codecraft::TcpReadWrite | example::FileReadWrite | example::TcpReadWrite |
| - | --- | --- | --- | --- |
| C# | 1 ms | 5 ms | 830 us | 153 us |
| C++ | 1 ms | 5 ms | 717 us | 74 us |
| D | 12 ms | 38 ms | 2 ms | 269 us |
| F# | 1 ms | 5 ms | 1 ms | 334 us |
| Go | 2 ms | 5 ms | 1 ms | 77 us |
| Java | 1 ms | 6 ms | 1 ms | 280 us |
| JavaScript | 37 ms | 64 ms | 1 ms | 900 us |
| Kotlin | 2 ms | 5 ms | 1 ms | 440 us |
| PHP | 18 ms | 49 ms | 1 ms | 331 us |
| Python | 5 ms | 12 ms | 927 us | 188 us |
| Ruby | 35 ms | 23 ms | 1 ms | 15 ms |
| Rust | 909 us | 5 ms | 775 us | 75 us |
| Scala | 3 ms | 6 ms | 2 ms | 911 us |
| TypeScript | 90 ms | 129 ms | 1 ms | 1 ms |