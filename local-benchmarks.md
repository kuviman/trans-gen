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
| C# | 527 us | 5 ms | 269 us | 59 us |
| C++ | 928 us | 5 ms | 424 us | 45 us |
| D | 1 ms | 5 ms | 386 us | 65 us |
| F# | 582 us | 5 ms | 290 us | 77 us |
| Go | 862 us | 5 ms | 339 us | 51 us |
| Haskell | 6 ms | 5 ms | 1 ms | 61 us |
| Java | 821 us | 5 ms | 355 us | 89 us |
| JavaScript | 37 ms | 10 ms | 647 us | 204 us |
| Kotlin | 713 us | 5 ms | 347 us | 106 us |
| PHP | 5 ms | 8 ms | 224 us | 78 us |
| Python | 4 ms | 12 ms | 372 us | 147 us |
| Ruby | 36 ms | 27 ms | 959 us | 15 ms |
| Rust | 568 us | 5 ms | 439 us | 44 us |
| Scala | 969 us | 5 ms | 497 us | 152 us |
| TypeScript | 37 ms | 9 ms | 645 us | 195 us |