# Local benchmarks

May be outdated.

Also see benchmarks done on GitHub Actions on [the testing branch](https://github.com/kuviman/trans-gen/tree/testing)

## Linux i7 7700HQ

| Language | codecraft::FileReadWrite | codecraft::TcpReadWrite | example::FileReadWrite | example::TcpReadWrite |
| - | --- | --- | --- | --- |
| C# | 274 us | 53 ms | 69 us | 49 us |
| C++ | 194 us | 3 ms | 12 us | 91 us |
| D | 988 us | 4 ms | 68 us | 92 us |
| F# | 301 us | 53 ms | 86 us | 66 us |
| Go | 540 us | 3 ms | 158 us | 64 us |
| Haskell | 544 us | 4 ms | 61 us | 191 us |
| Java | 839 us | 3 ms | 348 us | 67 us |
| JavaScript | 24 ms | 9 ms | 344 us | 195 us |
| Kotlin | 839 us | 3 ms | 339 us | 120 us |
| PHP | 3 ms | 5 ms | 44 us | 119 us |
| Python | 3 ms | 7 ms | 86 us | 115 us |
| Ruby | 8 ms | 7 ms | 113 us | 95 us |
| Rust | 104 us | 3 ms | 9 us | 87 us |
| Scala | 1 ms | 3 ms | 502 us | 165 us |
| Swift | 1 ms | 4 ms | 28 us | 129 us |
| TypeScript | 23 ms | 8 ms | 327 us | 197 us |

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