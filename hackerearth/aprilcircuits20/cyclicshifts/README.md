# Cyclic shifts

You are given a number `N` represented as a binary representation of `X = 16`
bits. You are also given a number `m` and a character `c (L or R)`.

Determine a number `M` that is generated after cyclically shifting the binary
representation of `N` by `m` positions either left if `c = L` or right is `c
= R`.

## Input format

The first line contains an integer `T` representing the number of queries. The
next `T` lines contain `N m c` as mentioned in the problem statement.

## Output format

Print `T` integers in a separate line representing the answer to each query.

## Constraints
1 ≤ T ≤ 1e4
1 ≤ N ≤ 65535
1 ≤ m ≤ 15

|--------------|---------------|
| SAMPLE INPUT | SAMPLE OUTPUT |
|--------------|---------------|
| 2            | 55587         |
| 7881 5 L     | 9177          |
| 7881 3 R     |               |
|--------------|---------------|

Explanation For first case : `N` in binary is `0001 1110 1100 1001` and
shifting it left by `5` position, it becomes `1101 1001 0010 0011` which in
decimal system is `55587`

For second case : `N` in binary is `0001 1110 1100 1001` and shifted `3`
position to right it becomes `0010 0011 1101 1001` which in decimal system is
`9177`

|--------------------|------------------------------------------------------|
| Time Limit:        | 1.0 sec(s) for each input file.                      |
| Memory Limit:      | 256 MB                                               |
| Source Limit:      | 1024 KB                                              |
| Marking Scheme:    | Marks are awarded if any testcase passes.            |
| Allowed Languages: | Bash, C, C++, C++14, Clojure, C#, D, Erlang, F#, Go, |
|                    | Groovy, Haskell, Java, Java 8,                       |
|                    | JavaScript(Rhino), JavaScript(Node.js),              |
|                    | Julia, Kotlin, Lisp, Lisp (SBCL), Lua,               |
|                    | Objective-C, OCaml, Octave, Pascal, Perl, PHP,       |
|                    | Python, Python 3, Racket, Ruby, Rust, Scala,         |
|                    | Swift, Swift-4.1, TypeScript, Visual Basic           |
|--------------------|------------------------------------------------------|
