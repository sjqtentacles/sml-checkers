# sml-checkers

[![CI](https://github.com/sjqtentacles/sml-checkers/actions/workflows/ci.yml/badge.svg)](https://github.com/sjqtentacles/sml-checkers/actions/workflows/ci.yml)

Pure Standard ML **English Draughts** (American Checkers): legal-move generation with mandatory captures and multi-jumps, king promotion, and adversarial search via the vendored `sml-gametree` engine.

No FFI, no threads, no clock. Byte-identical under **MLton** and **Poly/ML**.

## Running `make example` prints:

```
=== English Draughts (Checkers) ===
Start position:
.b.b.b.b
b.b.b.b.
.b.b.b.b
........
........
w.w.w.w.
.w.w.w.w
w.w.w.w.
Legal moves: 7

After Black move (sq 8 -> 13):
.b.b.b.b
b.b.b.b.
...b.b.b
..b.....
........
w.w.w.w.
.w.w.w.w
w.w.w.w.
Best move at depth 3: 8 -> 13
```

## API

```sml
val startPos   : state
val legalMoves : state -> move list       (* captures mandatory *)
val makeMove   : state -> move -> state
val terminal   : state -> bool            (* no legal moves *)
val pieceAt    : state -> int -> piece option
val toMove     : state -> int             (* Black or White *)
val bestMove   : int -> state -> move option
val toString   : state -> string
val moveFrom   : move -> int
val moveTo     : move -> int
val moveCaptures : move -> int list
val Black : int   val White : int
```

## Build & test

```sh
make test && make test-poly
make example
```

## Tests

**12 deterministic checks**: start position piece counts, 7 legal moves, perft(1)=7, move application, bestMove, toString.

## License

MIT. See [LICENSE](LICENSE).
