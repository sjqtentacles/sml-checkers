(* checkers.sig

   English Draughts (American Checkers) on an 8×8 board.

   Rules:
   - Pieces move and capture on dark squares only (32 playable squares).
   - Men move/capture diagonally forward; kings move/capture in all diagonals.
   - Captures are mandatory; multi-jumps are mandatory (continue in same turn).
   - A man reaching the last row promotes to a king.
   - Side to move loses if they have no legal moves.

   Square numbering: 1..32, standard international notation
   (row 0=top=Black side, square 1 top-left dark, row-major).
   Internally squares 0..31 (0-based) = standard squares 1..32.
*)

signature CHECKERS =
sig
  val Black : int    (* = 1 *)
  val White : int    (* = 2 *)

  type piece = { player : int, king : bool }
  type move                (* abstract; use accessors *)

  type state

  val startPos    : state

  (* Piece access *)
  val pieceAt     : state -> int -> piece option   (* square 0..31 *)
  val toMove      : state -> int

  (* Move generation *)
  val legalMoves  : state -> move list

  (* Apply a move *)
  val makeMove    : state -> move -> state

  (* Terminal: the side to move has no legal moves *)
  val terminal    : state -> bool

  (* Move accessors *)
  val moveFrom    : move -> int        (* source square 0..31 *)
  val moveTo      : move -> int        (* final destination square 0..31 *)
  val moveCaptures : move -> int list  (* captured squares (may be empty) *)

  (* Search *)
  val bestMove    : int -> state -> move option   (* depth -> best move *)

  (* Display *)
  val toString    : state -> string
end
