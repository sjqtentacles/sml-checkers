(* gametree.sig

   Signatures for the sml-gametree adversarial game-tree search library.
   The implementation (gametree.sml) provides functor GameTreeSearch.

   Usage:
     structure MySearch = GameTreeSearch(MyGame)
     val (score, mv) = MySearch.bestMove 6 initialState

   Algorithms (all depth-bounded, deterministic, no FFI/clock):
     negamax    — plain minimax; the correctness oracle
     alphaBeta  — negamax + alpha-beta pruning (same result, faster)
     pvs        — principal-variation search / NegaScout
     idab       — iterative-deepening alpha-beta
     bestMove   — IDAB convenience wrapper
*)

signature GAME =
sig
  type state
  type move

  val hash     : state -> int
  val moves    : state -> move list
  val apply    : state -> move -> state
  val terminal : state -> bool
  val eval     : state -> int
end

signature GAME_SEARCH =
sig
  type state
  type move

  val INF       : int

  val negamax   : int -> state -> int * move option
  val alphaBeta : int -> int -> int -> state -> int * move option
  val pvs       : int -> int -> int -> state -> int * move option
  val idab      : int -> state -> int * move option
  val bestMove  : int -> state -> move option
end
