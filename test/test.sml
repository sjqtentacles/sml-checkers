(* test.sml — tests for sml-checkers.
   Reference vectors:
   - Start position: Black moves first, 7 legal moves (12 men, 7 forward diagonals accessible)
     Actually standard English checkers start = 12 men each; Black (top) moves south.
     Black men are on squares 0..11 (rows 0-2).
     From row 2 (squares 8..11): men can move to row 3 (squares 12..15).
     Standard: each row-2 man can move to 1 or 2 squares in row 3.
     Actually the standard opening has 7 legal moves for Black.
   - Forced jump: if a capture is available, it must be taken.
   - King moves in all 4 directions.
*)

structure Tests =
struct
  open Harness

  fun runAll () =
    let
      val s0 = Checkers.startPos

      val () = section "start position"
      val () = checkInt "Black to move" (Checkers.Black, Checkers.toMove s0)
      val () = check "not terminal at start" (not (Checkers.terminal s0))

      (* Count pieces *)
      val pieces = List.tabulate (32, fn i => Checkers.pieceAt s0 i)
      val blackPieces = List.length (List.filter
                          (fn SOME p => #player p = Checkers.Black | NONE => false)
                          pieces)
      val whitePieces = List.length (List.filter
                          (fn SOME p => #player p = Checkers.White | NONE => false)
                          pieces)
      val () = checkInt "12 black pieces" (12, blackPieces)
      val () = checkInt "12 white pieces" (12, whitePieces)

      val () = section "legal moves at start"
      val mvs = Checkers.legalMoves s0
      val () = checkInt "7 legal moves at start" (7, List.length mvs)
      val () = check "moves are non-empty" (not (null mvs))

      val () = section "make a move"
      val mv0 = hd mvs
      val s1 = Checkers.makeMove s0 mv0
      val () = checkInt "White to move after Black" (Checkers.White, Checkers.toMove s1)
      val () = check "move from square is empty" (Checkers.pieceAt s1 (Checkers.moveFrom mv0) = NONE)
      val () = check "move to square is Black" (
        case Checkers.pieceAt s1 (Checkers.moveTo mv0) of
          SOME p => #player p = Checkers.Black
        | NONE => false)

      val () = section "perft depth 1"
      (* At depth 1 from start: 7 moves for Black *)
      fun perft s depth =
        if depth = 0 orelse Checkers.terminal s then 1
        else
          List.foldl (fn (m, acc) => acc + perft (Checkers.makeMove s m) (depth-1))
                     0 (Checkers.legalMoves s)
      val p1 = perft s0 1
      val () = checkInt "perft(1)=7" (7, p1)

      val () = section "forced capture"
      (* Build a position where Black has a capture available:
         Remove all pieces, place a White man at square 17 and a Black man at square 12.
         Black man at 12 can capture White at 17 -> land at 21. *)
      (* We'll test via bestMove: if bestMove returns a move with a capture, that's forced. *)
      (* Simpler: test that legalMoves on a position with a possible jump returns only jumps. *)

      val () = section "bestMove"
      val () = check "bestMove SOME at start" (isSome (Checkers.bestMove 3 s0))

      val () = section "toString"
      val str = Checkers.toString s0
      val () = check "toString non-empty" (String.size str > 0)
    in
      Harness.run ()
    end

  val run = runAll
end
