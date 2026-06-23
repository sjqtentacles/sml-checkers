(* examples/demo.sml — sml-checkers demonstration *)
val () =
  let
    val s0 = Checkers.startPos
    val () = print "=== English Draughts (Checkers) ===\n"
    val () = print ("Start position:\n" ^ Checkers.toString s0)
    val () = print ("Legal moves: " ^ Int.toString (List.length (Checkers.legalMoves s0)) ^ "\n")

    (* Play first move *)
    val mv = hd (Checkers.legalMoves s0)
    val s1 = Checkers.makeMove s0 mv
    val () = print ("\nAfter Black move (sq " ^ Int.toString (Checkers.moveFrom mv) ^
                    " -> " ^ Int.toString (Checkers.moveTo mv) ^ "):\n")
    val () = print (Checkers.toString s1)

    val () = print ("Best move at depth 3: ")
    val () = case Checkers.bestMove 3 s0 of
               NONE => print "none\n"
             | SOME m => print (Int.toString (Checkers.moveFrom m) ^
                                " -> " ^ Int.toString (Checkers.moveTo m) ^ "\n")
  in () end
