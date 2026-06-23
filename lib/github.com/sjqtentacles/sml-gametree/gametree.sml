(* gametree.sml

   Pure Standard ML adversarial game-tree search.
   Functor GameTreeSearch (G : GAME) : GAME_SEARCH

   Algorithms
   ----------
   negamax    — plain minimax (no pruning); correctness oracle
   alphaBeta  — negamax + alpha-beta pruning
   pvs        — principal-variation search (NegaScout)
   idab       — iterative-deepening alpha-beta (uses alphaBeta)

   All depth-bounded, deterministic, no FFI/clock/randomness.
   Safe with MLton 32-bit int: INF = 10^6.
*)

functor GameTreeSearch (G : GAME) : GAME_SEARCH =
struct
  type state = G.state
  type move  = G.move

  val INF = 1000000
  val nINF = ~1000000

  (* ------------------------------------------------------------------ *)
  (* Plain negamax — the reference oracle.                               *)
  fun negamax depth state =
    if depth = 0 orelse G.terminal state
    then (G.eval state, NONE)
    else
      let
        fun loop [] best bm = (best, bm)
          | loop (m :: ms) best bm =
              let val (cs, _) = negamax (depth - 1) (G.apply state m)
                  val s = ~ cs
              in  if s > best
                  then loop ms s (SOME m)
                  else loop ms best bm
              end
        (* seed with first move so best move is always SOME when non-terminal *)
        val firstMove = hd (G.moves state)
        val (fs, _) = negamax (depth - 1) (G.apply state firstMove)
        val initBest = ~ fs
      in
        loop (tl (G.moves state)) initBest (SOME firstMove)
      end

  (* ------------------------------------------------------------------ *)
  (* Alpha-beta negamax.                                                  *)
  fun alphaBeta depth alpha beta state =
    if depth = 0 orelse G.terminal state
    then (G.eval state, NONE)
    else
      let
        fun loop [] a best bm = (best, bm)
          | loop (m :: ms) a best bm =
              let val (cs, _) = alphaBeta (depth - 1) (~beta) (~a)
                                          (G.apply state m)
                  val s = ~ cs
              in  if s >= beta
                  then (s, SOME m)       (* beta cutoff *)
                  else
                    let val a' = if s > a then s else a
                        val (best', bm') = if s > best
                                           then (s, SOME m)
                                           else (best, bm)
                    in  loop ms a' best' bm' end
              end
        val firstMove = hd (G.moves state)
        val (fs, _) = alphaBeta (depth - 1) (~beta) (~alpha)
                                (G.apply state firstMove)
        val initBest = ~ fs
        val initA = if initBest > alpha then initBest else alpha
      in
        if initBest >= beta then (initBest, SOME firstMove)
        else loop (tl (G.moves state)) initA initBest (SOME firstMove)
      end

  (* ------------------------------------------------------------------ *)
  (* Principal-variation search (NegaScout).                             *)
  fun pvs depth alpha beta state =
    if depth = 0 orelse G.terminal state
    then (G.eval state, NONE)
    else
      let
        val ms = G.moves state
        fun searchFirst m =
          let val (cs, _) = pvs (depth - 1) (~beta) (~alpha) (G.apply state m)
          in ~ cs end
        fun loop [] a best bm = (best, bm)
          | loop (m :: ms2) a best bm =
              let
                (* zero-window search *)
                val (cs0, _) = pvs (depth - 1) (~ (a + 1)) (~a)
                                   (G.apply state m)
                val s0 = ~ cs0
                val s =
                  if s0 > a andalso s0 < beta
                  then (* re-search with full window *)
                    let val (cs2, _) = pvs (depth - 1) (~beta) (~s0)
                                           (G.apply state m)
                    in ~ cs2 end
                  else s0
                val a' = if s > a then s else a
                val (best', bm') = if s > best
                                   then (s, SOME m)
                                   else (best, bm)
              in
                if s >= beta then (s, SOME m)
                else loop ms2 a' best' bm'
              end
      in
        case ms of
          [] => (G.eval state, NONE)
        | (first :: rest) =>
            let
              val s1 = searchFirst first
              val a1 = if s1 > alpha then s1 else alpha
              val (best, bm) =
                if s1 >= beta then (s1, SOME first)
                else loop rest a1 s1 (SOME first)
            in
              (best, bm)
            end
      end

  (* ------------------------------------------------------------------ *)
  (* Iterative-deepening alpha-beta.                                      *)
  fun idab maxDepth state =
    let
      fun iter d result =
        if d > maxDepth then result
        else iter (d + 1) (alphaBeta d nINF INF state)
    in
      iter 1 (alphaBeta 1 nINF INF state)
    end

  fun bestMove depth state =
    if G.terminal state then NONE
    else #2 (idab depth state)

end
