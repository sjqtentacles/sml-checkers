(* checkers.sml

   English Draughts (American Checkers), 8×8 board.
   32 dark squares, numbered 0..31 (row-major, dark squares only).

   Row 0 (top, Black side):  sq  0  1  2  3  → cols 1,3,5,7
   Row 1:                    sq  4  5  6  7  → cols 0,2,4,6
   ...
   Row 7 (bottom, White):    sq 28 29 30 31  → cols 0,2,4,6

   Black starts on rows 0-2 (sq 0-11), moves south (increasing row).
   White starts on rows 5-7 (sq 20-31), moves north (decreasing row).
*)

structure Checkers :> CHECKERS =
struct

  val Black = 1
  val White = 2

  type piece = { player : int, king : bool }
  type move  = { from : int, captures : int list, dest : int }
  type state = { board : piece option array, turn : int }

  fun squareToRC sq =
    let val row = sq div 4
        val pos = sq mod 4
        val col = if row mod 2 = 0 then pos * 2 + 1 else pos * 2
    in (row, col) end

  fun rcToSquare row col =
    if row < 0 orelse row > 7 orelse col < 0 orelse col > 7 then NONE
    else if (row + col) mod 2 = 0 then NONE
    else
      let val pos = if row mod 2 = 0 then (col - 1) div 2 else col div 2
      in  SOME (row * 4 + pos) end

  fun startPos () =
    let val b = Array.array (32, NONE)
        val () = List.app (fn i => Array.update (b, i,
                              SOME {player=Black, king=false}))
                           (List.tabulate (12, fn i => i))
        val () = List.app (fn i => Array.update (b, i,
                              SOME {player=White, king=false}))
                           (List.tabulate (12, fn i => i + 20))
    in  { board = b, turn = Black } end

  val startPos = startPos ()

  fun pieceAt (st : state) sq = Array.sub (#board st, sq)
  fun toMove  (st : state) = #turn st
  fun moveFrom (m : move) = #from m
  fun moveTo   (m : move) = #dest m
  fun moveCaptures (m : move) = #captures m

  fun opp p = if p = Black then White else Black

  fun diagNeighbour sq dr dc =
    let val (r, c) = squareToRC sq
    in  rcToSquare (r + dr) (c + dc) end

  (* Generate jump sequences. Returns list of complete moves (multi-jump). *)
  fun genJumps (board : piece option array) sq p isKing
               (fromSq : int) (caps : int list) : move list =
    let
      val dirs =
        if isKing then [(1,1),(1,~1),(~1,1),(~1,~1)]
        else if p = Black then [(1,1),(1,~1)]
        else [(~1,1),(~1,~1)]
      fun tryDir (dr, dc) =
        case diagNeighbour sq dr dc of
          NONE => []
        | SOME mid =>
            if List.exists (fn c => c = mid) caps then []
            else
              case Array.sub (board, mid) of
                NONE => []
              | SOME {player=ep, king=_} =>
                  if ep = p then []
                  else
                    case diagNeighbour mid dr dc of
                      NONE => []
                    | SOME land =>
                        if Array.sub (board, land) <> NONE then []
                        else
                          let
                            val newCaps = caps @ [mid]
                            val newBoard =
                              Array.tabulate (32, fn i => Array.sub (board, i))
                            val () = Array.update (newBoard, mid, NONE)
                            val () = Array.update (newBoard, sq, NONE)
                            val () = Array.update (newBoard, land,
                                       SOME {player=p, king=isKing})
                            val sub = genJumps newBoard land p isKing fromSq newCaps
                          in
                            if null sub
                            then [{from=fromSq, captures=newCaps, dest=land}]
                            else sub
                          end
      val all = List.concat (List.map tryDir dirs)
    in all end

  fun genSimple (board : piece option array) sq p isKing : move list =
    let
      val dirs =
        if isKing then [(1,1),(1,~1),(~1,1),(~1,~1)]
        else if p = Black then [(1,1),(1,~1)]
        else [(~1,1),(~1,~1)]
    in
      List.mapPartial (fn (dr, dc) =>
        case diagNeighbour sq dr dc of
          NONE => NONE
        | SOME dest =>
            if Array.sub (board, dest) = NONE
            then SOME {from=sq, captures=[], dest=dest}
            else NONE)
        dirs
    end

  fun legalMoves (st : state) =
    let
      val p = #turn st
      val board = #board st
      fun sqs () =
        List.filter (fn sq =>
          case Array.sub (board, sq) of
            SOME {player=q, king=_} => q = p
          | NONE => false)
          (List.tabulate (32, fn i => i))
      val jumps =
        List.concat (List.map (fn sq =>
          case Array.sub (board, sq) of
            SOME {player=_, king=k} => genJumps board sq p k sq []
          | NONE => []) (sqs ()))
    in
      if not (null jumps) then jumps
      else
        List.concat (List.map (fn sq =>
          case Array.sub (board, sq) of
            SOME {player=_, king=k} => genSimple board sq p k
          | NONE => []) (sqs ()))
    end

  fun terminal (st : state) = null (legalMoves st)

  fun makeMove (st : state) (mv : move) =
    let
      val board = Array.tabulate (32, fn i => Array.sub (#board st, i))
      val piece = Array.sub (board, #from mv)
      val ()    = Array.update (board, #from mv, NONE)
      val ()    = List.app (fn c => Array.update (board, c, NONE)) (#captures mv)
      val dest  = #dest mv
      val (player, isKing) =
        case piece of
          SOME {player=p, king=k} =>
            (p, k orelse (p = Black andalso dest >= 28)
                    orelse (p = White andalso dest <= 3))
        | NONE => (Black, false)
      val () = Array.update (board, dest, SOME {player=player, king=isKing})
    in
      { board = board, turn = opp (#turn st) }
    end

  fun toString (st : state) =
    let
      fun cellStr sq =
        case Array.sub (#board st, sq) of
          NONE => "."
        | SOME {player=p, king=k} =>
            if p = Black then (if k then "B" else "b")
            else (if k then "W" else "w")
      fun rowStr row =
        let
          fun colStr col =
            if (row + col) mod 2 = 1 then
              case rcToSquare row col of
                SOME sq => cellStr sq
              | NONE => "?"
            else "."
        in String.concat (List.map colStr (List.tabulate (8, fn i => i)))
        end
    in
      String.concatWith "\n" (List.map rowStr (List.tabulate (8, fn i => i))) ^ "\n"
    end

  (* GAME interface *)
  local
    structure CkG : GAME =
    struct
      type state = { board : piece option array, turn : int }
      type move  = { from : int, captures : int list, dest : int }
      fun hash (st : state) =
        let val h = ref (#turn st)
        in  Array.appi (fn (i, v) =>
              h := !h * 5 + i +
                   (case v of
                      NONE => 0
                    | SOME {player=p, king=k} => p * 2 + (if k then 1 else 0)))
            (#board st) ; !h
        end
      fun moves st = legalMoves st
      fun apply st mv = makeMove st mv
      fun terminal st = null (legalMoves st)
      fun eval (st : state) =
        let
          val h = ref 0
          val () = Array.app (fn v =>
            case v of
              NONE => ()
            | SOME {player=p, king=k} =>
                let val v2 = if k then 3 else 1
                in  h := !h + (if p = #turn st then v2 else ~v2) end)
            (#board st)
        in !h end
    end
    structure S = GameTreeSearch(CkG)
  in
    fun bestMove depth (st : state) =
      if terminal st then NONE
      else S.bestMove depth st
  end

end
