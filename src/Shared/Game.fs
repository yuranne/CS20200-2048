namespace Shared

module Game =
    type Tile =
        | Empty
        | Normal of int
        | Cubic of int
        | Joker

    type GameStatus =
        | Playing
        | Won
        | Lost

    type MoveOutcome =
        { Board: Tile list
          ScoreGained: int
          Moved: bool
          Seed: int }

    type GameState =
        { Settings: GameSettings
          Board: Tile list
          Score: int
          Seed: int
          Moves: Direction list
          Status: GameStatus
          UsedUndo: bool }

    let defaultSettings : GameSettings =
        { BoardSize = 4
          TargetTile = 2048 }

    let private clamp minValue maxValue value =
        value |> max minValue |> min maxValue

    let private isPowerOfTwo value =
        value > 0 && (value &&& (value - 1)) = 0

    let normalizeSettings (settings: GameSettings) : GameSettings =
        let target =
            if settings.TargetTile >= 128 && settings.TargetTile <= 8192 && isPowerOfTwo settings.TargetTile then
                settings.TargetTile
            else
                defaultSettings.TargetTile

        { BoardSize = clamp 3 6 settings.BoardSize
          TargetTile = target }

    let isEmpty tile =
        tile = Empty

    let tileDisplay tile =
        match tile with
        | Empty -> ""
        | Normal value -> string value
        | Cubic value -> sprintf "%i³" value
        | Joker -> "J"

    let tileScoreValue tile =
        match tile with
        | Empty
        | Joker -> 0
        | Normal value -> value
        | Cubic value -> value * 3

    let tileRankValue = tileScoreValue

    let emptyBoard (settings: GameSettings) =
        let normalized = normalizeSettings settings
        List.replicate (normalized.BoardSize * normalized.BoardSize) Empty

    let maxTile board =
        board |> List.fold (fun highest tile -> max highest (tileRankValue tile)) 0

    let hasWon (settings: GameSettings) (board: Tile list) =
        maxTile board >= (normalizeSettings settings).TargetTile

    let private nextSeed seed =
        let unsigned = (int64 seed * 1103515245L + 12345L) &&& 0x7fffffffL
        int unsigned

    let private nextInt maxExclusive seed =
        let seed' = nextSeed seed
        seed', seed' % maxExclusive

    let private jokerRoll seed =
        let seed', roll = nextInt 8 seed
        seed', roll = 0

    let spawnTile seed board =
        let emptyCells =
            board
            |> List.indexed
            |> List.choose (fun (index, value) -> if isEmpty value then Some index else None)

        match emptyCells with
        | [] -> board, seed, false
        | _ ->
            let seedAfterCell, cellOffset = nextInt emptyCells.Length seed
            let seedAfterValue, valueRoll = nextInt 10 seedAfterCell
            let index = emptyCells.[cellOffset]
            let tile = if valueRoll = 0 then Normal 4 else Normal 2

            let board' =
                board
                |> List.mapi (fun currentIndex value -> if currentIndex = index then tile else value)

            board', seedAfterValue, true

    let private tryMergePair seed left right =
        match left, right with
        | Normal leftValue, Normal rightValue when leftValue = rightValue ->
            let tile = Normal(leftValue * 2)
            Some(tile, tileScoreValue tile, seed)
        | Cubic baseValue, Normal normalValue
        | Normal normalValue, Cubic baseValue when baseValue = normalValue ->
            let seed', jokerCreated = jokerRoll seed

            if jokerCreated then
                Some(Joker, 0, seed')
            else
                let tile = Normal(baseValue * 4)
                Some(tile, tileScoreValue tile, seed')
        | Joker, Normal value
        | Normal value, Joker ->
            let tile = Normal(value * 2)
            Some(tile, tileScoreValue tile, seed)
        | _ -> None

    let mergeLine seed line =
        let rec merge currentSeed score merged remaining =
            match remaining with
            | Normal left :: Normal middle :: Normal right :: tail when left = middle && middle = right ->
                let tile = Cubic left
                merge currentSeed (score + tileScoreValue tile) (tile :: merged) tail
            | left :: right :: tail ->
                match tryMergePair currentSeed left right with
                | Some(tile, scoreGained, seed') ->
                    merge seed' (score + scoreGained) (tile :: merged) tail
                | None ->
                    merge currentSeed score (left :: merged) (right :: tail)
            | value :: tail -> merge currentSeed score (value :: merged) tail
            | [] -> List.rev merged, score, currentSeed

        let compact = line |> List.filter (isEmpty >> not)
        let merged, score, seed' = merge seed 0 [] compact
        let padded = merged @ List.replicate (line.Length - merged.Length) Empty
        padded, score, seed'

    let private lines (direction: Direction) (size: int) (board: Tile list) : Tile list list =
        match direction with
        | Left ->
            [ for row in 0 .. size - 1 ->
                  [ for col in 0 .. size - 1 -> board.[row * size + col] ] ]
        | Right ->
            [ for row in 0 .. size - 1 ->
                  [ for col in size - 1 .. -1 .. 0 -> board.[row * size + col] ] ]
        | Up ->
            [ for col in 0 .. size - 1 ->
                  [ for row in 0 .. size - 1 -> board.[row * size + col] ] ]
        | Down ->
            [ for col in 0 .. size - 1 ->
                  [ for row in size - 1 .. -1 .. 0 -> board.[row * size + col] ] ]

    let private boardFromLines (direction: Direction) (size: int) (allLines: Tile list list) =
        let values = Array.create (size * size) Empty

        for lineIndex, line in List.indexed allLines do
            for offset, value in List.indexed line do
                let row, col =
                    match direction with
                    | Left -> lineIndex, offset
                    | Right -> lineIndex, size - 1 - offset
                    | Up -> offset, lineIndex
                    | Down -> size - 1 - offset, lineIndex

                values.[row * size + col] <- value

        values |> Array.toList

    let moveBoard (settings: GameSettings) (direction: Direction) seed (board: Tile list) =
        let size = (normalizeSettings settings).BoardSize

        let movedLines, score, seed' =
            lines direction size board
            |> List.fold
                (fun (mergedLines, totalScore, currentSeed) line ->
                    let line', scoreGained, nextSeed = mergeLine currentSeed line
                    line' :: mergedLines, totalScore + scoreGained, nextSeed)
                ([], 0, seed)
            |> fun (mergedLines, totalScore, nextSeed) -> List.rev mergedLines, totalScore, nextSeed

        let board' = boardFromLines direction size movedLines

        { Board = board'
          ScoreGained = score
          Moved = board' <> board
          Seed = if board' = board then seed else seed' }

    let canMove (settings: GameSettings) (board: Tile list) =
        board |> List.exists isEmpty
        || [ Up; Down; Left; Right ]
           |> List.exists (fun direction -> (moveBoard settings direction 1 board).Moved)

    let newGame (settings: GameSettings) seed =
        let normalized = normalizeSettings settings
        let board0 = emptyBoard normalized
        let board1, seed1, _ = spawnTile seed board0
        let board2, seed2, _ = spawnTile seed1 board1

        { Settings = normalized
          Board = board2
          Score = 0
          Seed = seed2
          Moves = []
          Status = Playing
          UsedUndo = false }

    let applyMove (state: GameState) (direction: Direction) =
        match state.Status with
        | Won
        | Lost ->
            { Board = state.Board
              ScoreGained = 0
              Moved = false
              Seed = state.Seed },
            state
        | Playing ->
            let outcome = moveBoard state.Settings direction state.Seed state.Board

            if not outcome.Moved then
                outcome, state
            else
                let boardAfterSpawn, seedAfterSpawn, _ = spawnTile outcome.Seed outcome.Board

                let score = state.Score + outcome.ScoreGained

                let status =
                    if hasWon state.Settings boardAfterSpawn then Won
                    elif canMove state.Settings boardAfterSpawn then Playing
                    else Lost

                let state' =
                    { state with
                        Board = boardAfterSpawn
                        Score = score
                        Seed = seedAfterSpawn
                        Moves = state.Moves @ [ direction ]
                        Status = status }

                outcome, state'

    let replay (settings: GameSettings) seed (moves: Direction list) =
        moves
        |> List.fold (fun state direction -> applyMove state direction |> snd) (newGame settings seed)
