module Client.App

open System
open Browser.Dom
open Browser.Types
open Elmish
open Elmish.React
open Fable.Core
open Fable.Remoting.Client
open Feliz
open Shared
open Shared.Game

type Theme =
    | Classic
    | Light
    | Dark
    | HighContrast

type SideTab =
    | LeaderboardTab
    | StatsTab
    | SettingsTab

type LocalStats =
    { Games: int
      Wins: int
      BestScore: int
      HighestTile: int }

type GameModel =
    { State: GameState
      History: GameState list
      Session: GameSessionDto option
      RankedRequestId: int
      StartedAtMs: int
      FinishedRecorded: bool }

type LeaderboardModel =
    { Entries: LeaderboardEntryDto list
      Loading: bool
      Error: string option
      Nickname: string
      SubmitStatus: string option }

type UiModel =
    { Theme: Theme
      ActiveTab: SideTab
      PointerStart: (float * float) option
      ShareText: string option }

type Model =
    { Game: GameModel
      Leaderboard: LeaderboardModel
      Stats: LocalStats
      DraftSettings: GameSettings
      Ui: UiModel }

type Msg =
    | StartNewGame
    | RankedGameStarted of int * GameSessionDto
    | RankedGameFailed of int * exn
    | MoveRequested of Direction
    | UndoRequested
    | SubmitScore
    | ScoreSubmitted of Result<LeaderboardEntryDto, string>
    | ScoreSubmitFailed of exn
    | LeaderboardLoaded of LeaderboardEntryDto list
    | LeaderboardFailed of exn
    | SetNickname of string
    | SetTheme of Theme
    | SetBoardSize of int
    | SetTargetTile of int
    | SetTab of SideTab
    | PointerStarted of (float * float)
    | PointerEnded of (float * float)
    | ShareRequested
    | ClearShare

let private api : IGameApi =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<IGameApi>

let private statsStorageKey = "cs20200-2048-stats"
let private nicknameStorageKey = "cs20200-2048-nickname"
let private themeStorageKey = "cs20200-2048-theme"

[<Emit("Date.now()")>]
let private dateNow () : float = jsNative

[<Emit("!!(document.activeElement && (document.activeElement.isContentEditable || ['INPUT', 'TEXTAREA', 'SELECT'].includes(document.activeElement.tagName)))")>]
let private activeElementAcceptsText () : bool = jsNative

let private nowMs () =
    int (dateNow ())

let private localSeed () =
    nowMs () &&& 0x7fffffff

let private emptyStats =
    { Games = 0
      Wins = 0
      BestScore = 0
      HighestTile = 0 }

let private saveStats stats =
    let value = sprintf "%i|%i|%i|%i" stats.Games stats.Wins stats.BestScore stats.HighestTile
    window.localStorage.setItem(statsStorageKey, value)

let private loadStats () =
    try
        let raw = window.localStorage.getItem(statsStorageKey)

        if isNull raw then
            emptyStats
        else
            let tryParse (value: string) =
                match Int32.TryParse value with
                | true, parsed -> Some parsed
                | _ -> None

            match raw.Split('|') |> Array.map tryParse with
            | [| Some games; Some wins; Some bestScore; Some highestTile |] ->
                { Games = games
                  Wins = wins
                  BestScore = bestScore
                  HighestTile = highestTile }
            | _ -> emptyStats
    with _ ->
        emptyStats

let private loadNickname () =
    try
        let raw = window.localStorage.getItem(nicknameStorageKey)

        if isNull raw || String.IsNullOrWhiteSpace raw then
            "Player"
        else
            raw
    with _ ->
        "Player"

let private saveNickname nickname =
    window.localStorage.setItem(nicknameStorageKey, nickname)

let private themeKey theme =
    match theme with
    | Classic -> "classic"
    | Light -> "light"
    | Dark -> "dark"
    | HighContrast -> "contrast"

let private themeLabel theme =
    match theme with
    | Classic -> "Classic"
    | Light -> "Light"
    | Dark -> "Dark"
    | HighContrast -> "Contrast"

let private themeFromKey value =
    match value with
    | "light" -> Light
    | "dark" -> Dark
    | "contrast" -> HighContrast
    | _ -> Classic

let private loadTheme () =
    try
        let raw = window.localStorage.getItem(themeStorageKey)
        if isNull raw then Classic else themeFromKey raw
    with _ ->
        Classic

let private saveTheme theme =
    window.localStorage.setItem(themeStorageKey, themeKey theme)

let private newGameModel (settings: GameSettings) seed session requestId =
    { State = newGame settings seed
      History = []
      Session = session
      RankedRequestId = requestId
      StartedAtMs = nowMs ()
      FinishedRecorded = false }

let private leaderboardCmd (settings: GameSettings) =
    Cmd.OfAsync.either
        api.getLeaderboard
        { BoardSize = settings.BoardSize
          TargetTile = settings.TargetTile
          Limit = 10 }
        LeaderboardLoaded
        LeaderboardFailed

let private startRankedCmd requestId (settings: GameSettings) =
    Cmd.OfAsync.either
        api.startRankedGame
        settings
        (fun session -> RankedGameStarted(requestId, session))
        (fun error -> RankedGameFailed(requestId, error))

let init () =
    let settings = defaultSettings

    let model =
        { Game = newGameModel settings (localSeed ()) None 0
          Leaderboard =
            { Entries = []
              Loading = true
              Error = None
              Nickname = loadNickname ()
              SubmitStatus = None }
          Stats = loadStats ()
          DraftSettings = settings
          Ui =
            { Theme = loadTheme ()
              ActiveTab = LeaderboardTab
              PointerStart = None
              ShareText = None } }

    model, Cmd.batch [ startRankedCmd model.Game.RankedRequestId settings; leaderboardCmd settings ]

let private recordFinishedGame (model: Model) =
    if model.Game.FinishedRecorded || model.Game.State.Status = Playing then
        model
    else
        let state = model.Game.State

        let stats =
            { Games = model.Stats.Games + 1
              Wins = model.Stats.Wins + if state.Status = Won then 1 else 0
              BestScore = max model.Stats.BestScore state.Score
              HighestTile = max model.Stats.HighestTile (maxTile state.Board) }

        saveStats stats

        { model with
            Stats = stats
            Game = { model.Game with FinishedRecorded = true } }

let private directionFromKey key =
    match key with
    | "ArrowUp"
    | "w"
    | "W" -> Some Up
    | "ArrowDown"
    | "s"
    | "S" -> Some Down
    | "ArrowLeft"
    | "a"
    | "A" -> Some Left
    | "ArrowRight"
    | "d"
    | "D" -> Some Right
    | _ -> None

let private keyboardSubscription (_: Model) : Sub<Msg> =
    let subscribe dispatch =
        let handler =
            fun (event: Event) ->
                let keyboardEvent = event :?> KeyboardEvent

                if not (activeElementAcceptsText ()) then
                    match directionFromKey keyboardEvent.key with
                    | Some direction ->
                        keyboardEvent.preventDefault()
                        dispatch (MoveRequested direction)
                    | None -> ()

        window.addEventListener ("keydown", handler)

        { new IDisposable with
            member _.Dispose() =
                window.removeEventListener ("keydown", handler) }

    [ [ "keyboard" ], subscribe ]

let private directionFromSwipe ((startX, startY): float * float) ((endX, endY): float * float) =
    let dx = endX - startX
    let dy = endY - startY
    let threshold = 24.0

    if max (abs dx) (abs dy) < threshold then
        None
    elif abs dx > abs dy then
        if dx > 0.0 then Some Right else Some Left
    else if dy > 0.0 then
        Some Down
    else
        Some Up

let private submitScoreCmd model =
    match model.Game.Session with
    | None ->
        Cmd.ofMsg (ScoreSubmitted(Error "This local game is not connected to a ranked session."))
    | Some session ->
        let submission =
            { SessionId = session.SessionId
              Nickname = model.Leaderboard.Nickname
              Moves = model.Game.State.Moves
              DurationMs = max 1 (nowMs () - model.Game.StartedAtMs)
              UsedUndo = model.Game.State.UsedUndo }

        Cmd.OfAsync.either api.submitScore submission ScoreSubmitted ScoreSubmitFailed

let update msg model =
    match msg with
    | StartNewGame ->
        let settings = normalizeSettings model.DraftSettings
        let requestId = model.Game.RankedRequestId + 1

        let model' =
            { model with
                Game = newGameModel settings (localSeed ()) None requestId
                DraftSettings = settings
                Leaderboard =
                    { model.Leaderboard with
                        Loading = true
                        Error = None
                        SubmitStatus = None }
                Ui = { model.Ui with ShareText = None } }

        model', Cmd.batch [ startRankedCmd requestId settings; leaderboardCmd settings ]

    | RankedGameStarted(requestId, session) ->
        if requestId <> model.Game.RankedRequestId then
            model, Cmd.none
        elif model.Game.State.Moves.IsEmpty && model.Game.Session.IsNone then
            let model' =
                { model with
                    Game = newGameModel session.Settings session.Seed (Some session) requestId
                    Leaderboard =
                        { model.Leaderboard with
                            SubmitStatus = Some "Ranked session ready." } }

            model', Cmd.none
        else
            { model with
                Leaderboard =
                    { model.Leaderboard with
                        SubmitStatus = Some "Ranked session is ready for the next new game." } },
            Cmd.none

    | RankedGameFailed(requestId, error) ->
        if requestId <> model.Game.RankedRequestId then
            model, Cmd.none
        else
            { model with
                Leaderboard =
                    { model.Leaderboard with
                        SubmitStatus = Some(sprintf "Ranked mode unavailable: %s" error.Message) } },
            Cmd.none

    | MoveRequested direction ->
        let outcome, state' = applyMove model.Game.State direction

        if not outcome.Moved then
            model, Cmd.none
        else
            let game' =
                { model.Game with
                    State = state'
                    History = model.Game.State :: model.Game.History }

            let model' =
                { model with Game = game' }
                |> recordFinishedGame

            model', Cmd.none

    | UndoRequested ->
        match model.Game.History with
        | previous :: rest ->
            let state' = { previous with UsedUndo = true }

            { model with
                Game =
                    { model.Game with
                        State = state'
                        History = rest
                        FinishedRecorded = false }
                Leaderboard =
                    { model.Leaderboard with
                        SubmitStatus = Some "Undo used. This run is unranked." } },
            Cmd.none
        | [] -> model, Cmd.none

    | SubmitScore ->
        { model with
            Leaderboard =
                { model.Leaderboard with
                    SubmitStatus = Some "Submitting score..." } },
        submitScoreCmd model

    | ScoreSubmitted result ->
        match result with
        | Ok entry ->
            { model with
                Leaderboard =
                    { model.Leaderboard with
                        SubmitStatus = Some(sprintf "Submitted %i points." entry.Score)
                        Loading = true } },
            leaderboardCmd model.Game.State.Settings
        | Error message ->
            { model with
                Leaderboard =
                    { model.Leaderboard with
                        SubmitStatus = Some message } },
            Cmd.none

    | ScoreSubmitFailed error ->
        { model with
            Leaderboard =
                { model.Leaderboard with
                    SubmitStatus = Some(sprintf "Submit failed: %s" error.Message) } },
        Cmd.none

    | LeaderboardLoaded entries ->
        { model with
            Leaderboard =
                { model.Leaderboard with
                    Entries = entries
                    Loading = false
                    Error = None } },
        Cmd.none

    | LeaderboardFailed error ->
        { model with
            Leaderboard =
                { model.Leaderboard with
                    Loading = false
                    Error = Some error.Message } },
        Cmd.none

    | SetNickname nickname ->
        saveNickname nickname

        { model with
            Leaderboard = { model.Leaderboard with Nickname = nickname } },
        Cmd.none

    | SetTheme theme ->
        saveTheme theme
        { model with Ui = { model.Ui with Theme = theme } }, Cmd.none

    | SetBoardSize size ->
        let settings = normalizeSettings { model.DraftSettings with BoardSize = size }
        { model with DraftSettings = settings }, Cmd.none

    | SetTargetTile target ->
        let settings = normalizeSettings { model.DraftSettings with TargetTile = target }
        { model with DraftSettings = settings }, Cmd.none

    | SetTab tab ->
        { model with Ui = { model.Ui with ActiveTab = tab } }, Cmd.none

    | PointerStarted point ->
        { model with Ui = { model.Ui with PointerStart = Some point } }, Cmd.none

    | PointerEnded point ->
        match model.Ui.PointerStart |> Option.bind (fun start -> directionFromSwipe start point) with
        | Some direction ->
            { model with Ui = { model.Ui with PointerStart = None } }, Cmd.ofMsg (MoveRequested direction)
        | None ->
            { model with Ui = { model.Ui with PointerStart = None } }, Cmd.none

    | ShareRequested ->
        let state = model.Game.State

        let text =
            sprintf
                "2048 score: %i, max tile: %i, moves: %i, board: %ix%i"
                state.Score
                (maxTile state.Board)
                state.Moves.Length
                state.Settings.BoardSize
                state.Settings.BoardSize

        { model with Ui = { model.Ui with ShareText = Some text } }, Cmd.none

    | ClearShare ->
        { model with Ui = { model.Ui with ShareText = None } }, Cmd.none

let private tileClass value =
    if value = 0 then
        "tile tile-empty"
    else
        sprintf "tile tile-value tile-%i" value

let private statusText status =
    match status with
    | Playing -> "Playing"
    | Won -> "You reached the target."
    | Lost -> "No moves left."

let private formatDuration (ms: int) =
    let seconds = ms / 1000
    sprintf "%i:%02i" (seconds / 60) (seconds % 60)

let private scoreCard (label: string) (value: string) =
    Html.div [
        prop.className "score-card"
        prop.children [
            Html.span [ prop.text label ]
            Html.strong [ prop.text value ]
        ]
    ]

let private actionButton (label: string) (disabled: bool) onClick =
    Html.button [
        prop.className "action-button"
        prop.disabled disabled
        prop.onClick onClick
        prop.text label
    ]

let private boardView model dispatch =
    let state = model.Game.State
    let size = state.Settings.BoardSize

    Html.section [
        prop.className "board-shell"
        prop.tabIndex 0
        prop.autoFocus true
        prop.children [
            Html.div [
                prop.className (sprintf "board board-size-%i" size)
                prop.onPointerDown (fun (event: PointerEvent) ->
                    event.preventDefault()
                    dispatch (PointerStarted(event.clientX, event.clientY)))
                prop.onPointerUp (fun (event: PointerEvent) ->
                    event.preventDefault()
                    dispatch (PointerEnded(event.clientX, event.clientY)))
                prop.onPointerCancel (fun _ -> dispatch (PointerEnded(0.0, 0.0)))
                prop.children [
                    for value in state.Board do
                        Html.div [
                            prop.className (tileClass value)
                            prop.text (if value = 0 then "" else string value)
                        ]
                ]
            ]

            if state.Status <> Playing then
                Html.div [
                    prop.className "status-panel"
                    prop.children [
                        Html.h2 [ prop.text (statusText state.Status) ]
                        Html.p [
                            prop.text (
                                sprintf "Score %i with max tile %i." state.Score (maxTile state.Board)
                            )
                        ]
                        Html.div [
                            prop.className "inline-actions"
                            prop.children [
                                actionButton "Submit" false (fun _ -> dispatch SubmitScore)
                                actionButton "New" false (fun _ -> dispatch StartNewGame)
                            ]
                        ]
                    ]
                ]
        ]
    ]

let private headerView model dispatch =
    let state = model.Game.State

    Html.header [
        prop.className "app-header"
        prop.children [
            Html.div [
                prop.children [
                    Html.p [ prop.className "eyebrow"; prop.text "F# SAFE 2048" ]
                    Html.h1 [ prop.text "2048" ]
                ]
            ]
            Html.div [
                prop.className "score-row"
                prop.children [
                    scoreCard "Score" (string state.Score)
                    scoreCard "Best" (string model.Stats.BestScore)
                    scoreCard "Max" (string (maxTile state.Board))
                ]
            ]
        ]
    ]

let private controlsView model dispatch =
    Html.div [
        prop.className "control-strip"
        prop.children [
            actionButton "New" false (fun _ -> dispatch StartNewGame)
            actionButton "Undo" model.Game.History.IsEmpty (fun _ -> dispatch UndoRequested)
            actionButton "Submit" (model.Game.State.Moves.IsEmpty) (fun _ -> dispatch SubmitScore)
            actionButton "Share" false (fun _ -> dispatch ShareRequested)
        ]
    ]

let private tabButton active (label: string) tab dispatch =
    Html.button [
        prop.className (if active then "tab-button tab-active" else "tab-button")
        prop.onClick (fun _ -> dispatch (SetTab tab))
        prop.text label
    ]

let private leaderboardView model dispatch =
    Html.div [
        prop.className "panel-content"
        prop.children [
            Html.label [
                prop.className "field"
                prop.children [
                    Html.span [ prop.text "Nickname" ]
                    Html.input [
                        prop.value model.Leaderboard.Nickname
                        prop.maxLength 16
                        prop.onChange (SetNickname >> dispatch)
                    ]
                ]
            ]

            match model.Leaderboard.SubmitStatus with
            | Some message -> Html.p [ prop.className "notice"; prop.text message ]
            | None -> Html.none

            if model.Leaderboard.Loading then
                Html.p [ prop.className "muted"; prop.text "Loading leaderboard..." ]
            else
                match model.Leaderboard.Error with
                | Some message -> Html.p [ prop.className "error"; prop.text message ]
                | None ->
                    Html.div [
                        prop.className "leaderboard-list"
                        prop.children [
                            for index, entry in model.Leaderboard.Entries |> List.indexed do
                                Html.div [
                                    prop.className "leaderboard-entry"
                                    prop.children [
                                        Html.span [ prop.className "rank"; prop.text (string (index + 1)) ]
                                        Html.span [ prop.className "player"; prop.text entry.Nickname ]
                                        Html.strong [ prop.text (string entry.Score) ]
                                        Html.span [ prop.text (sprintf "%i tile" entry.MaxTile) ]
                                        Html.span [ prop.text (formatDuration entry.DurationMs) ]
                                    ]
                                ]

                            if model.Leaderboard.Entries.IsEmpty then
                                Html.p [ prop.className "muted"; prop.text "No ranked scores yet." ]
                        ]
                    ]
        ]
    ]

let private statsView model =
    let winRate =
        if model.Stats.Games = 0 then
            "0%"
        else
            sprintf "%i%%" (model.Stats.Wins * 100 / model.Stats.Games)

    Html.div [
        prop.className "panel-content stats-grid"
        prop.children [
            scoreCard "Games" (string model.Stats.Games)
            scoreCard "Wins" (string model.Stats.Wins)
            scoreCard "Win rate" winRate
            scoreCard "Highest" (string model.Stats.HighestTile)
        ]
    ]

let private optionItem (value: string) (label: string) =
    Html.option [
        prop.value value
        prop.text label
    ]

let private settingsView model dispatch =
    Html.div [
        prop.className "panel-content"
        prop.children [
            Html.label [
                prop.className "field"
                prop.children [
                    Html.span [ prop.text "Board" ]
                    Html.select [
                        prop.value (string model.DraftSettings.BoardSize)
                        prop.onChange (fun (value: string) -> dispatch (SetBoardSize(int value)))
                        prop.children [
                            optionItem "3" "3 x 3"
                            optionItem "4" "4 x 4"
                            optionItem "5" "5 x 5"
                            optionItem "6" "6 x 6"
                        ]
                    ]
                ]
            ]
            Html.label [
                prop.className "field"
                prop.children [
                    Html.span [ prop.text "Target" ]
                    Html.select [
                        prop.value (string model.DraftSettings.TargetTile)
                        prop.onChange (fun (value: string) -> dispatch (SetTargetTile(int value)))
                        prop.children [
                            optionItem "1024" "1024"
                            optionItem "2048" "2048"
                            optionItem "4096" "4096"
                            optionItem "8192" "8192"
                        ]
                    ]
                ]
            ]
            Html.label [
                prop.className "field"
                prop.children [
                    Html.span [ prop.text "Theme" ]
                    Html.select [
                        prop.value (themeKey model.Ui.Theme)
                        prop.onChange (themeFromKey >> SetTheme >> dispatch)
                        prop.children [
                            optionItem "classic" "Classic"
                            optionItem "light" "Light"
                            optionItem "dark" "Dark"
                            optionItem "contrast" "Contrast"
                        ]
                    ]
                ]
            ]
            Html.p [
                prop.className "muted"
                prop.text (sprintf "Current theme: %s" (themeLabel model.Ui.Theme))
            ]
        ]
    ]

let private sidePanel model dispatch =
    Html.aside [
        prop.className "side-panel"
        prop.children [
            Html.div [
                prop.className "tab-row"
                prop.children [
                    tabButton (model.Ui.ActiveTab = LeaderboardTab) "Scores" LeaderboardTab dispatch
                    tabButton (model.Ui.ActiveTab = StatsTab) "Stats" StatsTab dispatch
                    tabButton (model.Ui.ActiveTab = SettingsTab) "Settings" SettingsTab dispatch
                ]
            ]
            match model.Ui.ActiveTab with
            | LeaderboardTab -> leaderboardView model dispatch
            | StatsTab -> statsView model
            | SettingsTab -> settingsView model dispatch
        ]
    ]

let private sharePanel model dispatch =
    match model.Ui.ShareText with
    | None -> Html.none
    | Some text ->
        Html.div [
            prop.className "share-panel"
            prop.children [
                Html.textarea [
                    prop.readOnly true
                    prop.value text
                ]
                actionButton "Close" false (fun _ -> dispatch ClearShare)
            ]
        ]

let view model dispatch =
    Html.main [
        prop.className "app"
        prop.custom ("data-theme", themeKey model.Ui.Theme)
        prop.children [
            headerView model dispatch
            controlsView model dispatch
            Html.div [
                prop.className "layout"
                prop.children [
                    boardView model dispatch
                    sidePanel model dispatch
                ]
            ]
            sharePanel model dispatch
        ]
    ]

Program.mkProgram init update view
|> Program.withSubscription keyboardSubscription
|> Program.withReactSynchronous "root"
|> Program.run
