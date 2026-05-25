namespace Server.Tests

open System
open System.IO
open Microsoft.Data.Sqlite
open Microsoft.VisualStudio.TestTools.UnitTesting
open Server
open Shared
open Shared.Game

[<TestClass>]
type ApiTests() =
    let createStore () =
        let path = Path.Combine(Path.GetTempPath(), sprintf "cs20200-2048-%s.sqlite" (Guid.NewGuid().ToString("N")))
        let store = LeaderboardStore(path)
        store.Initialize()
        store, path

    let cleanup path =
        SqliteConnection.ClearAllPools()

        if File.Exists path then
            File.Delete path

    [<TestMethod>]
    member _.``ranked session can be created and submitted``() =
        async {
            let store, path = createStore ()

            try
                let api = Api.createGameApi store
                let! session = api.startRankedGame defaultSettings

                let submission =
                    { SessionId = session.SessionId
                      Nickname = "Tester"
                      Moves = [ Left; Up; Right ]
                      DurationMs = 15000
                      UsedUndo = false }

                let! result = api.submitScore submission

                match result with
                | Ok entry ->
                    Assert.AreEqual<string>("Tester", entry.Nickname)
                    Assert.AreEqual<int>(3, entry.MoveCount)
                    Assert.IsTrue(entry.Score >= 0)
                | Error message -> Assert.Fail(message)
            finally
                cleanup path
        }
        |> Async.RunSynchronously

    [<TestMethod>]
    member _.``undo submissions are rejected``() =
        async {
            let store, path = createStore ()

            try
                let api = Api.createGameApi store
                let! session = api.startRankedGame defaultSettings

                let submission =
                    { SessionId = session.SessionId
                      Nickname = "Tester"
                      Moves = [ Left ]
                      DurationMs = 5000
                      UsedUndo = true }

                let! result = api.submitScore submission

                match result with
                | Ok _ -> Assert.Fail("Undo games should not be ranked.")
                | Error message -> Assert.AreEqual<string>("Undo games are unranked.", message)
            finally
                cleanup path
        }
        |> Async.RunSynchronously

    [<TestMethod>]
    member _.``session cannot be submitted twice``() =
        async {
            let store, path = createStore ()

            try
                let api = Api.createGameApi store
                let! session = api.startRankedGame defaultSettings

                let submission =
                    { SessionId = session.SessionId
                      Nickname = "Tester"
                      Moves = [ Left ]
                      DurationMs = 5000
                      UsedUndo = false }

                let! first = api.submitScore submission
                let! second = api.submitScore submission

                match first, second with
                | Ok _, Error message -> Assert.AreEqual<string>("This ranked game session was already submitted.", message)
                | _ -> Assert.Fail("Expected first submission to pass and second to fail.")
            finally
                cleanup path
        }
        |> Async.RunSynchronously

    [<TestMethod>]
    member _.``score submission matches shared replay with typed tile rules``() =
        async {
            let store, path = createStore ()

            try
                let api = Api.createGameApi store
                let! session = api.startRankedGame defaultSettings
                let moves = [ Left; Up; Right; Down; Left; Up; Left; Down ]
                let replayed = replay session.Settings session.Seed moves

                let submission =
                    { SessionId = session.SessionId
                      Nickname = "Replay"
                      Moves = moves
                      DurationMs = 21000
                      UsedUndo = false }

                let! result = api.submitScore submission

                match result with
                | Ok entry ->
                    Assert.AreEqual<int>(replayed.Score, entry.Score)
                    Assert.AreEqual<int>(maxTile replayed.Board, entry.MaxTile)
                    Assert.AreEqual<int>(replayed.Moves.Length, entry.MoveCount)
                | Error message -> Assert.Fail(message)
            finally
                cleanup path
        }
        |> Async.RunSynchronously

    [<TestMethod>]
    member _.``leaderboard ordering uses score max tile moves and duration``() =
        let store, path = createStore ()

        try
            let settings = defaultSettings
            let now = DateTimeOffset.UtcNow

            store.InsertEntry(
                { Nickname = "Slow"
                  Score = 100
                  MaxTile = 16
                  MoveCount = 20
                  DurationMs = 20000
                  Won = false
                  SubmittedAt = now },
                settings
            )

            store.InsertEntry(
                { Nickname = "Fast"
                  Score = 100
                  MaxTile = 16
                  MoveCount = 20
                  DurationMs = 10000
                  Won = false
                  SubmittedAt = now.AddSeconds(1.0) },
                settings
            )

            store.InsertEntry(
                { Nickname = "High"
                  Score = 120
                  MaxTile = 8
                  MoveCount = 30
                  DurationMs = 30000
                  Won = false
                  SubmittedAt = now.AddSeconds(2.0) },
                settings
            )

            let entries =
                store.GetLeaderboard
                    { BoardSize = settings.BoardSize
                      TargetTile = settings.TargetTile
                      Limit = 10 }

            Assert.AreEqual<string>("High", entries.[0].Nickname)
            Assert.AreEqual<string>("Fast", entries.[1].Nickname)
            Assert.AreEqual<string>("Slow", entries.[2].Nickname)
        finally
            cleanup path
