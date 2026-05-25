namespace Shared.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open Shared
open Shared.Game

[<TestClass>]
type GameTests() =
    let assertList expected actual =
        Assert.IsTrue((expected = actual), sprintf "Expected %A but got %A" expected actual)

    let row tiles =
        tiles @ List.replicate (4 - List.length tiles) Empty

    [<TestMethod>]
    member _.``mergeLine gives triple normals priority``() =
        let line, score, _ = mergeLine 1 (row [ Normal 2; Normal 2; Normal 2 ])
        assertList (row [ Cubic 2 ]) line
        Assert.AreEqual<int>(6, score)

    [<TestMethod>]
    member _.``mergeLine keeps fourth normal after triple merge``() =
        let line, score, _ = mergeLine 1 (row [ Normal 2; Normal 2; Normal 2; Normal 2 ])
        assertList (row [ Cubic 2; Normal 2 ]) line
        Assert.AreEqual<int>(6, score)

    [<TestMethod>]
    member _.``mergeLine keeps classic pair merges when no triple exists``() =
        let line, score, _ = mergeLine 1 (row [ Normal 2; Normal 2; Normal 4 ])
        assertList (row [ Normal 4; Normal 4 ]) line
        Assert.AreEqual<int>(4, score)

    [<TestMethod>]
    member _.``cubic plus matching normal can create Joker``() =
        let line, score, seed' = mergeLine 3 (row [ Cubic 2; Normal 2 ])
        assertList (row [ Joker ]) line
        Assert.AreEqual<int>(0, score)
        Assert.AreNotEqual<int>(3, seed')

    [<TestMethod>]
    member _.``cubic plus matching normal creates four n when Joker misses``() =
        let line, score, seed' = mergeLine 1 (row [ Cubic 2; Normal 2 ])
        assertList (row [ Normal 8 ]) line
        Assert.AreEqual<int>(8, score)
        Assert.AreNotEqual<int>(1, seed')

    [<TestMethod>]
    member _.``Joker merges with normal tile and doubles it``() =
        let line, score, _ = mergeLine 1 (row [ Joker; Normal 16 ])
        assertList (row [ Normal 32 ]) line
        Assert.AreEqual<int>(32, score)

    [<TestMethod>]
    member _.``Joker does not merge with Joker``() =
        let line, score, _ = mergeLine 1 (row [ Joker; Joker ])
        assertList (row [ Joker; Joker ]) line
        Assert.AreEqual<int>(0, score)

    [<TestMethod>]
    member _.``Joker does not merge with cubic tile``() =
        let line, score, _ = mergeLine 1 (row [ Joker; Cubic 2 ])
        assertList (row [ Joker; Cubic 2 ]) line
        Assert.AreEqual<int>(0, score)

    [<TestMethod>]
    member _.``tile display and rank values match special tile policy``() =
        Assert.AreEqual<string>("2³", tileDisplay (Cubic 2))
        Assert.AreEqual<string>("J", tileDisplay Joker)
        Assert.AreEqual<int>(6, tileRankValue (Cubic 2))
        Assert.AreEqual<int>(0, tileRankValue Joker)

    [<TestMethod>]
    member _.``move without board change does not spawn``() =
        let settings = defaultSettings

        let state =
            { Settings = settings
              Board = [ Normal 2; Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ]
              Score = 0
              Seed = 42
              Moves = []
              Status = Playing
              UsedUndo = false }

        let outcome, state' = applyMove state Left
        Assert.IsFalse(outcome.Moved)
        assertList state.Board state'.Board
        Assert.AreEqual<int>(42, state'.Seed)

    [<TestMethod>]
    member _.``win and loss detection work``() =
        Assert.IsTrue(hasWon defaultSettings [ Normal 2048; Empty; Empty; Empty ])

        let fullLockedBoard =
            [ Normal 2; Normal 4; Normal 2; Normal 4
              Normal 4; Normal 2; Normal 4; Normal 2
              Normal 2; Normal 4; Normal 2; Normal 4
              Normal 4; Normal 2; Normal 4; Normal 2 ]

        Assert.IsFalse(canMove defaultSettings fullLockedBoard)

    [<TestMethod>]
    member _.``seeded spawn is deterministic``() =
        let board = emptyBoard defaultSettings
        let boardA, seedA, spawnedA = spawnTile 123 board
        let boardB, seedB, spawnedB = spawnTile 123 board
        assertList boardA boardB
        Assert.AreEqual<int>(seedA, seedB)
        Assert.AreEqual<bool>(spawnedA, spawnedB)

    [<TestMethod>]
    member _.``seeded replay is deterministic across Joker and spawn RNG``() =
        let state =
            { Settings = defaultSettings
              Board = [ Cubic 2; Normal 2; Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ]
              Score = 0
              Seed = 3
              Moves = []
              Status = Playing
              UsedUndo = false }

        let _, first = applyMove state Left
        let _, second = applyMove state Left
        assertList first.Board second.Board
        Assert.AreEqual<int>(first.Score, second.Score)
        Assert.AreEqual<int>(first.Seed, second.Seed)

    [<TestMethod>]
    member _.``settings are normalized``() =
        let normalized =
            normalizeSettings
                { BoardSize = 99
                  TargetTile = 123 }

        Assert.AreEqual<int>(6, normalized.BoardSize)
        Assert.AreEqual<int>(2048, normalized.TargetTile)
