namespace UnitTestProject1

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open FiveInRow.Core
open FiveInRow.Core.GameDef
open FiveInRow.Core.Position

[<TestClass>]
type PositionTest() = 
    [<TestMethod>]
    member x.PositionEnumSeq1 () = 
        let pos = Position.empty |> Position.replayForPlayer [ 0, 0; 0, 1 ] Player1
        let seq = Position.enumerateSequences pos |> Seq.map Seq.toArray |> Seq.toArray
        Assert.AreEqual(2, pos.Moves.Length)

    [<TestMethod>]
    member x.PositionEnumSeq2 () = 
        let pos = Position.empty |> Position.replayForPlayer [ 0, 0; 1, 0 ] Player1
        let seq = Position.enumerateSequences pos |> Seq.map Seq.toArray |> Seq.toArray
        Assert.AreEqual(2, pos.Moves.Length)

    [<TestMethod>]
    member x.PositionEnumSeq3 () = 
        let pos = Position.empty |> Position.replayForPlayer [ boardDimension - 4, boardDimension - 3; boardDimension - 3, boardDimension - 2 ] Player1
        let seq = Position.enumerateSequences pos |> Seq.map Seq.toArray |> Seq.toArray
        Assert.AreEqual(2, pos.Moves.Length)