namespace UnitTestProject1

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open FiveInRow.Core
open FiveInRow.Core.GameDef
open FiveInRow.Core.Threat
open FiveInRow.Core.Position

[<TestClass>]
type PositionTest() = 
    [<TestMethod>]
    member x.PositionEnumSeq1 () = 
        let pos = Position.empty |> Position.replayForPlayer [ 0, 0; 0, 1 ] Player1
        let seq = Position.enumerateSequences pos |> Seq.map Seq.toArray |> Seq.toArray
        Assert.AreEqual(6, seq.Length)

    [<TestMethod>]
    member x.PositionEnumSeq2 () = 
        let pos = Position.empty |> Position.replayForPlayer [ 0, 0; 1, 0 ] Player1
        let seq = Position.enumerateSequences pos |> Seq.map Seq.toArray |> Seq.toArray
        Assert.AreEqual(6, seq.Length)

    [<TestMethod>]
    member x.PositionEnumSeq3 () = 
        let pos = Position.empty |> Position.replayForPlayer [ boardDimension - 4, boardDimension - 3; boardDimension - 3, boardDimension - 2 ] Player1
        let seq = Position.enumerateSequences pos |> Seq.map Seq.toArray |> Seq.toArray
        Assert.AreEqual(9, seq.Length)

    [<TestMethod>]
    member x.PositionEnumSeqForPoint1 () = 
        let pos = Position.empty |> Position.replayForPlayer [ 0, 0; 1, 1 ] Player1
        let seq1 = Position.enumerateSequences pos |> Seq.map Seq.toArray |> Seq.toArray
        let seq2 = Position.enumerateSequencesForPoint (10, 10) SE pos |> Seq.map Seq.toArray |> Seq.toArray
        Assert.AreEqual(6, seq1.Length)
        Assert.AreEqual(6, seq2.Length)

    [<TestMethod>]
    member x.PositionThreatDiscovery1 () = 
        let pos = Position.replayForPlayer [ (0, 0); (1, 1); (2, 2) ] Player1 Position.empty
        let threats = identifyThreats Player1 pos |> Seq.toArray
        Assert.AreEqual(2, threats.Length)
        Assert.IsTrue(threats |> Array.exists (fun t -> fst t = Four && (snd t).Gain = (3, 3)))
        Assert.IsTrue(threats |> Array.exists (fun t -> fst t = Four && (snd t).Gain = (4, 4)))

    [<TestMethod>]
    member x.PositionThreatDiscovery2 () = 
        let (pos, _) = Position.replay [ (3, 3); (3, 2); (3, 4); (6, 3); (3, 5) ] Position.empty
        let threats = identifyThreats Player1 pos |> Seq.toArray
        Assert.AreEqual(2, threats.Length)
        Assert.IsTrue(threats |> Array.exists (fun t -> fst t = Four && (snd t).Gain = (3, 6)))
        Assert.IsTrue(threats |> Array.exists (fun t -> fst t = Four && (snd t).Gain = (3, 7)))

    [<TestMethod>]
    member x.PositionThreatDiscovery3 () = 
        let (pos, _) = Position.replay [ (3, boardDimension - 1); (6, 2); (3, boardDimension - 2); (6, 5); (3, boardDimension - 3) ] Position.empty
        let threats = identifyThreats Player1 pos |> Seq.toArray
        Assert.AreEqual(2, threats.Length)
        Assert.IsTrue(threats |> Array.exists (fun t -> fst t = Four && (snd t).Gain = (3, boardDimension - 4)))
        Assert.IsTrue(threats |> Array.exists (fun t -> fst t = Four && (snd t).Gain = (3, boardDimension - 5)))

    [<TestMethod>]
    member x.PositionThreatDiscovery4 () = 
        let (pos, _) = Position.replay [ (3, 4); (12, 2); (3, 5); ] Position.empty
        let threats = identifyThreats Player1 pos |> Seq.toArray
        Assert.AreEqual(2, threats.Length)
        Assert.IsTrue(threats |> Array.exists (fun t -> fst t = Three && (snd t).Gain = (3, 6)))
        Assert.IsTrue(threats |> Array.exists (fun t -> fst t = Three && (snd t).Gain = (3, 3)))
