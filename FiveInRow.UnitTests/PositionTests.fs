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
        let seq2 = Position.enumerateSequencesForPoint (10, 10) SE pos.Moves |> Seq.map Seq.toArray |> Seq.toArray
        Assert.AreEqual(6, seq1.Length)
        Assert.AreEqual(6, seq2.Length)

    [<TestMethod>]
    member x.PositionThreatDiscovery1 () = 
        let pos = Position.replayForPlayer [ (0, 0); (1, 1); (2, 2) ] Player1 Position.empty
        let threats = identifyThreats Player1 pos |> Seq.toArray
        let check threats =
            Assert.AreEqual(2, Array.length threats)
            Assert.IsTrue(threats |> Array.exists (fun t -> fst t = Four && (snd t).Gain = (3, 3)))
            Assert.IsTrue(threats |> Array.exists (fun t -> fst t = Four && (snd t).Gain = (4, 4)))
        let threats2 = Position.getThreats Player1 pos |> Seq.toArray
        check threats
        check threats2

    [<TestMethod>]
    member x.PositionThreatDiscovery2 () = 
        let (pos, _) = Position.replay [ (3, 3); (3, 2); (3, 4); (6, 3); (3, 5) ] Position.empty
        let threats = identifyThreats Player1 pos |> Seq.toArray
        let check threats =
            Assert.AreEqual(2, Array.length threats)
            Assert.IsTrue(threats |> Array.exists (fun t -> fst t = Four && (snd t).Gain = (3, 6)))
            Assert.IsTrue(threats |> Array.exists (fun t -> fst t = Four && (snd t).Gain = (3, 7)))
        let threats2 = Position.getThreats Player1 pos |> Seq.toArray
        check threats
        check threats2

    [<TestMethod>]
    member x.PositionThreatDiscovery3 () = 
        let (pos, _) = Position.replay [ (3, boardDimension - 1); (6, 2); (3, boardDimension - 2); (6, 5); (3, boardDimension - 3) ] Position.empty
        let threats = identifyThreats Player1 pos |> Seq.toArray
        let check threats =
            Assert.AreEqual(2, Array.length threats)
            Assert.IsTrue(threats |> Array.exists (fun t -> fst t = Four && (snd t).Gain = (3, boardDimension - 4)))
            Assert.IsTrue(threats |> Array.exists (fun t -> fst t = Four && (snd t).Gain = (3, boardDimension - 5)))
        let threats2 = Position.getThreats Player1 pos |> Seq.toArray
        check threats
        check threats2

    [<TestMethod>]
    member x.PositionThreatDiscovery4 () = 
        let (pos, _) = Position.replay [ (3, 4); (12, 2); (3, 5); ] Position.empty
        let threats = identifyThreats Player1 pos |> Seq.toArray
        let check threats =
            Assert.AreEqual(2, Array.length threats)
            Assert.IsTrue(threats |> Array.exists (fun t -> fst t = Three && (snd t).Gain = (3, 6)))
            Assert.IsTrue(threats |> Array.exists (fun t -> fst t = Three && (snd t).Gain = (3, 3)))
        let threats2 = Position.getThreats Player1 pos |> Seq.toArray
        check threats
        check threats2

    [<TestMethod>]
    member x.PositionThreatDiscovery5 () = 
        let (pos, _) = Position.replay [ (3, 4); (12, 2); (3, 5); (13, 3); (3, 6) ] Position.empty
        let checkForPlayer1 threats =
            Assert.AreEqual(2, Array.length threats)
            Assert.IsTrue(threats |> Array.exists (fun t -> fst t = StraightFour && (snd t).Gain = (3, 3)))
            Assert.IsTrue(threats |> Array.exists (fun t -> fst t = StraightFour && (snd t).Gain = (3, 7)))
        let checkForPlayer2 threats =
            Assert.AreEqual(2, Array.length threats)
            Assert.IsTrue(threats |> Array.exists (fun t -> fst t = Three && (snd t).Gain = (11, 1)))
            Assert.IsTrue(threats |> Array.exists (fun t -> fst t = Three && (snd t).Gain = (14, 4)))
        let threats = identifyThreats Player1 pos |> Seq.toArray
        let threats2 = Position.getThreats Player1 pos |> Seq.toArray
        let threats3 = identifyThreats Player2 pos |> Seq.toArray
        let threats4 = Position.getThreats Player2 pos |> Seq.toArray

        checkForPlayer1 threats
        checkForPlayer1 threats2
        checkForPlayer2 threats3
        checkForPlayer2 threats4

    [<TestMethod>]
    member x.PositionThreatDiscovery6 () = 
        let (pos, _) = Position.replay [ (4, 3); (2, 12); (5, 3); (3, 11); (6, 3) ] Position.empty
        let checkForPlayer1 threats =
            Assert.AreEqual(2, Array.length threats)
            Assert.IsTrue(threats |> Array.exists (fun t -> fst t = StraightFour && (snd t).Gain = (3, 3)))
            Assert.IsTrue(threats |> Array.exists (fun t -> fst t = StraightFour && (snd t).Gain = (7, 3)))
        let checkForPlayer2 threats =
            Assert.AreEqual(2, Array.length threats)
            Assert.IsTrue(threats |> Array.exists (fun t -> fst t = Three && (snd t).Gain = (1, 13)))
            Assert.IsTrue(threats |> Array.exists (fun t -> fst t = Three && (snd t).Gain = (4, 10)))
        let threats = identifyThreats Player1 pos |> Seq.toArray
        let threats2 = Position.getThreats Player1 pos |> Seq.toArray
        let threats3 = identifyThreats Player2 pos |> Seq.toArray
        let threats4 = Position.getThreats Player2 pos |> Seq.toArray

        checkForPlayer1 threats
        checkForPlayer1 threats2
        checkForPlayer2 threats3
        checkForPlayer2 threats4