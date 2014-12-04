namespace UnitTestProject1

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open FiveInRow.Core
open FiveInRow.Core.GameDef
open FiveInRow.Core.Board
open FiveInRow.Core.Threats

[<TestClass>]
type ThreatTests() =
    [<TestMethod>]
    member x.TestFourThreatMatch1 () =
        let s = 
            [
                Obstacle, (-1, -1)
                Rest, (0, 0)
                Rest, (1, 1)
                Rest, (2, 2)
                Available, (3, 3)
                Available, (4, 4)
            ]
        let threats = Threats.matchThreat s
        Assert.AreEqual(2, threats.Length)
        Assert.IsTrue(threats |> Seq.exists (fun t -> fst t = Four && (snd t).Gain = (3, 3)))
        Assert.IsTrue(threats |> Seq.exists (fun t -> fst t = Four && (snd t).Gain = (4, 4)))
      
    [<TestMethod>]
    member x.TestFourThreatDiscovery1 () = 
        let (board, _) = Board.replay [ (0, 0); (6, 1); (1, 1); (6, 3); (2, 2) ]
        let threats = identifyThreatsUnconstrained Player1 board |> Seq.toArray
        Assert.AreEqual(2, threats.Length)
        Assert.IsTrue(threats |> Array.exists (fun t -> fst t = Four && (snd t).Gain = (3, 3)))
        Assert.IsTrue(threats |> Array.exists (fun t -> fst t = Four && (snd t).Gain = (4, 4)))

    [<TestMethod>]
    member x.TestFourThreatDiscovery2 () = 
        let (board, _) = Board.replay [ (3, 3); (3, 2); (3, 4); (6, 3); (3, 5) ]
        let threats = identifyThreatsUnconstrained Player1 board |> Seq.toArray
        Assert.AreEqual(2, threats.Length)
        Assert.IsTrue(threats |> Array.exists (fun t -> fst t = Four && (snd t).Gain = (3, 6)))
        Assert.IsTrue(threats |> Array.exists (fun t -> fst t = Four && (snd t).Gain = (3, 7)))

    [<TestMethod>]
    member x.TestFourThreatDiscovery3 () = 
        let (board, _) = Board.replay [ (3, boardDimension - 1); (6, 2); (3, boardDimension - 2); (6, 5); (3, boardDimension - 3) ]
        let threats = identifyThreatsUnconstrained Player1 board |> Seq.toArray
        Assert.AreEqual(2, threats.Length)
        Assert.IsTrue(threats |> Array.exists (fun t -> fst t = Four && (snd t).Gain = (3, boardDimension - 4)))
        Assert.IsTrue(threats |> Array.exists (fun t -> fst t = Four && (snd t).Gain = (3, boardDimension - 5)))

    [<TestMethod>]
    member x.TestTwoThreatDiscovery4 () = 
        let (board, _) = Board.replay [ (3, 4); (12, 2); (3, 5); ]
        let threats = identifyThreatsUnconstrained Player1 board |> Seq.toArray
        Assert.AreEqual(2, threats.Length)
        Assert.IsTrue(threats |> Array.exists (fun t -> fst t = Three && (snd t).Gain = (3, 6)))
        Assert.IsTrue(threats |> Array.exists (fun t -> fst t = Three && (snd t).Gain = (3, 3)))
