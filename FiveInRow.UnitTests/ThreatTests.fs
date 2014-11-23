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
    member x.TestThreatDiscovery1 () = 
        let (board, _) = Board.replay [ (0, 0); (6, 1); (1, 1); (6, 3); (2, 2) ]
        let threats = identifyThreats Player1 board |> Seq.toArray
        ()
