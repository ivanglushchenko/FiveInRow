namespace UnitTestProject1

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open FiveInRow.Core
open FiveInRow.Core.GameDef
open FiveInRow.Core.Board

[<TestClass>]
type AITest() = 
    [<TestMethod>]
    member x.TestEasyAI1 () = 
        let board = Board.replay [ (6, 7); (5, 6); (6, 6); (6, 5) ]
        let ai = AI.getEasy Player1 board
        Assert.AreNotEqual(0, ai.PossibleMoves.Length)
        let (topAnswer, _) = ai.PossibleMoves.Head
        Assert.IsTrue(topAnswer = (7, 4) || topAnswer = (4, 7))