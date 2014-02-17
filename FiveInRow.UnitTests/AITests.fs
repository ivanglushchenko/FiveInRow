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
        let (board, _) = Board.replay [ (6, 7); (5, 6); (6, 6); (6, 5) ]
        let ai = AI.getEasy Player1 board
        Assert.AreNotEqual(0, ai.PossibleMoves.Length)
        let (topAnswer, _) = ai.PossibleMoves.Head
        Assert.IsTrue(topAnswer = (7, 4) || topAnswer = (4, 7))

    [<TestMethod>]
    member x.TestEasyAI2 () = 
        let (board, _) = Board.replay [ (7, 8); (8, 8); (7, 9) ]
        let ai = AI.getEasy Player1 board
        Assert.AreNotEqual(0, ai.PossibleMoves.Length)
        let (topAnswer, _) = ai.PossibleMoves.Head
        Assert.AreEqual(topAnswer, (7, 7))

        let ai = AI.getEasy Player2 board
        Assert.AreNotEqual(0, ai.PossibleMoves.Length)
        let (topAnswer, _) = ai.PossibleMoves.Head
        Assert.AreEqual(topAnswer, (7, 7))
        
    [<TestMethod>]
    member x.TestEasyAI3 () = 
        let (board, _) = Board.replay [ (5, 5); (5, 6); (6, 5); (6, 6);(5, 4); (6, 7); (6, 3) ]
        let ai = AI.getEasy Player1 board
        Assert.AreNotEqual(0, ai.PossibleMoves.Length)
        let (topAnswer, _) = ai.PossibleMoves.Head
        Assert.AreEqual(topAnswer, (4, 5))

        let ai = AI.getEasy Player2 board
        Assert.AreNotEqual(0, ai.PossibleMoves.Length)
        let (topAnswer, _) = ai.PossibleMoves.Head
        Assert.AreEqual(topAnswer, (4, 5))