﻿namespace UnitTestProject1

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
        Assert.IsTrue(Option.isSome ai.NextMove)
        let topAnswer = Option.get ai.NextMove
        Assert.IsTrue(topAnswer = (7, 4) || topAnswer = (4, 7))

    [<TestMethod>]
    member x.TestEasyAI2 () = 
        let (board, _) = Board.replay [ (7, 8); (8, 8); (7, 9) ]
        let ai = AI.getEasy Player1 board
        Assert.IsTrue(Option.isSome ai.NextMove)
        let topAnswer = Option.get ai.NextMove
        Assert.AreEqual(topAnswer, (7, 7))

        let ai = AI.getEasy Player2 board
        Assert.IsTrue(Option.isSome ai.NextMove)
        let topAnswer = Option.get ai.NextMove
        Assert.AreEqual(topAnswer, (7, 7))
        
    [<TestMethod>]
    member x.TestEasyAI3 () = 
        let (board, _) = Board.replay [ (5, 5); (5, 6); (6, 5); (6, 6);(5, 4); (6, 7); (6, 3) ]
        let ai = AI.getEasy Player1 board
        Assert.IsTrue(Option.isSome ai.NextMove)
        let topAnswer = Option.get ai.NextMove
        Assert.AreEqual(topAnswer, (4, 5))

        let ai = AI.getEasy Player2 board
        Assert.IsTrue(Option.isSome ai.NextMove)
        let topAnswer = Option.get ai.NextMove
        Assert.AreEqual(topAnswer, (4, 5))

    [<TestMethod>]
    member x.TestMediumAI1 () = 
        let (board, _) = Board.replay [ (7, 8); (8, 7); (6, 7); (8, 9); (7, 9); (8, 8); (7, 10) ]
        let ai = AI.getMedium Player2 board
        Assert.IsTrue(Option.isSome ai.NextMove)
        let topAnswer = Option.get ai.NextMove
        Assert.AreEqual(topAnswer,  (8, 10))

    [<TestMethod>]
    member x.TestHardAI1 () = 
        let (board, _) = Board.replay [ (8, 6); (7, 5); (7, 7); (6, 8); (6, 6); (5, 5); (6, 5); (6, 4); (7, 6); (5, 6); (5, 4); (4, 3); (8, 7); (9, 8); (9, 7); (6, 7); (11, 7); (10, 7); (8, 8) ]
        let ai = AI.getHard Player2 board
        Assert.IsTrue(Option.isSome ai.NextMove)
        let topAnswer = Option.get ai.NextMove
        Assert.AreEqual(topAnswer,  (8, 9))

    [<TestMethod>]
    member x.TestHardAI2 () = 
        let (board, _) = Board.replay [ (7, 8); (8, 7); (6, 7); (8, 9); (7, 9); (8, 8); (7, 10) ]
        let ai = AI.getHard Player2 board
        Assert.IsTrue(Option.isSome ai.NextMove)
        let topAnswer = Option.get ai.NextMove
        Assert.AreEqual(topAnswer,  (8, 6))