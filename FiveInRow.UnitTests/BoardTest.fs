namespace UnitTestProject1

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open FiveInRow.Core
open FiveInRow.Core.GameDef
open FiveInRow.Core.Board

[<TestClass>]
type BoardTest() = 
    [<TestMethod>]
    member x.TestEmpty () = 
        Assert.AreEqual(0, Board.empty.Moves.Length)

    [<TestMethod>]
    member x.TestExtendEmpty () =
        let b = extend (0, 0) Player1 Board.empty
        Assert.AreEqual(1, b.Moves.Length)
        Assert.AreEqual(0, b.Rows.Length)
        Assert.AreEqual(0, Board.empty.Moves.Length)
        Assert.AreEqual(0, Board.empty.Rows.Length)

    [<TestMethod>]
    member x.TestEnumerateCells () =
        let r1 = Row.create (5, 5) (5, 10)
        let c1 = Row.enumerateCells E r1 |> Seq.toList
        Assert.AreEqual([ 5, 5; 5, 6; 5, 7; 5, 8; 5, 9; 5, 10 ], c1)

        let r2 = Row.create (5, 5) (10, 10)
        let c2 = Row.enumerateCells SE r2 |> Seq.toList
        Assert.AreEqual([ 5, 5; 6, 6; 7, 7; 8, 8; 9, 9; 10, 10 ], c2)

        let r3 = Row.create (5, 5) (10, 5)
        let c3 = Row.enumerateCells S r3 |> Seq.toList
        Assert.AreEqual([ 5, 5; 6, 5; 7, 5; 8, 5; 9, 5; 10, 5 ], c3)

        let r4 = Row.create (5, 10) (10, 5)
        let c4 = Row.enumerateCells SW r4 |> Seq.toList
        Assert.AreEqual([ 5, 10; 6, 9; 7, 8; 8, 7; 9, 6; 10, 5 ], c4)

    [<TestMethod>]
    member x.TestAddOneRow () =
        let b = 
            Board.empty
            |> extend (0, 0) Player1
            |> extend (0, 1) Player1
        Assert.AreEqual(2, b.Moves.Length)
        Assert.AreEqual(2, b.Rows.Length)
        Assert.AreEqual(1, getRowsCount b)
        Assert.AreEqual(1, ((getRows b) |> Seq.head).Rank)

        let b = 
            Board.empty
            |> extend (0, 2) Player1
            |> extend (0, 1) Player1
        Assert.AreEqual(2, b.Moves.Length)
        Assert.AreEqual(2, b.Rows.Length)
        Assert.AreEqual(1, getRowsCount b)
        Assert.AreEqual(2, ((getRows b) |> Seq.head).Rank)

        Assert.AreEqual(0, Board.empty.Moves.Length)
        Assert.AreEqual(0, Board.empty.Rows.Length)

    [<TestMethod>]
    member x.TestAddTwoRows () =
        let b = 
            Board.empty
            |> extend (0, 0) Player1
            |> extend (1, 1) Player1
            |> extend (0, 2) Player1
        Assert.AreEqual(3, b.Moves.Length)
        Assert.AreEqual(3, b.Rows.Length)
        Assert.AreEqual(2, getRowsCount b)
        Assert.AreEqual(1, Board.getRowRank (1, 1) SE b)
        Assert.AreEqual(1, Board.getRowRank (1, 1) SW b)

        Assert.AreEqual(0, Board.empty.Moves.Length)
        Assert.AreEqual(0, Board.empty.Rows.Length)

    [<TestMethod>]
    member x.TestAddThreeRowsInTriangle () =
        let b = 
            Board.empty
            |> extend (0, 0) Player1
            |> extend (1, 1) Player1
            |> extend (0, 1) Player1
        Assert.AreEqual(3, b.Moves.Length)
        Assert.AreEqual(3, b.Rows.Length)
        Assert.AreEqual(3, getRowsCount b)
        Assert.AreEqual(1, Board.getRowRank (1, 1) S b)
        Assert.AreEqual(1, Board.getRowRank (1, 1) SE b)
        Assert.AreEqual(1, Board.getRowRank (0, 0) SE b)
        Assert.AreEqual(1, Board.getRowRank (0, 0) E b)
        Assert.AreEqual(1, Board.getRowRank (0, 1) E b)
        Assert.AreEqual(1, Board.getRowRank (0, 1) E b)
        Assert.AreEqual(0, Board.empty.Moves.Length)
        Assert.AreEqual(0, Board.empty.Rows.Length)

    [<TestMethod>]
    member x.TestAddOneRowLength3 () =
        let b = 
            Board.empty
            |> extend (3, 3) Player1
            |> extend (2, 2) Player1
            |> extend (1, 1) Player1
        Assert.AreEqual(3, b.Moves.Length)
        Assert.AreEqual(2, b.Rows.Length)
        Assert.AreEqual(1, getRowsCount b)
        Assert.AreEqual(2, Board.getRowRank (1, 1) SE b)
        Assert.AreEqual(2, Board.getRowRank (3, 3) SE b)

    [<TestMethod>]
    member x.TestAddOneRowLength3_Another () =
        let b = 
            Board.empty
            |> extend (0, 0) Player1
            |> extend (2, 2) Player1
            |> extend (1, 1) Player1
        Assert.AreEqual(3, b.Moves.Length)
        Assert.AreEqual(2, b.Rows.Length)
        Assert.AreEqual(1, getRowsCount b)
        Assert.AreEqual(1, Board.getRowRank (0, 0) SE b)
        Assert.AreEqual(1, Board.getRowRank (2, 2) SE b)

    [<TestMethod>]
    member x.TestAddManyRows () =
        let b = 
            Board.empty
            |> extend (1, 0) Player1
            |> extend (1, 2) Player1
            |> extend (2, 1) Player1
            |> extend (1, 1) Player1
        Assert.AreEqual(4, b.Moves.Length)
        Assert.AreEqual(4, b.Rows.Length)
        Assert.AreEqual(4, getRowsCount b)
        Assert.AreEqual(1, Board.getRowRank (2, 1) SE b)
        Assert.AreEqual(2, Board.getRowRank (2, 1) S b)
        Assert.AreEqual(2, Board.getRowRank (2, 1) SW b)
        Assert.AreEqual(2, Board.getRowRank (1, 1) S b)
        Assert.AreEqual(1, Board.getRowRank (1, 0) E b)
        Assert.AreEqual(1, Board.getRowRank (1, 0) SE b)
        Assert.AreEqual(1, Board.getRowRank (1, 2) E b)
        Assert.AreEqual(2, Board.getRowRank (1, 2) SW b)

    [<TestMethod>]
    member x.TestMergeRows1 () =
        let b = 
            Board.empty
            |> extend (1, 1) Player1
            |> extend (0, 0) Player1
            |> extend (3, 3) Player1
            |> extend (4, 4) Player1
            |> extend (2, 2) Player1
        Assert.AreEqual(5, b.Moves.Length)
        Assert.AreEqual(2, b.Rows.Length)
        Assert.AreEqual(5, Board.getRowLength (0, 0) SE b)
        Assert.AreEqual(1, Board.getRowRank (0, 0) SE b)
        Assert.AreEqual(1, getRowsCount b)

    [<TestMethod>]
    member x.TestRows1 () =
        let b = 
            Board.empty
            |> extend (1, 1) Player2
            |> extend (0, 0) Player1
            |> extend (3, 3) Player2
            |> extend (4, 4) Player1
            |> extend (2, 2) Player1
        Assert.AreEqual(5, b.Moves.Length)
        Assert.AreEqual(0, b.Rows.Length)
        Assert.AreEqual(0, getRowsCount b)

    [<TestMethod>]
    member x.TestRows2 () =
        let b = 
            Board.empty
            |> extend (5, 6) Player1
            |> extend (5, 7) Player2
            |> extend (6, 6) Player1
            |> extend (6, 7) Player2
            |> extend (4, 7) Player1
        Assert.AreEqual(5, b.Moves.Length)
        Assert.AreEqual(5, b.Rows.Length)
        Assert.AreEqual(3, getRowsCount b)

    [<TestMethod>]
    member x.TestRows3 () =
        let b = 
            Board.empty
            |> extend (5, 6) Player1
            |> extend (5, 7) Player2
            |> extend (6, 6) Player1
            |> extend (6, 7) Player2
        Assert.AreEqual(4, b.Moves.Length)
        Assert.AreEqual(4, b.Rows.Length)
        Assert.AreEqual(2, getRowsCount b)

    [<TestMethod>]
    member x.TestRows4 () =
        let b = 
            Board.empty
            |> extend (5, 6) Player1
            |> extend (5, 7) Player2
            |> extend (5, 8) Player2
        Assert.AreEqual(3, b.Moves.Length)
        Assert.AreEqual(2, b.Rows.Length)
        Assert.AreEqual(1, getRowsCount b)
        Assert.AreEqual(1, Board.getRowRank (5, 7) E b)

    [<TestMethod>]
    member x.TestRows5 () =
        let b = 
            Board.empty
            |> extend (5, 6) Player1
            |> extend (5, 7) Player2
            |> extend (5, 8) Player2
            |> extend (5, 9) Player1
        Assert.AreEqual(4, b.Moves.Length)
        //Assert.AreEqual(2, b.Rows.Length)
        //Assert.AreEqual(1, getRowsCount b)
        //Assert.AreEqual(0, Board.getRowRank (5, 7) E b)

    [<TestMethod>]
    member x.TestRows6 () =
        let b = 
            Board.empty
            |> extend (3, 10) Player1
            |> extend (3, 11) Player1
            |> extend (4, 11) Player1
            |> extend (2, 8) Player1
            |> extend (3, 9) Player1
        Assert.AreEqual(5, b.Moves.Length)
        Assert.AreEqual(5, b.Rows.Length)
        Assert.AreEqual(4, getRowsCount b)
        Assert.AreEqual(2, Board.getRowRank (3, 9) E b)

    [<TestMethod>]
    member x.TestHistogram () =
        let b = extend (0, 0) Player1 Board.empty
        RowHistogram.inc Player1 5 0 b.Histogram
        Assert.AreEqual(1, RowHistogram.getCount Player1 5 0 b.Histogram)
        Assert.AreEqual(0, RowHistogram.getCount Player1 5 0 Board.empty.Histogram)