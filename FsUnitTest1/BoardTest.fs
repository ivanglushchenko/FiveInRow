namespace UnitTestProject1

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open FiveInRow.Foundation.GameDef
open FiveInRow.Foundation.Immutable

[<TestClass>]
type BoardTest() = 
    [<TestMethod>]
    member x.TestEmpty () = 
        Assert.AreEqual(0, EmptyBoard.Moves.Count)

    [<TestMethod>]
    member x.TestExtendEmpty () =
        let b = extend ((0, 0), Player1) EmptyBoard
        Assert.AreEqual(1, b.Moves.Count)
        Assert.AreEqual(0, b.Rows.Count)
        Assert.AreEqual(0, EmptyBoard.Moves.Count)
        Assert.AreEqual(0, EmptyBoard.Rows.Count)

    [<TestMethod>]
    member x.TestEnumerateCells () =
        let r1 = createRow (5, 5) (5, 10)
        let c1 = enumerateCells r1 E |> Seq.toList
        Assert.AreEqual([ 5, 5; 5, 6; 5, 7; 5, 8; 5, 9; 5, 10 ], c1)

        let r2 = createRow (5, 5) (10, 10)
        let c2 = enumerateCells r2 SE |> Seq.toList
        Assert.AreEqual([ 5, 5; 6, 6; 7, 7; 8, 8; 9, 9; 10, 10 ], c2)

        let r3 = createRow (5, 5) (10, 5)
        let c3 = enumerateCells r3 S |> Seq.toList
        Assert.AreEqual([ 5, 5; 6, 5; 7, 5; 8, 5; 9, 5; 10, 5 ], c3)

        let r4 = createRow (5, 10) (10, 5)
        let c4 = enumerateCells r4 SW |> Seq.toList
        Assert.AreEqual([ 5, 10; 6, 9; 7, 8; 8, 7; 9, 6; 10, 5 ], c4)

    [<TestMethod>]
    member x.TestAddOneRow () =
        let b = 
            EmptyBoard
            |> extend ((0, 0), Player1)
            |> extend ((0, 1), Player1)
        Assert.AreEqual(2, b.Moves.Count)
        Assert.AreEqual(1, b.Rows.Count)
        Assert.AreEqual(0, EmptyBoard.Moves.Count)
        Assert.AreEqual(0, EmptyBoard.Rows.Count)