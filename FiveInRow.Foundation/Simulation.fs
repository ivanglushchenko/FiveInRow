module FiveInRow.Foundation.Simulation

open GameDef
open GameAI

type Battle(d1: Difficulty, d2: Difficulty) =
    do
        boardDimension <- 19

    let getter = function
        | Easy -> fun b -> EasyAI(b) :> AI
        | Medium -> fun b -> MediumAI(b) :> AI
        | Hard -> fun b -> HardAI(b) :> AI

    let aiMap = [ (Player1, getter d1); (Player2, getter d2) ] |> Map.ofList

    let startingBoard = Board.Create 19
    
    member x.Play () =
        let rec loop (board: Board) =
            let ai = aiMap.[board.Player] board
            match ai.Winner with
            | Some(p) -> Some(p)
            | _ ->
                match ai.Moves with
                | (pos, _) :: _ ->
                    match board.Set pos with
                    | Some(nextBoard) -> loop nextBoard
                    | None -> None
                | _ -> None
        loop startingBoard
            
