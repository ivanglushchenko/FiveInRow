namespace FiveInRow.Foundation

open GameDef

type GameSettings(boardSize_, difficulty_, opponent_) =
    inherit ObservableObject()

    let mutable boardSize = boardSize_
    let mutable difficulty = difficulty_
    let mutable opponent = opponent_

    new() = GameSettings(0, Easy, AI(Player2))
    
    member x.BoardSize
        with get() = boardSize
        and set(v) =
            if v <> boardSize then
                boardSize <- v
                x.OnPropertyChanged(<@ x.BoardSize @>)

    member x.Difficulty
        with get() = difficulty
        and set(v) =
            if v <> difficulty then
                difficulty <- v
                x.OnPropertyChanged(<@ x.Difficulty @>)

    member x.Opponent
        with get() = opponent
        and set(v) =
            if v <> opponent then
                opponent <- v
                x.OnPropertyChanged(<@ x.Opponent @>)