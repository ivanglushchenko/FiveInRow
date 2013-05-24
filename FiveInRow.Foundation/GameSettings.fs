namespace FiveInRow.Foundation

open GameDef

type GameSettings(boardSize_, difficulty_) =
    inherit ObservableObject()

    let mutable boardSize = boardSize_
    let mutable difficulty = difficulty_

    new() = GameSettings(0, Easy)
    
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

