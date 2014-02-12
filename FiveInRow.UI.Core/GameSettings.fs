namespace FiveInRow.Core

open FiveInRow.GameMechanics.GameDef

type GameSettings(boardSize_, difficulty_, opponent_) =
    inherit ObservableObject()

    let mutable boardSize = boardSize_
    let mutable difficulty = difficulty_
    let mutable opponent = opponent_
    let mutable version = 1

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

    member x.Version
        with get() = version
        and set(v) =
            if v <> version then
                version <- v
                x.OnPropertyChanged(<@ x.Version @>)


type GameSettingsVM() =
    inherit ObservableObject()

    let mutable opponentAIPlayer1 = false
    let mutable opponentAIPlayer2 = false
    let mutable opponentHuman = false
    let mutable boardSize19 = false
    let mutable boardSize35 = false
    let mutable boardSize51 = false
    let mutable diffEasy = false
    let mutable diffMedium = false
    let mutable diffHard = false

    member x.OpponentAIPlayer1
        with get() = opponentAIPlayer1
        and set(v) =
            if v <> opponentAIPlayer1 then
                opponentAIPlayer1 <- v
                x.OnPropertyChanged(<@ x.OpponentAIPlayer1 @>)

    member x.OpponentAIPlayer2
        with get() = opponentAIPlayer2
        and set(v) =
            if v <> opponentAIPlayer2 then
                opponentAIPlayer2 <- v
                x.OnPropertyChanged(<@ x.OpponentAIPlayer2 @>)

    member x.OpponentHuman
        with get() = opponentHuman
        and set(v) =
            if v <> opponentHuman then
                opponentHuman <- v
                x.OnPropertyChanged(<@ x.OpponentHuman @>)

    member x.BoardSize19
        with get() = boardSize19
        and set(v) =
            if v <> boardSize19 then
                boardSize19 <- v
                x.OnPropertyChanged(<@ x.BoardSize19 @>)

    member x.BoardSize35
        with get() = boardSize35
        and set(v) =
            if v <> boardSize35 then
                boardSize35 <- v
                x.OnPropertyChanged(<@ x.BoardSize35 @>)

    member x.BoardSize51
        with get() = boardSize51
        and set(v) =
            if v <> boardSize51 then
                boardSize51 <- v
                x.OnPropertyChanged(<@ x.BoardSize51 @>)

    member x.DiffEasy
        with get() = diffEasy
        and set(v) =
            if v <> diffEasy then
                diffEasy <- v
                x.OnPropertyChanged(<@ x.DiffEasy @>)

    member x.DiffMedium
        with get() = diffMedium
        and set(v) =
            if v <> diffMedium then
                diffMedium <- v
                x.OnPropertyChanged(<@ x.DiffMedium @>)

    member x.DiffHard
        with get() = diffHard
        and set(v) =
            if v <> diffHard then
                diffHard <- v
                x.OnPropertyChanged(<@ x.DiffHard @>)

    member x.ToGameSettings () =
        let opponent = 
            if x.OpponentAIPlayer1 then AI(Player1)
            else if x.OpponentAIPlayer2 then AI(Player2)
            else Human
        let boardSize =
            if x.BoardSize19 then 19
            else if x.BoardSize35 then 35
            else 51
        let diff =
            if x.DiffEasy then Easy
            else if x.DiffMedium then Medium
            else Hard
        GameSettings(boardSize, diff, opponent)