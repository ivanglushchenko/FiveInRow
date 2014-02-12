module FiveInRow.GameMechanics.AI

open FiveInRow.GameMechanics.GameDef

type AI = { PossibleMoves: (Position * float) list
            Winner: Player option }

let empty = { PossibleMoves = []
              Winner = None }