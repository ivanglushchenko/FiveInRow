module FiveInRow.Core.RowX

open GameDef
open Row

type RowX = { S: Row option
              E: Row option
              SE: Row option
              SW: Row option }

let empty = { S = None
              E = None
              SE = None
              SW = None }

let isEmpty rx =
    Option.isNone rx.S && Option.isNone rx.E && Option.isNone rx.SE && Option.isNone rx.SW

let update dir r rx =
    match dir with
    | S -> { rx with S = r }
    | E -> { rx with E = r }
    | SE -> { rx with SE = r }
    | SW -> { rx with SW = r }

let get dir rx =
    match dir with
    | S -> rx.S
    | E -> rx.E
    | SE -> rx.SE
    | SW -> rx.SW