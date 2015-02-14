module FiveInRow.Core.SquareX

open GameDef
open Row
open Threat

type SquareX<'a> =
    {
        S: 'a option
        E: 'a option
        SE: 'a option
        SW: 'a option
    }

    override x.ToString() = 
        let tos name = function | Some t -> sprintf "%s: %O" name t | _ -> ""
        let parts = [| tos "S" x.S; tos "E" x.E; tos "SE" x.SE; tos "SW" x.SW |]
        System.String.Join(", ", parts |> Array.filter (fun s -> s.Length > 0))

let empty =
    {
        S = None
        E = None
        SE = None
        SW = None
    }

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

type RowX = SquareX<Row>
type ThreatX = SquareX<Threat list>

let toSeq sx = 
    seq { yield sx.S
          yield sx.E
          yield sx.SE
          yield sx.SW }

let isThreatXEmpty x = 
    let isEmpty = function | Some (hd :: _) -> false | _ -> true
    isEmpty x.S && isEmpty x.E && isEmpty x.SE && isEmpty x.SW

let txToString x =
    let tos name = function | Some (hd :: _) -> sprintf "%s: %O" name hd | _ -> ""
    let parts = [| tos "S" x.S; tos "E" x.E; tos "SE" x.SE; tos "SW" x.SW |]
    System.String.Join(", ", parts |> Array.filter (fun s -> s.Length > 0))