module FiveInRow.Core.Threat

open System.Collections.Generic
open FiveInRow.Core.GameDef

type ThreatData = 
    {
        // The gain square of a threat is the square played by the attacker
        Gain: Point;
        // The cost squares of a threat are the squares played by the defender, in response to the threat
        Cost: Point list
        // The rest squares of a threat are the squares containing a threat possibility, the gain square excepted
        Rest: Point list
    }
    override x.ToString() =
        let costs = x.Cost |> List.map (fun (x, y) -> sprintf "%i,%i" (x + 1) (y + 1))
        let costsStr = System.String.Join("; ", costs |> List.toArray)
        sprintf "{ Gain %i,%i;  Cost %s }" (fst x.Gain + 1) (snd x.Gain + 1) costsStr

type ThreatType =
    // Line of seven squares of which the three center squares are occupied by the attacker, and the remaining four squares are empty
    // ..ooo..
    // Line of six squares, with three consecutive squares of the four center squares occupied by the attacker, and the remaining three squares empty
    // x.ooo..
    | Three
    // Line of six squares of which the attacker has occupied three non-consecutive squares of the four center squares, while the other three squares are empty
    // .o.oo.
    | BrokenThree
    // Line of five squares, of which the attacker has occupied any four, with the fifth square empty
    // xoooo.
    | Four
    // Line of six squares, of which the attacker has occupied the four center squares
    // .oooo.
    | StraightFour
    | Five

    override x.ToString() =
        match x with
        | Three -> "Three"
        | BrokenThree -> "BrokenThree"
        | Four -> "Four"
        | StraightFour -> "StraightFour"
        | Five -> "Five"

type Threat = ThreatType * ThreatData

type ThreatTreeNode =
    {
        Threat: Threat
        Dependencies: ThreatTreeNode list option
    }

type BoardSequenceElement = 
    | InBoardSquare of SquareStatus * Point
    | Border
    override x.ToString() =
        match x with
        | InBoardSquare (s, (r, c)) -> sprintf "{%i, %i} %O" r c s
        | Border -> "B"

// Different kinds of threat pattern squares, from current player's perspective
type ThreatPatternSquareKind =
    | Gain
    | Cost
    | Rest
    | Available
    | Obstacle
    override x.ToString() =
        match x with
        | Gain -> "Gain"
        | Cost -> "Cost"
        | Rest -> "Rest"
        | Available -> "Available"
        | Obstacle -> "Obstacle"

let threatPatterns =
    [
        // b: ..xx..
        // a: oxxxo.
        Three, [ Cost; Gain; Rest; Rest; Cost; Available ]
        // ..x.x.
        // .oxxxo
        Three, [ Available; Cost; Rest; Gain; Rest; Cost ]
        // ..x.x.
        // oxxoxo
        BrokenThree, [ Cost; Gain; Rest; Cost; Rest; Cost ]
        // b: oxxx..
        // a: oxxxxo
        Four, [Obstacle; Rest; Rest; Rest; Gain; Cost ]
        // b: oxxx..
        // a: oxxxox
        Four, [Obstacle; Rest; Rest; Rest; Cost; Gain ]
        // b: oxx.x.
        // a: oxxxxo
        Four, [Obstacle; Rest; Rest; Gain; Rest; Cost ]
        // b: oxx.x.
        // a: oxxoxx
        Four, [Obstacle; Rest; Rest; Cost; Rest; Gain ]
        // b: oxx..x
        // a: oxxxox
        Four, [Obstacle; Rest; Rest; Gain; Cost; Rest ]
        // b: oxx..x
        // a: oxxoxx
        Four, [Obstacle; Rest; Rest; Cost; Gain; Rest ]
        // b: ox.x.x
        // a: oxxxox
        Four, [Obstacle; Rest; Gain; Rest; Cost; Rest ]
        // b: ox.x.x
        // a: oxoxxx
        Four, [Obstacle; Rest; Cost; Rest; Gain; Rest ]
        // b: ox..xx
        // a: oxxoxx
        Four, [Obstacle; Rest; Gain; Cost; Rest; Rest ]
        // b: ox..xx
        // a: oxoxxx
        Four, [Obstacle; Rest; Cost; Gain; Rest; Rest ]
        // b: ox.xx.
        // a: oxoxxx
        Four, [Obstacle; Rest; Cost; Rest; Rest; Gain]
        // b: ox.xx.
        // a: oxxxxo
        Four, [Obstacle; Rest; Gain; Rest; Rest; Cost]

        // no cost squares because this is supposed to be the last move 
        StraightFour, [ Available; Gain; Rest; Rest; Rest; Available ]
        StraightFour, [ Available; Rest; Gain; Rest; Rest; Available ]
        Five, [ Rest; Rest; Rest; Rest; Gain ]
        Five, [ Rest; Rest; Rest; Gain; Rest ]
        Five, [ Rest; Rest; Gain; Rest; Rest ]
    ] 

type ThreatPatternTreeNode =
    {
        Value: ThreatType option
        Children: System.Collections.Generic.Dictionary<ThreatPatternSquareKind, ThreatPatternTreeNode>
    }
   
let minPatternLength = threatPatterns |> List.map (snd >> List.length) |> List.min
let maxPatternLength = threatPatterns |> List.map (snd >> List.length) |> List.max

let threatPatternTree =
    let rec build patterns =
        let discoveredThreats, continuations = patterns |> List.partition (snd >> List.isEmpty)
        let split = function | (k, hd :: tl) -> Some (k, hd, tl) | _ -> None
        let splittedPatterns = continuations |> List.choose split
        let node = Dictionary<ThreatPatternSquareKind, ThreatPatternTreeNode>()
        for next, group in splittedPatterns |> Seq.groupBy (fun (kind, next, tail) -> next) do
            let nextNodes = group |> Seq.map (fun (k, _, tl) -> k, tl) |> Seq.toList |> build
            node.Add(next, nextNodes)
        let threat = 
            match discoveredThreats with
            | hd :: [] -> hd |> fst |> Some
            | [] -> None
            | _ -> failwith "More than one threat detected"
        { Value = threat; Children = node }
    let reverseNonSymmetrical list =
        let rev = List.rev list
        if rev = list then None else Some rev
    let reversedPatterns = 
        threatPatterns 
        |> List.map (fun (kind, squares) -> kind, reverseNonSymmetrical squares)
        |> List.filter (snd >> Option.isSome)
        |> List.map (fun (k, v) -> k, Option.get v)
    build (threatPatterns @ reversedPatterns)

let isWinningThreat = function | Five | StraightFour -> true | _ -> false

let private matchThreatOld s =
    let matchThreatPattern sequence (kind, pattern) =
        let rec matchNext sequence pattern gain cost rest =
            match sequence, pattern with
            | (Rest, p) :: tl_seq,      Rest :: tl_pat      -> matchNext tl_seq tl_pat gain cost (p :: rest)
            | (Available, p) :: tl_seq, Gain :: tl_pat      -> matchNext tl_seq tl_pat (p :: gain) cost rest
            | (Available, p) :: tl_seq, Cost :: tl_pat      -> matchNext tl_seq tl_pat gain (p :: cost) rest
            | (Available, p) :: tl_seq, Available :: tl_pat -> matchNext tl_seq tl_pat gain cost rest
            | (Obstacle, _) :: tl_seq,  Obstacle :: tl_pat  -> matchNext tl_seq tl_pat gain cost rest
            | [], [] -> Some { Gain = List.head gain; Cost = cost; Rest = rest }
            | _ -> None
        match matchNext sequence pattern [] [] [] with
        | Some data -> (kind, data) |> Some
        | _ -> None
    seq {
        yield! threatPatterns |> List.choose (matchThreatPattern s)
        yield! threatPatterns |> List.map (fun (k, s) -> k, List.rev s) |> List.choose (matchThreatPattern s)
        } |> Seq.toList

let private matchThreatNew s =
    let rec traverseTree s node gain cost rest =
        seq {
            let has key = node.Children.ContainsKey key
            let get key = node.Children.[key]
            match s with
            | (Rest, p) :: tl when has Rest -> 
                yield! traverseTree tl (get Rest) gain cost (p :: rest)
            | (Available, p) :: tl ->
                if has Gain then 
                    yield! traverseTree tl (get Gain) (p :: gain) cost rest
                if has Cost then
                    yield! traverseTree tl (get Cost) gain (p :: cost) rest
                if has Available then
                    yield! traverseTree tl (get Available) gain cost rest
            | (Obstacle, _) :: tl when has Obstacle ->
                yield! traverseTree tl (get Obstacle) gain cost rest
            | [] ->
                match node.Value with
                | Some threatType -> yield threatType, { Gain = gain.Head; Cost = cost; Rest = rest }
                | _ -> ()
            | _ -> () }
    traverseTree s threatPatternTree [] [] [] |> Seq.toList

let matchThreat = matchThreatNew