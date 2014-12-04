module FiveInRow.Core.Threats

open System.Collections.Generic
open PersistentHashMap
open GameDef
open Board

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

let isInBounds (r, c) = r >=0 && c >= 0 && r < boardDimension && c < boardDimension

let enumerateSequences board =
    let getInBoardSquare (r, c) =
        InBoardSquare (Board.get r c board, (r, c))
    let isOccupied (r, c) = 
        Board.get r c board |> isOccupied
    let generateSeqOfIndices lengthFrom lengthTo p inc_p =
        let rec getNext p l acc breakIfOffboard numOfActiveSquares =
            seq {
                if l = lengthTo then 
                    if numOfActiveSquares > 1 then yield acc
                else
                    if l >= lengthFrom && numOfActiveSquares > 1 then yield acc
                    let incActiveCount = if isOccupied p then 1 else 0
                    if isInBounds p then
                        yield! getNext (inc_p p) (l + 1) (getInBoardSquare p :: acc) breakIfOffboard (numOfActiveSquares + incActiveCount) 
                    else
                        if breakIfOffboard = false then
                            yield! getNext (inc_p p) (l + 1) (Border :: acc) true (numOfActiveSquares + incActiveCount) }
        getNext p 0 [] false 0

    seq {
        for r in -1..boardDimension - 1 do
            for c in -1..boardDimension - 1 do
                for inc in [ (fun p -> fst p, snd p + 1)
                             (fun p -> fst p + 1, snd p)
                             (fun p -> fst p + 1, snd p + 1)
                             (fun p -> fst p + 1, snd p - 1) ] do
                    yield! generateSeqOfIndices 5 7 (r, c) inc }

let (>>=) m cont = Option.bind cont m

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
    
let log s = System.Diagnostics.Debug.WriteLine s

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

let matchThreatOld s =
    seq {
        yield! threatPatterns |> List.choose (matchThreatPattern s)
        yield! threatPatterns |> List.map (fun (k, s) -> k, List.rev s) |> List.choose (matchThreatPattern s)
        } |> Seq.toList

let matchThreatNew s =
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
    //log (sprintf "start matching l=%i" (Seq.length s))
    traverseTree s threatPatternTree [] [] [] |> Seq.toList

let matchThreat = matchThreatNew

let identifyThreats player sequences =
    let toThreatPatternSquare = function
        | InBoardSquare (Occupied pl, p) when pl = player -> Rest, p
        | InBoardSquare (Occupied _, p) -> Obstacle, p
        | InBoardSquare (Empty, p) -> Available, p
        | Border -> Obstacle, (-1, -1)
    let convertAndMatch s = s |> List.map toThreatPatternSquare |> matchThreat
    let runIfNone cont opt = 
        match opt with
        | Some _ -> opt
        | _ -> cont()
    seq {
        for s in sequences do
            yield! convertAndMatch s }

let identifyThreatsUnconstrained player board =
    let hasEnoughOccupiedCells l s =
        let folder c el = match el with | InBoardSquare (Occupied _, _) -> c + 1 | _ -> c
        List.fold folder 0 s >= l
    board 
        |> enumerateSequences 
        |> Seq.where (hasEnoughOccupiedCells 2) 
        |> identifyThreats player

let isWinningThreat = function | Five | StraightFour -> true | _ -> false

let significantSquareDistance = 2

let buildThreatsTree player board maxDepth =
    let isDependentOrClose parent child =
        match parent with
        | Some (_, data) ->
            if child.Rest |> List.exists (fun p -> p = data.Gain) then true
            else
                match getLinearDictance child.Gain data.Gain with
                | Some d -> d < significantSquareDistance
                | _ -> false
        | _ -> true
    let rec buildNextLevel board depth threat =
        let extend board threatData =
            let folder acc el = Board.extend el (next player) acc
            threatData.Cost |> List.fold folder (Board.extend threatData.Gain player board)
        if depth = maxDepth then None
        elif (match threat with | Some (kind, _) when isWinningThreat kind -> true | _ -> false) then None
        else
            let threats = identifyThreatsUnconstrained player board |> Seq.toList
            let connectedThreats = 
                threats
                |> Seq.where (snd >> isDependentOrClose threat)
                |> Seq.toList
            threats
                |> Seq.where (snd >> isDependentOrClose threat)
                |> Seq.map (
                    fun t ->
                        { Threat = t; 
                          Dependencies = buildNextLevel (extend board (snd t)) (depth + 1) (Some t) })
                |> Seq.toList
                |> Some
    buildNextLevel board 0 None

let analyzeTree tree =
    let rec countMoves node =
        match node.Threat with
        | Five, _ -> Some 0
        | StraightFour, _ -> Some 1
        | _ ->
            match node.Dependencies with
            | Some dep -> 
                let sortedWinningPaths = dep |> List.choose countMoves |> List.sortBy (fun t -> -t)
                if List.isEmpty sortedWinningPaths then None
                else Some sortedWinningPaths.Head
            | None -> None
    
    let goodNodes = 
        tree    
            |> List.map (fun n -> n, countMoves n) 
            |> List.filter (snd >> Option.isSome)
            |> List.sortBy (fun (t, c) -> -(Option.get c))
    match goodNodes with
    | (node, _) :: _ -> let (_, data) = node.Threat in Some data.Gain
    | _ -> None
