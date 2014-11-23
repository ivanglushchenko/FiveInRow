module FiveInRow.Core.Threats

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

type ThreatType =
    // ...xx...
    | Two
    // Line of seven squares of which the three center squares are occupied by the attacker, and the remaining four squares are empty
    // ..ooo..
    // Line of six squares, with three consecutive squares of the four center squares occupied by the attacker, and the remaining three squares empty
    // x.ooo..
    | Three
    // Line of six squares of which the attacker has occupied three non-consecutive squares of the four center squares, while the other three squares are empty
    // .o.oo.
    | BrokenThree
    // xooo..
    | ClosedThree
    //  Line of five squares, of which the attacker has occupied any four, with the fifth square empty
    // xoooo.
    | Four
    // Line of six squares, of which the attacker has occupied the four center squares
    // .oooo.
    | StraightFour

type Threat = ThreatType * ThreatData

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

let isInBounds (r, c) = r >=0 && c >= 0 && r < boardDimension && c < boardDimension

let enumerateSequences board =
    let getInBoardSquare (r, c) =
        InBoardSquare (Board.get r c board, (r, c))

    let generateSeqOfIndices length p inc_p =
        let rec getNext p l acc breakIfOffboard =
            if l = 0 then acc |> List.rev |> Some
            else
                if isInBounds p then
                    getNext (inc_p p) (l - 1) (getInBoardSquare p :: acc) breakIfOffboard
                else
                    if breakIfOffboard then None
                    else getNext (inc_p p) (l - 1) (Border :: acc) true
        getNext p length [] false

    let generateSubsets s =
        let rec takeAllExceptLast xs acc =
            match xs with
            | hd :: nk :: tl -> takeAllExceptLast (nk :: tl) (hd :: acc)
            | _ -> acc |> List.rev
        seq { yield takeAllExceptLast s []; yield List.tail s }

    seq {
        for r in -1..boardDimension - 1 do
            for c in -1..boardDimension - 1 do
                for inc in [(fun p -> fst p, snd p + 1); (fun p -> fst p + 1, snd p); (fun p -> fst p + 1, snd p + 1)] do
                    match generateSeqOfIndices 7 (r, c) inc with
                    | Some s ->
                        yield s
                        yield! generateSubsets s
                    | None -> ()
                () }

let (>>=) m cont = Option.bind cont m

let threatPatterns =
    [
        // b: ...xx..
        // a: .oxxxo.
        Two, [ Available; Cost; Gain; Rest; Rest; Cost; Available ]
        // b: oxxx..
        // a: oxxxxo
        ClosedThree, [Obstacle; Rest; Rest; Rest; Gain; Cost ]
        // b: oxxx..
        // a: oxxxox
        ClosedThree, [Obstacle; Rest; Rest; Rest; Cost; Gain ]
        // b: ..xxx..
        // a: .xxxxo.
        Three, [ Available; Gain; Rest; Rest; Rest; Cost; Available ] ] 

let matchThreatPattern sequence (kind, pattern) =
    let rec matchNext sequence pattern gain cost rest =
        match sequence, pattern with
        | (Rest, p) :: tl_seq,      Rest :: tl_pat     -> matchNext tl_seq tl_pat gain cost (p :: rest)
        | (Available, p) :: tl_seq, Gain :: tl_pat     -> matchNext tl_seq tl_pat (p :: gain) cost rest
        | (Available, p) :: tl_seq, Cost :: tl_pat     -> matchNext tl_seq tl_pat gain (p :: cost) rest
        | (Obstacle, _) :: tl_seq,  Obstacle :: tl_pat -> matchNext tl_seq tl_pat gain cost rest
        | [], [] -> Some { Gain = List.head gain; Cost = cost; Rest = rest }
        | _ -> None
    match matchNext sequence pattern [] [] [] with
    | Some data -> (kind, data) |> Some
    | _ -> None

let matchThreat s =
    let rec matchNext ps =
        match ps with
        | hd :: tl ->
            match matchThreatPattern s hd with
            | Some threat -> Some threat
            | _ -> matchNext tl
        | [] -> None
    matchNext threatPatterns

let identifyThreats player board =
    let toThreatPatternSquare = function
        | InBoardSquare (Occupied pl, p) when pl = player -> Rest, p
        | InBoardSquare (Occupied _, p) -> Obstacle, p
        | InBoardSquare (Empty, p) -> Available, p
        | Border -> Obstacle, (-1, -1)
    seq {
        for s in enumerateSequences board do
            match s |> List.map toThreatPatternSquare |> matchThreat with
            | Some t -> yield t
            | None -> () }
