module FiveInRow.Core.Threats

open GameDef

type ThreatData = { Gain: Point        // The gain square of a threat is the square played by the attacker
                    Cost: Point        // The cost squares of a threat are the squares played by the defender, in response to the threat
                    Rest: Point list } // The rest squares of a threat are the squares containing a threat possibility, the gain square excepted

type Threat = 
    | Four of ThreatData
    | StraightFour of ThreatData
    | Three of ThreatData
    | BrokenThree of ThreatData

type SquareThreats = { mutable S: Threat option
                       mutable E: Threat option
                       mutable SE: Threat option
                       mutable SW: Threat option } 

module Position =
    type Position = SquareStatus[][]

    let empty dim = Array.init dim (fun _ -> Array.create dim Empty)

    let get (r, c) position = Array.get (Array.get position r) c

    let set (r, c) position value = Array.set (Array.get position r) c value

    let extend (r, c) player position =
        if get (r, c) position |> isEmpty = false then failwith "Square is already occupied"
        set (r, c) position (Occupied player)
        position
    
let identifyThreats position =
    for r in 0..boardDimension - 1 do
        for c in 0..boardDimension - 1 do
            ()
    []
    