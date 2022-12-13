module Day13

open System.IO
open FSharpPlus
open FParsec

let input = File.ReadLines("./13/input.txt")

type PacketData =
    | Int of int
    | List of PacketData list

let packetParser, packetParserRef = createParserForwardedToRef ()

let plist =
    between (pstring "[") (pstring "]") (sepBy packetParser (pstring ",") |>> PacketData.List)

let pint = pint32 |>> PacketData.Int

packetParserRef := pint <|> plist

let parsePacket (input: string) : PacketData =
    match run packetParser input with
    | Success(result, _, _) -> result
    | Failure(errorMsg, _, _) -> failwith $"Invalid input {input}. Error: {errorMsg}"

let packets =
    input
    |> Seq.filter (fun l -> l <> "")
    |> Seq.map parsePacket
    |> Seq.chunkBySize 2
    |> Seq.map (fun packets -> packets[0], packets[1])
    |> Seq.toList

let rec isInRightOrder (left: PacketData) (right: PacketData) : bool option =
    match left, right with
    | Int left, Int right when left < right -> Some true
    | Int left, Int right when left > right -> Some false
    | Int left, Int right when left = right -> None
    | List(leftHead :: leftTail), List(rightHead :: rightTail) ->
        match isInRightOrder leftHead rightHead with
        | Some result -> Some result
        | None -> isInRightOrder (PacketData.List leftTail) (PacketData.List rightTail)
    | List [], List [] -> None
    | List [], List(rightHead :: rightTail) -> Some true
    | List(leftHead :: leftTail), List [] -> Some false
    | Int left, right -> isInRightOrder (PacketData.List [ Int left ]) right
    | left, Int right -> isInRightOrder left (PacketData.List [ Int right ])

module Part1 =
    let result () =
        let results =
            packets
            |> Seq.mapi (fun i packets -> i + 1, isInRightOrder (fst packets) (snd packets))
            |> Seq.cache

        results
        |> Seq.filter (fun (i, result) -> result.IsSome && result.Value)
        |> Seq.sumBy fst

module Part2 =
    let divider1 = PacketData.List [ PacketData.List [ PacketData.Int 2 ] ]
    let divider2 = PacketData.List [ PacketData.List [ PacketData.Int 6 ] ]

    let allPackets =
        packets
        |> Seq.collect (fun (left, right) -> [ left; right ])
        |> Seq.append [ divider1; divider2 ]
        |> Seq.cache

    let compare left right =
        match isInRightOrder left right with
        | Some true -> -1
        | Some false -> 1
        | None -> failwith $"No result of comparing packets"

    let sorted = allPackets |> Seq.sortWith compare |> Seq.toList

    let result () =
        (1 + (sorted |> List.findIndex ((=) divider1)))
        * (1 + (sorted |> List.findIndex ((=) divider2)))

let solve =
    { PartOne = Part1.result
      PartTwo = Part2.result }
