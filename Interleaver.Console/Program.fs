// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open Interleaver.Core.Interleave
open Interleaver.Core.EventSourcing

let mutable playerOneEvents: Event list = List.Empty
let mutable playerTwoEvents: Event list = List.Empty
let mutable playerThreeEvents: Event list = List.Empty

let aggregateId = System.Guid.NewGuid()

let sendRandomCommand playerNumber =

  let generateCommand =
    let rnd = System.Random()
    match rnd.Next() with
    | n when n % 2 = 0 -> PublicAction
    | _ -> SecretAction

  let eventStorage = 
    match playerNumber with
    | 1 -> (playerOneEvents, fun newEvent -> playerOneEvents <- newEvent :: playerOneEvents)
    | 2 -> (playerTwoEvents, fun newEvent -> playerTwoEvents <- newEvent :: playerTwoEvents)
    | 3 -> (playerThreeEvents, fun newEvent -> playerThreeEvents <- newEvent :: playerThreeEvents)
    | _ -> failwith "there can only be 3 players in this game"

  commandHandler aggregateId generateCommand (fun _ -> fst(eventStorage)) (snd(eventStorage)) 
  |> ignore

[<EntryPoint>]
let main argv = 
    while true do
      sendRandomCommand 1
      sendRandomCommand 2
      sendRandomCommand 3
    0 // return an integer exit code
