
open System
open System.Threading
open Interleaver.Core.Interleave
open Interleaver.Core.EventSourcing

let mutable playerOneEvents: Event list = List.Empty
let mutable playerTwoEvents: Event list = List.Empty
let mutable playerThreeEvents: Event list = List.Empty

let eventIntervalBracket = (500, 1000)
let synchronizeInterval = 5 * snd(eventIntervalBracket)
let speed = 1

let aggregateId = System.Guid.NewGuid()

let colorizeNumber = function
  | x when x % 2 = 0 -> Console.ForegroundColor <- ConsoleColor.Blue 
  | x when x % 3 = 0 -> Console.ForegroundColor <- ConsoleColor.DarkRed 
  | _ -> Console.ForegroundColor <- ConsoleColor.DarkGreen

let sendRandomCommand playerNumber =

  let rnd = System.Random(DateTime.Now.Millisecond)
  let generateCommand() =  
    match rnd.Next() with
    | n when n % 2 = 0 || n % 3 = 0 -> PublicAction
    | _ -> SecretAction

  let eventStorage() = 
    match playerNumber with
    | 1 -> (playerOneEvents, fun newEvent -> 
      playerOneEvents <- playerOneEvents @ [newEvent])
    | 2 -> (playerTwoEvents, fun newEvent -> 
      playerTwoEvents <- playerTwoEvents  @ [newEvent])
    | 3 -> (playerThreeEvents, fun newEvent -> 
      playerThreeEvents <- playerThreeEvents  @ [newEvent])
    | _ -> failwith "there can only be 3 players in this game"

  let command = generateCommand()
  commandHandler aggregateId command (fun _ -> fst(eventStorage())) (snd(eventStorage())) 
  |> ignore

  printf 
    "%s" 
    (match command with
    | PublicAction -> "P"
    | SecretAction -> "S")

let playerLoop playerNumber =
  async {
  while true do
    async  {
      colorizeNumber playerNumber 
      sendRandomCommand playerNumber
      let rnd = System.Random(DateTime.Now.Millisecond)
      do! Async.Sleep (rnd.Next(fst(eventIntervalBracket) / speed, snd(eventIntervalBracket) / speed))
    }
    |> Async.RunSynchronously
    |> ignore
  }

let synchronizeLoop =
  async {
    let synchronizeAll
      (aggregateId: AggregateId) 
      (fitnessFunction: FitnessFunction) =
      let player1Events = playerOneEvents
      let player2Events = playerTwoEvents
      let player3Events = playerThreeEvents

      Console.ForegroundColor <- ConsoleColor.Gray 
      printfn ""
      printfn "Beginning synchronization..."
      printfn "Synchronizing player1 events..."
      synchronize 
        aggregateId 
        (fun _ -> player1Events)
        ([(fun _ -> player2Events); (fun _ -> player3Events)])
        (fun events -> playerOneEvents <- events)
        fitnessFunction
      |> ignore
    
      printfn "Synchronizing player2 events..."
      synchronize 
        aggregateId 
        (fun _ -> player2Events)
        ([(fun _ -> player1Events); (fun _ -> player3Events)])
        (fun events -> playerTwoEvents <- events)
        fitnessFunction
      |> ignore
      
      printfn "Synchronizing player3 events..."
      synchronize 
        aggregateId 
        (fun _ -> player3Events)
        ([(fun _ -> player2Events); (fun _ -> player1Events)])
        (fun events -> playerThreeEvents <- events)
        fitnessFunction
      |> ignore
     
      playerOneEvents
      |> List.map
        (function
        | SomeSecretEvent e -> "S"
        | SomePublicEvent e -> "P")
      |> List.fold (+) ""
      |> printfn "Resulting player1 interleaved stream : %s"

      playerTwoEvents
      |> List.map
        (function
        | SomeSecretEvent e -> "S"
        | SomePublicEvent e -> "P")
      |> List.fold (+) ""
      |> printfn "Resulting player2 interleaved stream : %s"

      playerThreeEvents
      |> List.map
        (function
        | SomeSecretEvent e -> "S"
        | SomePublicEvent e -> "P")
      |> List.fold (+) ""
      |> printfn "Resulting player3 interleaved stream : %s"

      playerOneEvents <- []
      playerTwoEvents <- []
      playerThreeEvents <- []

      printfn "End of synchronization."

    while true do
      async {
        do! Async.Sleep (synchronizeInterval / speed)
        synchronizeAll 
          aggregateId 
          (function 
          | SomePublicEvent event -> true
          | _ -> false)
        |> ignore
      }
      |> Async.RunSynchronously
      |> ignore
  }
    
[<EntryPoint>]
let main argv = 

  [1; 2; 3]
  |> List.map playerLoop
  |> List.append [synchronizeLoop]
  |> List.map (fun x -> 
    Async.OnCancel(fun () -> printfn "Cancelled !") |> ignore
    x)
  |> Async.Parallel
  |> Async.RunSynchronously
  |> ignore

  0 // return an integer exit code
