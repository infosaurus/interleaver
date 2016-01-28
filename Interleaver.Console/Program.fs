// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open Interleaver.Core.Interleave
open Interleaver.Core.EventSourcing
open System

let mutable playerOneEvents: Event list = List.Empty
let mutable playerTwoEvents: Event list = List.Empty
let mutable playerThreeEvents: Event list = List.Empty

let synchronizeTickMilliseconds = 10000

let aggregateId = System.Guid.NewGuid()

let colorizeNumber = function
  | x when x % 2 = 0 -> Console.ForegroundColor <- ConsoleColor.DarkBlue 
  | x when x % 3 = 0 -> Console.ForegroundColor <- ConsoleColor.DarkRed 
  | _ -> Console.ForegroundColor <- ConsoleColor.DarkGreen

let sendRandomCommand playerNumber =

  let rnd = System.Random()
  let generateCommand =  
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

let playerLoop playerNumber =
  async {
  let rnd = System.Random()
  while true do
    async  {
      colorizeNumber playerNumber
      printfn "player number %d sending command..." playerNumber
      sendRandomCommand playerNumber
      do! Async.Sleep (rnd.Next(100, 1000))
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

      printfn "End of synchronization."

    while true do
      async {
        do! Async.Sleep synchronizeTickMilliseconds
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
  |> Async.Parallel     
  |> Async.RunSynchronously 
  |> ignore

  0 // return an integer exit code
