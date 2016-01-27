namespace Interleaver.Core

module Interleave =
  
  open EventSourcing

  type FitnessFunction = Event -> bool
  type StreamSaver = Event list -> Unit

  let interleave (source:Event list) (foreign: Event list) (fitnessFunction: FitnessFunction) =
    let fittingEvents = 
      foreign 
      |> List.filter fitnessFunction
    List.append source fittingEvents

  let reconcile (source:Event list) (foreign: Event list list) (fitnessFunction: FitnessFunction) =
    foreign
    |> List.fold (fun acc x -> (interleave acc x fitnessFunction)) source

  let synchronize 
    (aggregateId: AggregateId) 
    (sourceStreamFetcher: StreamFetcher) 
    (foreignStreamFetchers: StreamFetcher list)
    (streamSaver: StreamSaver)
    (fitnessFunction: FitnessFunction) =

    let sourceStream = sourceStreamFetcher aggregateId
    let foreignStreams =
      foreignStreamFetchers
      |> List.map (fun x -> x aggregateId)
    reconcile sourceStream foreignStreams fitnessFunction 
    |> streamSaver
    OK