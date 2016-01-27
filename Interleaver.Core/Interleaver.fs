namespace Interleaver.Core

module Interleave =
  
  open EventSourcing

  let interleave (source:Event list) (foreign: Event list) (fitnessFunction: Event -> bool) =
    let fittingEvents = 
      foreign 
      |> List.filter fitnessFunction
    List.append source fittingEvents

  let reconcile (source:Event list) (foreign: Event list list) (fitnessFunction: Event -> bool) =
    foreign
    |> List.fold (fun acc x -> (interleave acc x fitnessFunction)) source