namespace Interleaver.Tests

open FsCheck
open FsCheck.Xunit

module PropertyTests =

  open Interleaver.Core.EventSourcing
  open Interleaver.Core.Interleave

  [<Property(Verbose = true)>]
  let ``Interleaved stream should contain at least as many elements as original stream`` 
    (source: Event list) (foreign: Event list) (fitness: Event -> bool) =
      List.length (interleave source foreign fitness) >= List.length source


  [<Property(Verbose = true)>]
  let ``Interleaved stream should contain X additional events where X is number of fitting foreign events`` 
    (source: Event list) (foreign: Event list) (fitness: Event -> bool) =
      let nbFittingEvents = 
        foreign 
        |> List.filter fitness
        |> List.length
      List.length (interleave source foreign fitness) =
        (List.length source) + nbFittingEvents

  [<Property(Verbose = true, MaxTest = 10)>]
  let ``Reconciled stream should contain at least as many elements as original stream`` 
    (source: Event list) (foreign: Event list list) (fitness: Event -> bool) =
      List.length (reconcile source foreign fitness) >= List.length source