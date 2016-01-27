namespace Interleaver.Core

module EventSourcing =

  type EventId = System.Guid
  type PlayerId = System.Guid
  type AggregateId = System.Guid

  type Aggregate = { id: AggregateId }
    with static member Zero = { id = System.Guid.NewGuid() }

  type EventData = { id: EventId; playerId: PlayerId; aggregateId: AggregateId }
  type Event =
    | SomeEvent of EventData

  type Command = Unit
  type CommandResult =
    | OK
    | KO

  type StreamFetcher = AggregateId -> Event list
  type StreamSaver = Aggregate -> Unit

  let applyEvent (aggregate: Aggregate) (event: Event) =
    aggregate

  let commandHandler
    (aggregateId: AggregateId)
    (command: Command)
    (streamFetcher: StreamFetcher)
    (streamSaver: StreamSaver) =

    let eventStream = streamFetcher aggregateId
    let currentAggregateState =
      eventStream
      |> List.fold applyEvent Aggregate.Zero
    let event = 
      match command with
      | _ -> SomeEvent({ id = System.Guid.NewGuid(); playerId = System.Guid.NewGuid(); aggregateId = System.Guid.NewGuid()})
    applyEvent currentAggregateState event |> streamSaver
    OK