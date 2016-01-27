namespace Interleaver.Core

module EventSourcing =

  type EventId = System.Guid
  type PlayerId = System.Guid
  type AggregateId = System.Guid

  type Aggregate = { id: AggregateId }
    with static member Zero = { id = System.Guid.NewGuid() }

  type EventData = { id: EventId; playerId: PlayerId; aggregateId: AggregateId }
  type Event =
    | SomeSecretEvent of EventData
    | SomePublicEvent of EventData

  type Command =
    | SecretAction
    | PublicAction
  type CommandResult =
    | OK
    | KO

  type StreamFetcher = AggregateId -> Event list
  type EventSaver = Event -> Unit

  let applyEvent (aggregate: Aggregate) (event: Event) =
    aggregate

  let commandHandler
    (aggregateId: AggregateId)
    (command: Command)
    (streamFetcher: StreamFetcher)
    (eventSaver: EventSaver) =

    let eventStream = streamFetcher aggregateId
    let currentAggregateState =
      eventStream
      |> List.fold applyEvent Aggregate.Zero
    let event = 
      match command with
      | SecretAction -> SomeSecretEvent({ id = System.Guid.NewGuid(); playerId = System.Guid.NewGuid(); aggregateId = System.Guid.NewGuid()})
      | PublicAction -> SomePublicEvent({ id = System.Guid.NewGuid(); playerId = System.Guid.NewGuid(); aggregateId = System.Guid.NewGuid()})
    event |> eventSaver
    OK