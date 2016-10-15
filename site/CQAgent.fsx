
module CQAgent = 


    type Message<'TState> =
      | Query of ('TState -> unit)
      | Command of ('TState -> 'TState)

    type CQAgent<'TState>(initialState: 'TState) =
      let innerModel =
        MailboxProcessor<Message<'TState>>.Start(fun inbox ->
          let rec messageLoop (state: 'TState) =
            async {
              let! msg = inbox.Receive()
              match msg with
              | Query q ->
                  q state
                  return! messageLoop state
              | Command c ->
                  let newState = c state
                  return! messageLoop(newState)
            }
          messageLoop initialState)

      member this.Query<'T> (q: 'TState -> 'T) =
        innerModel.PostAndReply(fun chan -> Query(fun state ->
          let res = q state
          chan.Reply(res)))

      member this.Command<'T> (c: 'TState -> 'T * 'TState) =
        innerModel.PostAndReply(fun chan -> Command(fun state ->
          let res, newState = c state
          chan.Reply(res)
          newState))