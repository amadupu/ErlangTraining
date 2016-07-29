
-module(server).
-compile(export_all).


start() ->
    spawn(server,init,[]).


init() ->
    register(myserver,self()),
    State = getChannels(),
    loop(State).

getChannels() ->
    lists:seq(1,100).

loop(State) ->
   receive
     {From,alloc} -> 
        {Ret,NewState} = alloc(State),
        From ! Ret,
        loop(NewState);
     {free,ChannelId} ->
        NewState = free(ChannelId,State),
        loop(NewState)
   end.



alloc([] = State) -> { no_more_channels,State };

alloc([H|T] = _State) -> { H,T }.


free(Id,State) -> [Id|State]. 


alloc_channel() -> 
     myserver ! { self(), alloc },
     receive
       Any -> io:format("Allocated Id: ~p~n",[Any])
     end.
    
free_channel(Id) ->
     myserver ! { free, Id }.




