
-module(server_callback).
-compile(export_all).



init() -> 
   lists:seq(1,100).

handle_call(alloc,State)  ->
    getChannel(State).
  
handle_cast({free,Id},State) ->
    freeChannel(Id,State).

getChannel([] = State) -> {no_channel,State};

getChannel([H|T]) -> {H, T}.

freeChannel(Id,State) -> [Id|State].


start() ->
   server_behavior:start(?MODULE).


alloc() ->
   server_behavior:call(?MODULE,alloc).


free(Id) ->
   server_behavior:cast(?MODULE,{free,Id}).

