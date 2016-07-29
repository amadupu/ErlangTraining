
-module(server_otp).
-compile(export_all).
-behavior(gen_server).



%% init
init(Args) -> 
   {ok, lists:seq(1,100)}.

%% handle_call
handle_call(alloc,From,State) ->
   {Res, NewState } = getChannel(State),
   {reply,Res,NewState}.

%% handle_cast
handle_cast({free,Id},State) -> 
   NewState = freeChannel(Id,State),
   {noreply,NewState}.

%% handle_info
handle_info(Info,State) -> {noreply,State}.

%% teminate
terminate(Reason,State) -> ok.

%% code_change
code_change(OldVsn,State,Extra) -> {ok,State}.

getChannel([] = State) -> {no_channel,State};
getChannel([H|T]) -> {H, T}.

freeChannel(Id,State) -> [Id|State].


start() ->
   gen_server:start({local, ?MODULE},?MODULE,[],[]).


alloc() ->
   gen_server:call(?MODULE,alloc).


free(Id) ->
   gen_server:cast(?MODULE,{free,Id}).

