
-module().
-behavior(gen_server).
-compile(export_all).


start() ->
   gen_server:start_link({local,?MODULE},?MODULE,[],[]).

init(_Args) ->
   {ok, []}.

handle_call(_Request,_From,_State) -> {reply,_From,_State}.

handle_cast(stop,State) -> {stop,normal,State};

handle_cast(_Request,_State) -> {noreply,_State}.

teminate(_Reason,State) -> ok.

code_change(_OldVsn,State,_Extra) -> {noreply,State}.

handle_info(Request,State) -> 
  {noreply,State}.

