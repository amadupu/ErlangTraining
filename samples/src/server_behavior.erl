
-module(server_behavior).

-compile(export_all).

start(Mod) ->
    spawn(?MODULE,init,[Mod]).

call(Name,Req) ->
    Name ! {call, self(), Req},
    receive
      {Name, Res} -> Res
    end.


cast(Name,Req) ->
    Name ! {cast, Req},
    ok.


init(Mod) ->
    register(Mod,self()),
    State = Mod:init(),
    loop(Mod,State).
    


loop(Mod,State) ->
    receive
       {call, From, Req} -> 
           {Res,NewState} = Mod:handle_call(Req,State),
           From ! {Mod, Res},
           loop(Mod,NewState);
       {cast, Req } -> 
           NewState = Mod:handle_cast(Req,State),
           loop(Mod,NewState)
    end.
 


