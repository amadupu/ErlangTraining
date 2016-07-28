
-module(client).
-compile(export_all).


%%

start() ->
    Pid = self(),
    register(server, spawn(fun() -> loop(Pid) end)).



loop(Pid) ->
    io:format("Server Spawned with Pid: ~p~n",[self()]),
    receive
       {Pid, stop} -> stopped,
                      Pid ! stop_success;
       Any -> io:format("Server received: ~p~n",[Any]),
              Pid ! received_message,
              loop(Pid)
    end.


rpc(Msg) ->
   server ! {self(), Msg },
   receive
     Any -> io:format("Client received: ~p~n",[Any])
   end.
  
