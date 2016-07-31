
-module(client).
-compile(export_all).

start() ->
   Name = Id = arun,
   {ok, Socket} = gen_tcp:connect("10.198.2.63",1234,[binary,{packet,2}]),
   spawn(fun() -> loop({Socket,Id,Name}) end).


loop({Socket,Id,Name} = State) ->
    receive
      {tcp, Socket, Bin} -> 
          io:format("Received: ~p~n",[binary_to_term(Bin)]),
          loop(State);

      {tcp_closed, Socket} ->
          io:format("Server Closed the connection");

      {send, register} -> 
          gen_tcp:send(Socket,term_to_binary({register, Id, Name})),
          loop(State);
      {send, client, To, Message} -> 
          gen_tcp:send(Socket,term_to_binary({client,Id,To, Message})),
          loop(State);
      {send, group,  Message} -> 
          gen_tcp:send(Socket,term_to_binary({group,Id,Message})),
          loop(State);
      Any -> io:format("Recevied Unknown Message: ~p~n",[Any]),
          loop(State)
    end.
  

register(Pid) -> 
    Pid ! {send, register}.
    
sendClientMessage(Pid,To,Message) -> 
    Pid ! {send, client, To, Message}.


sendGroupMessag(Pid,Message) ->
    Pid ! {send, group, Message}.
