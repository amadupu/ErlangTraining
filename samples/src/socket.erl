
-module(socket).
-compile(export_all).


get_url(Host) ->
   {ok,Socket} = gen_tcp:connect(Host,80,[binary,{packet,0}]),
   gen_tcp:send(Socket,"GET /HTTP/1.0\r\n\r\n"),
   loop([]).




loop(SoFar) ->
   receive
     {tcp,_Socket,Bin} -> loop([Bin|SoFar]);
     {tcp_closed,_Socket} -> string:tokens(binary_to_list(list_to_binary(lists:reverse(SoFar))),"\r\n")
   end.
