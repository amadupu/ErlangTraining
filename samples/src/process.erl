
-module(process).

-compile(export_all).

max(N) ->
   Max = erlang:system_info(process_limit),
   io:format("Maximum supportred processes: ~p~n",[Max]),
   statistics(runtime),
   statistics(wall_clock),
   
   L = for(0,N,fun() -> spawn(fun() -> wait() end)end),
   {_,Time1} = statistics(runtime),
   {_,Time2} = statistics(wall_clock),
   lists:foreach(fun(Pid) -> Pid ! die end, L),
   U1 = Time1 * 1000/N,
   U2 = Time2 * 1000/N,
   io:format("Process spawn time ~p(~p) microseconds ~n",[U1,U2]).

wait() ->
   receive
     die -> void
   end.


for(Final,Final,_Fun,SoFar) -> lists:reverse(SoFar);
for(Initial,Final,Fun,SoFar) -> for(Initial + 1, Final, Fun, [Fun()|SoFar]).

for(Initial,Final,Fun) -> for(Initial,Final,Fun,[]).



start_process() -> 
    spawn(fun() ->  
            receive
              Any -> io:format("Received message: ~w~n",[Any])
            end
          end
         ).
