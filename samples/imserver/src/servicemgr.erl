
-module(servicemgr).
-compile(export_all).
-behavior(supervisor).


start() ->
   error_logger:info_msg("~p: Staring Supervisor",[?MODULE]),
   supervisor:start_link({local,?MODULE},?MODULE,[]).



init(_Args) ->
  {ok, { {one_for_one, 0, 1},
         [
           {dbservice,  {dbservice,  start,  []} ,      permanent, 5000,        worker, [dbservice]},
           {controller, {controller, start,  []} , 	permanent, brutal_kill, worker, [controller]}
         ] 
       } 
  }.
