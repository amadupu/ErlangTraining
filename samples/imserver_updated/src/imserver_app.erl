
-module(imserver_app).
-behavior(application).
-compile(export_all).

-include("tables.hrl").

start(Type,Args) ->
   error_logger:info_msg("~p Application Started with Type: ~p Args: ~p~n",[?MODULE,Type,Args]),
   mnesia:start(),
   mnesia:wait_for_tables([user],60000),
   
   error_logger:info_msg("~p Starting supervisor",[?MODULE]),
   Ret = servicemgr:start(),
   error_logger:info_msg("~p Application Started Successfuly Ret: ~p~n",[?MODULE,Ret]),
   Ret.


stop(_State) ->
   error_logger:info_msg("~p Application Stopping..: ~p Args: ~p~n",[?MODULE,_State]),
   ok.
