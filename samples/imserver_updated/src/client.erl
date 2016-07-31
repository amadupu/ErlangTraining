


-module(client).
-behavior(gen_fsm).
-compile(export_all).

-include("tables.hrl").

start(MPid) ->
    error_logger:info_msg("~p: Starting fsm ",[?MODULE]),
    gen_fsm:start_link(?MODULE,MPid,[]).

init(MPid = _Args) ->
   error_logger:info_msg("~p: Starting init ",[?MODULE]),
   process_flag(trap_exit,true),
   error_logger:info_msg("~p: Ending init ",[?MODULE]),
   {ok, idle, MPid, 300000}.

idle({register,Id,Name} = _Request,MPid = State) ->
  error_logger:info_msg("~p: Received Message: ~p~n ",[?MODULE,_Request]),
  dbservice:add_user(Id,Name,self()),
  MPid ! {send, 200, register_success},
  {next_state, registered, State, 300000};


idle(timeout,State) ->
  error_logger:info_msg("~p: Idle Timeout ~n ",[?MODULE]),
  {stop,normal,State};

idle(_Request,MPid=State)->
   error_logger:info_msg("~p: Idle Bad Request~n",[?MODULE]),
   MPid ! {send, 400, bad_request},
   {next_state,registered,State,300000}.


registered({client, From, _To, _Message} = Request,MPid = State) ->
   error_logger:info_msg("~p: Registered Received Message: ~p~n ",[?MODULE,Request]),
   case dbservice:is_user_exist(From) of
     false -> 
        error_logger:info_msg("~p: Invalid User: ~p~n ",[?MODULE,From]),
        MPid ! {send, 404, user_not_found};
     true -> 
        error_logger:info_msg("~p: Forwarding request to controller: ~p~n ",[?MODULE,Request]),
        gen_server:cast(?CONTROLLER,Request)
   end,
   {next_state,registered,State,300000};

registered({group, From,_Message} = Request,MPid = State) ->
   error_logger:info_msg("~p: Registered Received Message: ~p~n ",[?MODULE,Request]),
   case dbservice:is_user_exist(From) of
     true -> 
        error_logger:info_msg("~p: Forwarding request to controller: ~p~n ",[?MODULE,Request]),
        gen_server:cast(?CONTROLLER,Request);
     false -> 
       error_logger:info_msg("~p: Invalid User: ~p~n ",[?MODULE,From]),
       MPid ! {send, 404, user_not_found}
   end,
   {next_state,registered,State,300000};


registered({send_cli_msg, From, Message} = _Request,MPid = State) ->
   error_logger:info_msg("~p: Client Response: ~p~n ",[?MODULE,_Request]),
   MPid ! {send, From, Message}, 
   {next_state,registered,State,300000};

registered({send_grp_msg, From, Message} = _Request,MPid = State) ->
   error_logger:info_msg("~p: Client Response: ~p~n ",[?MODULE,_Request]),
   MPid ! {send, From, Message}, 
   {next_state,registered,State,300000};

registered(timeout,State) ->
   error_logger:info_msg("~p: Registered timout~n ",[?MODULE]),
   {stop,normal,State};

registered(_Request,MPid=State)->
   error_logger:info_msg("~p: Registered 400~n ",[?MODULE]),
   MPid ! {send, 400, bad_request},
   {next_state,registered,State,300000}.



code_change(_OldVsn,StateName,StateData,_Extra) -> {ok, StateName,StateData}.

handle_event(_Event,StateName,StateData) -> {next_state, StateName, StateData, 300000}.

handle_sync_event(_Event,_From, StateName,StateData) -> {next_state, StateName, StateData, 300000}.

handle_info(_Info,StateName,StateData) -> {next_state, StateName, StateData, 300000}.

terminate(_Reason, _StateName, _StateData) -> ok.
