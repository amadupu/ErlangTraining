


-module(client).
-behavior(gen_fsm).
-compile(export_all).

-include("tables.hrl").

start(MPid) ->
    gen_fsm:start_link(?MODULE,MPid,[]).

init(MPid = _Args) ->
   process_flag(trap_exit,true),
   {ok, idle, MPid, 30000}.

idle({register,Id,Name},State) ->
  dbservice:add_user(Id,Name,self()),
  {next_state, registered, State, 30000};


idle(timeout,State) ->
  {stop,normal,State};

idle(_Request,MPid=State)->
   MPid ! {send, 400, bad_request},
   {next_state,registered,State,30000}.


registered({client, From, _To, _Message} = Request,MPid = State) ->
   case dbservice:is_user_exist(From) of
     false -> 
        io:format("~p Invalid User: ~p",[?MODULE,From]),
        MPid ! {send, 404, user_not_found};
     true -> 
        gen_server:cast(?CONTROLLER,Request)
   end,
   {next_state,registered,State,30000};

registered({group, From,_Message} = Request,MPid = State) ->
   case dbservice:is_user_exist(From) of
     true -> 
        gen_server:cast(?CONTROLLER,Request);
     false -> 
       io:format("~p Invalid User: ~p",[?MODULE,From]),
       MPid ! {send, 404, user_not_found}
   end,
   {next_state,registered,State,30000};


registered({send_cli_msg, From, Message} = _Request,MPid = State) ->
   MPid ! {send, From, Message}, 
   {next_state,registered,State,30000};

registered({send_grp_msg, From, Message} = _Request,MPid = State) ->
   MPid ! {send, From, Message}, 
   {next_state,registered,State,30000};

registered(timeout,State) ->
   {stop,normal,State};

registered(_Request,MPid=State)->
   MPid ! {send, 400, bad_request},
   {next_state,registered,State,30000}.



code_change(_OldVsn,StateName,StateData,_Extra) -> {ok, StateName,StateData}.

handle_event(_Event,StateName,StateData) -> {next_state, StateName, StateData, 30000}.

handle_sync_event(_Event,_From, StateName,StateData) -> {next_state, StateName, StateData, 30000}.

handle_info(_Info,StateName,StateData) -> {next_state, StateName, StateData, 30000}.

terminate(_Reason, _StateName, _StateData) -> ok.
