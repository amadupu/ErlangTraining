
-module(controller).
-behavior(gen_server).
-compile(export_all).

-include("tables.hrl").

start() ->
   error_logger:info_msg("Starting Controller"),
   gen_server:start_link({local,?MODULE},?MODULE,[],[]).

init(_Args) ->
   process_flag(trap_exit,true),
   {ok, Port} = application:get_env(imserver,port),
   {ok, LSock} = gen_tcp:listen(Port,[binary, {packet,2}, {active,once}, {reuseaddr,true}]),
   spawn(fun() -> accept_handler(LSock) end),
   {ok, []}.


accept_handler(LSock) ->
   case whereis(?MODULE) of
       Pid when is_pid(Pid) -> 
             link(Pid);
       Any -> error_logger:error_msg("~p Not registered. Failed with reason: ~p",[?MODULE,Any])
   end,
   gen_server:cast(?MODULE, {change_state,self()}),
   error_logger:info_msg("Waiting for Client to Connect"),
   {ok, Socket} = gen_tcp:accept(LSock),
   inet:setopts(Socket,[binary, {packet,2} , {active,once}]),
   error_logger:info_msg("Client Connection Accepted"),
   spawn(fun() -> accept_handler(LSock) end),
   %% create clien fsm
   {ok, ClientPid} = client:start(self()),
   error_logger:info_msg("~p client pid ~p~n",[?MODULE,ClientPid]),
   try mm_loop(ClientPid, Socket)
   catch
    Type:Reason -> error_logger:error_msg("Mmloop failed with exception: ~p(~p)~n",[Type,Reason])
   end.
   

mm_loop(ClientPid, Socket) ->
   receive
      {tcp,Socket,Bin} -> 
           ClientMsg = binary_to_term(Bin),
           error_logger:info_msg("~p: Received Message: ~p~n",[?MODULE, ClientMsg]),
           gen_fsm:send_event(ClientPid,ClientMsg),
           error_logger:info_msg("setting opts"),
           inet:setopts(Socket,[{active,once}]),
           error_logger:info_msg("looping again"),
           mm_loop(ClientPid,Socket);
      {tcp_closed,Socket} -> 
           error_logger:info_msg("~p: Client connection closed",[?MODULE]);
      {send,From,Msg} = _Req ->
           error_logger:info_msg("~p: Sending msg to Client: ~p~n ",[?MODULE,{From,Msg}]),
           Bin = term_to_binary({From,Msg}),
           gen_tcp:send(Socket,Bin),
           mm_loop(ClientPid,Socket);
      Any -> error_logger:error_msg("MM loop Unknown Message received ~p~n",[Any]),
           mm_loop(ClientPid,Socket)
   end.

handle_call(_Request,_From,_State) -> {reply,[] ,_State}.

handle_cast({client,From,To,Msg} = _Request,State) -> 
    error_logger:info_msg("~p: Received client Msg: ~p~n",[?MODULE, _Request]),
    case dbservice:get_user(To) of
       undefined ->
          error_logger:error_msg("~p User not found: ~p",[?MODULE, To]);
       Rec -> gen_fsm:send_event(Rec#user.pid,{send_cli_msg, From, Msg})
    end,
    {noreply,State};

handle_cast({group,From,Msg} = _Request,State) ->
    error_logger:info_msg("~p: Received group Msg: ~p~n",[?MODULE, _Request]),
    F = fun(Rec) ->
       gen_fsm:send_event(Rec#user.pid,{send_grp_msg, From, Msg})
    end,
    [F(X) || X <- dbservice:get_users(), X#user.id /= From],
    {noreply,State};

handle_cast({change_state, NewState },_State) -> 
    error_logger:info_msg("~p: Received change_state event~n",[?MODULE]),
    {noreply,NewState};


handle_cast(stop,State) -> 
    error_logger:info_msg("~p: Stopping the controller~n",[?MODULE]),
    {stop,normal,State}.

handle_info({'EXIT',Pid, _Reason} , Pid = State) -> 
  error_logger:info_msg("~p: Received exit..stopping the controller",[?MODULE]),
  {stop,unknown_error,State};

handle_info({'EXIT',_Pid, _Reason} , State) -> 
  error_logger:info_msg("~p: Received exit..ignored",[?MODULE]),
  {noreply,State}.

terminate(_Reason,_State) -> 
  error_logger:info_msg("~p: Calling terminate",[?MODULE]),
  ok.

code_change(_OldVsn,State,_Extra) -> 
  error_logger:info_msg("~p: code change called",[?MODULE]),
  {noreply,State}.


