
-module(controller).
-behavior(gen_server).
-compile(export_all).

-include("tables.hrl").

start(Port) ->
   gen_server:start_link({local,?MODULE},?MODULE,Port,[]).

init(Port = _Args) ->
   process_flag(trap_exit,true),
   {ok, LSock} = gen_tcp:listen(Port,[binary, {packet,2}, {active,once}, {reuseaddr,true}]),
   spawn(fun() -> accept_handler(LSock) end),
   {ok, []}.


accept_handler(LSock) ->
   case whereis(?MODULE) of
       Pid when is_pid(Pid) -> 
             link(Pid);
       Any -> io:format("~p not registered ~p~n",[?MODULE,Any])
   end,
   gen_server:cast(?MODULE, {change_state,self()}),
   {ok, Socket} = gen_tcp:accept(LSock),
   inet:setopts(Socket,[{active,once}]),
   spawn(fun() -> accept_handler(LSock) end),
   %% create clien fsm
   ClientPid = client:start(self()),
   mm_loop(ClientPid, Socket).
   

mm_loop(ClientPid, Socket) ->
   receive
      {tcp,Socket,Bin} -> 
           ClientMsg = binary_to_term(Bin),
           gen_fsm:send_event(ClientPid,ClientMsg),
           inet:setopts(Socket,[{active,once}]),
           mm_loop(ClientPid,Socket);
      {tcp_closed,Socket} -> 
           io:format("~p: Client Exiting ~p",[?MODULE,ClientPid]);
      {send,From,Msg} = _Req ->
           Bin = term_to_binary({From,Msg}),
           gen_tcp:send(Socket,Bin)
   end.

handle_call(_Request,_From,_State) -> {reply,[] ,_State}.

handle_cast({client,From,To,Msg} = _Request,State) -> 
    case dbservice:get_user(To) of
       undefined ->
          io:format("~p User not found: ~p",[?MODULE, To]);
       Rec -> gen_fsm:send_event(Rec#user.pid,{send_cli_msg, From, Msg})
    end,
    {noreply,State};

handle_cast({group,From,Msg} = _Request,State) ->
    F = fun(Rec) ->
       gen_fsm:send_event(Rec#user.pid,{send_grp_msg, From, Msg})
    end,
    [F(X) || X <- dbservice:get_users(), X#user.id /= From],
    {noreply,State};

handle_cast({change_state, NewState },_State) -> {noreply,NewState};


handle_cast(stop,State) -> {stop,normal,State}.

handle_info({'EXIT',Pid, _Reason} , Pid = State) -> 
  {stop,unknown_error,State};

handle_info({'EXIT',Pid, _Reason} , State) -> 
  io:format("~p Client Exited: ~p~n",[?MODULE,Pid]),
  {noreply,State}.

terminate(_Reason,_State) -> ok.

code_change(_OldVsn,State,_Extra) -> {noreply,State}.


