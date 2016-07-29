
-module(dbservice).
-behavior(gen_server).
-compile(export_all).

-include("tables.hrl").
-include_lib("stdlib/include/qlc.hrl").

start() ->
   gen_server:start_link({local,?MODULE},?MODULE,[],[]).

init(_Args) ->
   process_flag(trap_exit,true),
   mnesia:start(),
   ok = mnesia:wait_for_tables([user],5000),
   {ok, []}.

handle_call(_Request,_From,_State) -> {reply,_From,_State}.

handle_cast({add_user, Id, Name} = _Request,State) ->

   F = fun() ->
     mnesia:write(#user{id=Id,name=Name})
   end,

   mnesia:transaction(F),
   {noreply,State};


handle_cast({del_user, Id} = _Request, State ) ->
   F = fun() ->
      Oid = {user,Id},
      mnesia:delete(Oid)
   end,
   mnesia:transaction(F),
   {noreply,State};

handle_cast(stop,State) -> {stop,normal,State};

handle_cast(_Request,_State) -> {noreply,_State}.

terminate(_Reason,_State) -> application:stop(mnesia).

code_change(_OldVsn,State,_Extra) -> {noreply,State}.

handle_info(Request,State) -> 
  io:format("Received info event for dbservice: ~p~n",[Request]),
  {noreply,State}.

is_user_exist(Id) ->
  F = fun() ->
   qlc:eval(qlc:q([Rec || Rec <- mnesia:table(user), Rec#user.id =:= Id]))
  end,
  {atomic, Value} = mnesia:transaction(F),
  if
    Value =:= [] -> false;
    true -> true
  end.

get_user(Id) ->
  F = fun() ->
   qlc:eval(qlc:q([Rec || Rec <- mnesia:table(user), Rec#user.id =:= Id]))
  end,
  {atomic, Value} = mnesia:transaction(F),
  if
    Value =:= [] -> undefined;
    true -> [Rec] = Value,Rec
  end.
   
get_users() ->
  F = fun() ->
   qlc:eval(qlc:q([Rec || Rec <- mnesia:table(user)]))
  end,
  {atomic, Value} = mnesia:transaction(F),
  Value.

install(Nodes) ->
   %% create schema
   mnesia:create_schema(Nodes),
   %% start mnesia
   rpc:multicall(Nodes,application,start,[mnesia]),
   %% create tables
   mnesia:create_table(user, [{attributes,record_info(fields,user)}, {record_name, user}, {access_mode, read_write},
                              {disc_copies,   Nodes}, {type,set}]),
   %% stop tables
   rpc:multicall(Nodes,application,stop,[mnesia]).

uninstall(Nodes) ->
   rpc:multicall(Nodes,application,stop,[mnesia]),
   catch (mnesia:delete_schema(Nodes)).
