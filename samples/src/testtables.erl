
-module(testtables).
-compile(export_all).

-include("testrecords.hrl").
-include_lib("stdlib/include/qlc.hrl").

install(Nodes) ->
   %% create schema
   mnesia:create_schema(Nodes),

   %% start mnesia
   rpc:multicall(Nodes,application,start,[mnesia]),

   %% create tables
   
   mnesia:create_table(user, [{attributes,record_info(fields,user)}, {record_name, user}, {access_mode, read_write}, 
                              {disc_copies,   Nodes}, {type,set}]),

   mnesia:create_table(expertise, [{attributes,record_info(fields,expertise)}, {record_name, expertise}, 
                                   {access_mode, read_write}, {disc_copies,   Nodes}, {type,set}]),
   %% stop tables

   rpc:multicall(Nodes,application,stop,[mnesia]).



uninstall(Nodes) ->
   rpc:multicall(Nodes,application,stop,[mnesia]),
   catch (mnesia:delete_schema(Nodes)). 




start(Nodes) ->
   rpc:multicall(Nodes,application,start,[mnesia]),
   ok = mnesia:wait_for_tables([user,expertise],5000).



%% add a records

add(Record) when is_record(Record,user) ->
   F = fun() ->
       mnesia:write(user,Record,write)
   end,
   {atomic,_Val} = mnesia:transaction(F);


add(Record) when is_record(Record,expertise) ->
   F = fun() ->
       mnesia:write(expertise,Record,write)
   end,
   {atomic,_Val} = mnesia:transaction(F).

%% read a record


get_user(Id) ->
   Oid = {user,Id},
   F = fun() ->
      mnesia:read(Oid)
   end,
   {atomic,_Val} = mnesia:transaction(F).



%% read expertise

get_expertise(Id) ->
   Oid = {expertise,Id},
   F = fun() ->
      mnesia:read(Oid)
   end,
   {atomic,_Val} = mnesia:transaction(F).
   


read_user_table() ->
   F = fun() ->
     qlc:eval(qlc:q([{Rec#user.id , Rec#user.name}|| Rec <- mnesia:table(user)]))
   end,
   {atomic,Val} = mnesia:transaction(F),
   Val.


display_table(Name) -> 
   F = fun() ->
     qlc:eval(qlc:q([Rec || Rec <- mnesia:table(Name)]))
   end,
   {atomic,Val} = mnesia:transaction(F),
   Val.

read_user_table(Expertise) ->
   F = fun() ->
     qlc:eval(qlc:q([{Rec#user.id , Rec#user.name}|| Rec  <- mnesia:table(user), 
                                                     Rec1 <- mnesia:table(expertise),
                                                     Rec1#expertise.id =:= Rec#user.id,
                                                     Rec1#expertise.expertise =:= Expertise]))
   end,
   {atomic,Val} = mnesia:transaction(F),
   Val.
  
delete(Id) ->
   F = fun() ->
      mnesia:delete(user,Id),
      mnesia:delete(expertise,Id)
   end,
   {atomic,_Val} = mnesia:transaction(F).
   
