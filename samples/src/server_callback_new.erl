
-module(server_callback_new).
-compile(export_all).



init() -> 
   none.

handle_call(get,State)  ->
   { im_server, State }.

handle_cast(_,State) -> State.

start() ->
   server_behavior:start(?MODULE).

get() ->
   server_behavior:call(?MODULE,get).


