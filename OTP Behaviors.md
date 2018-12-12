# OTP Behavior

OTP behaviros include _worker_ processes, and _supervisors_.

* **Worker**: the acutal processing
* **Supervisor**: monitor workers and other supervisors

# Generic Server

Generic server that implement client/server behaviors are defined in the `gen_server` behavior that comes as part of the standard application.

## Start the server

Call the function `gen_server:start4` and `gen_server_link/4` function as follows:

```erlang
gen_server:start_link(ServerName, CallBackModule, Arguments, Options).
gen_server:start(ServerName, CallBackModule, Arguments, Options).
gen_server:start_link(ServerName, CallBackModule, Arguments).
gen_server:start(ServerName, CallBackModule, Arguments).
```

* ServerName: tuple of format `{local, Name}` or `{global, Name}`.
* CallbackModule: name of the module in which the specific callback functions are palced.
* Arguments: 

## Passing Messages

Use the following calls to send a message to your server:

```erlang
% asynchronous message request
gen_server:cast(Name, Message)
% synchronous message request
gen_server:call(Name, Message)
```

* _Name_: either the local resigered name of the server or the tuple `{global, Name}`. 
* _Message_: A valid Erlang term containing a message passed on to the server

Upon receiving the message, `gen_server` will call the callback function `handle_cast(Message, LoopData)` in the callback module. The `LoopData` is the argument returned by the `init/1` callback function.

## Stoping the server

In your `handle_call/3` or `handle_cast/2` callback function, return `{stop, Reason, Reply, NewLoopData}` or `{stop, Reason, NewLoopData}` respectively. The generic code executes the `terminate(Reason, LoopData)` callback.

The `terminate` function is the place to insert the code needed to clean up the `LoopData` of the server. In the example, we cleaned the `ETS` and `Dets` tables.

```erlang
stop() -> 
    gen_server:cast(?MODULE, stop).

handle_cast(stop, LoopData) -> 
    {stop, normal, LoopData}.

terminate(_Reason, _LoopData) -> 
    usr_db:close_tables().
```

# Example

```erlang
-module(mybank).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0]).
-compile(export_all).
-define(SERVER, ?MODULE).


start() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
stop()  -> gen_server:call(?MODULE, stop).

sum(A, B) -> gen_server:call(?MODULE, {sum, A, B}).
retrieve() -> gen_server:call(?MODULE, {retrieve}).

%% ====================================================================
%% Behavioural functions
%% ====================================================================
-record(state, {}).

%% init/1
%% ====================================================================
%% 
init([]) -> {ok, ets:new(?MODULE, [])}.

%% handle_call/3
%% ====================================================================
%%
handle_call({sum, A, B}, _From, Tab) ->
    Reply = ets:insert(Tab, {result, A+B}),
    {reply, Reply, Tab};

handle_call({retrieve}, _From, Tab) ->
    Reply = case ets:lookup(Tab, result) of
        [] -> no_result;
        [{_, Value}] -> {your, result, is, Value}
        end,
    {reply, Reply, Tab};


handle_call(stop, _From, Tab) ->
    {stop, normal, stopped, Tab}.

%% handle_cast/2
%% ====================================================================
handle_cast(Msg, State) ->
    {noreply, State}.


%% handle_info/2
%% ====================================================================

handle_info(Info, State) ->
    {noreply, State}.


%% terminate/2
%% ====================================================================
terminate(Reason, State) ->
    ok.
```



