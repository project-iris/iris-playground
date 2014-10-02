-module(behavior).
-export([main/0]).

%% START OMIT
-behaviour(iris_server). // HLbeh
-export([init/2, handle_broadcast/2, handle_request/3, handle_tunnel/2,
    handle_drop/2, terminate/2]).

% Implement the startup and cleanup methods of iris_server.
init(_Conn, your_init_args) -> {ok, your_state}. // HLbeh
terminate(_Reason, _State)  -> ok.               // HLbeh

main() ->
    % Register a micro-service instance into the network
    {ok, Server} = iris_server:start(55555, "Erlang Service", ?MODULE, your_init_args), // HLbeh

    % Unregister the service
    ok = iris_server:stop(Server). // HLbeh

% Remaining callbacks methods, not used in this demo
handle_broadcast(_Message, State)      -> {stop, not_implemented, State}.
handle_request(_Request, _From, State) -> {stop, not_implemented, State}.
handle_tunnel(_Tunnel, State)          -> {stop, not_implemented, State}.
handle_drop(_Reason, State)            -> {stop, not_implemented, State}.
%% END OMIT
