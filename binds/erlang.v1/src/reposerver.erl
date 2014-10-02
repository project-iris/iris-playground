-module(reposerver).
-export([main/0]).

-behaviour(iris_server).
-export([init/2, handle_broadcast/2, handle_request/3, handle_tunnel/2,
    handle_drop/2, terminate/2]).

% Implement the startup and cleanup methods of iris_server.
init(_Conn, Id)            -> {ok, Id}.
terminate(_Reason, _State) -> ok.

main() ->
    % Generate a random ID for the web server
    random:seed(erlang:now()),
    Id = random:uniform(100),

    % Register a micro-service instance into the Iris network
    {ok, Server} = iris_server:start(55555, "repository", ?MODULE, Id),

    io:format("Waiting for inbound tunnels...~n"),
    lists:foreach(fun(Left) ->
    	io:format("~p seconds left till terminate...~n", [Left]),
	    timer:sleep(1000)
	   end, lists:seq(60, 1, -1)),

    % Unregister the service
    ok = iris_server:stop(Server).

% Remaining callbacks methods, not used in this demo
handle_broadcast(_Message, State)      -> {stop, not_implemented, State}.
handle_request(_Request, _From, State) -> {stop, not_implemented, State}.
handle_drop(_Reason, State)            -> {stop, not_implemented, State}.

%% START OMIT
% Callback of iris_server, invoked when a tunnel is inbound
handle_tunnel(Tunnel, Id) -> // HLtunnel
    % Fetch the file name
    {ok, Name} = iris_tunnel:recv(Tunnel, 1000), // HLtunnel

    % Simulate sending some multi-part data stream
    lists:foreach(fun(Part) ->
        Piece  = io_lib:format("Erlang repo #~p: <~s> part #~p", [Id, Name, Part]),
        Binary = iolist_to_binary(Piece),

        ok = iris_tunnel:send(Tunnel, Binary, 1000) // HLtunnel
    end, lists:seq(1, 10)),

    ok = iris_tunnel:close(Tunnel), // HLtunnel
    {noreply, Id}.
%% END OMIT
