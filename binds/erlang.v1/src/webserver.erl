-module(webserver).
-export([main/0]).

-behaviour(iris_server).
-export([init/2, handle_broadcast/2, handle_request/3, handle_tunnel/2,
    handle_drop/2, terminate/2]).

% Implement the startup and cleanup methods of iris_server.
init(_Conn, Id)            -> {ok, Id}.
terminate(_Reason, _State) -> ok.

%% START OMIT
%% Format each request a bit and return as the reply
handle_request(Request, _From, Id) -> // HLreq
	Response = <<Id/binary, ": ", Request/binary>>,
	{reply, {ok, Response}, Id}.

main() ->
    % Generate a random ID for the web server
    random:seed(erlang:now()),
    Id = list_to_binary(io_lib:format("erl-www-~p", [random:uniform(100)])),

    % Register a new webserver into the Iris network
    {ok, Server} = iris_server:start(55555, "webserver", ?MODULE, Id), // HLreq

    % Serve a while, then quit
    io:format("Waiting for requests..."),
    timer:sleep(100 * 1000),

    ok = iris_server:stop(Server).
%% END OMIT

% Remaining callbacks methods, not used in this demo
handle_broadcast(_Message, State) -> {stop, not_implemented, State}.
handle_tunnel(_Tunnel, State)     -> {stop, not_implemented, State}.
handle_drop(_Reason, State)       -> {stop, not_implemented, State}.
