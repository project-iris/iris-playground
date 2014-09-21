-module(repoclient).
-export([main/0]).

%% START OMIT
main() ->
    {ok, Client} = iris_client:start(55555),

    % Open an outbound tunnel to a data store
    case iris_client:tunnel(Client, "Erly Library", 1000) of // HLtunnel
    	{ok, Tunnel}    -> fetch(Tunnel);
    	{error, Reason} -> io:format("Data store connection failed: ~p~n", [Reason])
    end,

    ok = iris_client:stop(Client).

%% Request a file and retrieve the multi-part response
fetch(Tunnel) ->
	iris_tunnel:send(Tunnel, <<"some file">>, 1000), // HLtunnel
	recv(Tunnel).

recv(Tunnel) ->
	case iris_tunnel:recv(Tunnel, 1000) of // HLtunnel
		{ok, Data} -> io:format("~s~n", [Data]), recv(Tunnel);
		_Otherwise -> ok = iris_tunnel:close(Tunnel) // HLtunnel
	end.
%% END OMIT
