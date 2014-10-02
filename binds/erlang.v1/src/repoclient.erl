-module(repoclient).
-export([main/0]).

%% START OMIT
main() ->
    {ok, Client} = iris_client:start(55555),

    % Open an outbound tunnel to a data store
    case iris_client:tunnel(Client, "repository", 1000) of // HLtunnel
        {error, Reason} -> io:format("Tunneling failed: ~p~n", [Reason]);
        {ok, Tunnel}    ->
            % Request a file and retrieve the multi-part response
            iris_tunnel:send(Tunnel, <<"some file">>, 1000), // HLtunnel
            fetch(Tunnel)
    end,
    ok = iris_client:stop(Client).

fetch(Tunnel) ->
	case iris_tunnel:recv(Tunnel, 1000) of // HLtunnel
		{ok, Data} -> io:format("~s~n", [Data]), fetch(Tunnel);
		_Otherwise -> ok = iris_tunnel:close(Tunnel) // HLtunnel
	end.
%% END OMIT
