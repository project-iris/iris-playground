-module(webbrowser).
-export([main/0]).

main() ->
%% START OMIT
% Connect to the Iris network as a simple client
{ok, Client} = iris_client:start(55555), // HLreq

% Issue a dummy request every second
lists:foreach(fun(Index) ->
    Request = iolist_to_binary(io_lib:format("Request #~p", [Index])),
    case iris_client:request(Client, "webserver", Request, 1000) of // HLreq
        {ok, Reply}     -> io:format("Web reply: ~s~n", [Reply]);
        {error, Reason} -> io:format("Request failed: ~s~n", [Reason])
    end,
    timer:sleep(1000)
end, lists:seq(1, 100)),

% Disconnect from the Iris network
ok = iris_client:stop(Client). // HLreq
%% END OMIT
