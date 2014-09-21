-module(portalchell).
-export([main/0]).

%% START OMIT

-behavior(iris_topic). // HLsub
-export([init/1, handle_event/2, terminate/2]).

%% Implement the startup and cleanup methods of iris_topic.
init(nil)                  -> {ok, nil}. // HLsub
terminate(_Reason, _State) -> ok.        // HLsub

%% Print all events arriving on a subscription
handle_event(Event, State) -> // HLsub
    io:format("~s~n~n", [Event]),
    {noreply, State}.

main() ->
    {ok, Client} = iris_client:start(55555),

    io:format("Tuning in to Aperture channels...~n"),
    iris_client:subscribe(Client, "official", ?MODULE, nil), // HLsub

    timer:sleep(60 * 1000),
    ok = iris_client:stop(Client).
%% END OMIT
