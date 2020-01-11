-module(process_manager).
-export([register/2, kill/1, init/0, destroy/0]).

% przechowuje PIDy

% tworzy kontener na PIDy
init() ->
    io:format("init process manager: ~p~n", [self()]),
    ets:new(pids, [set, named_table, public]).

%zapisuje podany proces po kluczu i PID
register(Key, PID) -> ets:insert(pids, {Key, PID}).

% ubija dany proces po kluczu
kill(Key) ->
    PID = element(2, hd(ets:lookup(pids, Key))),
    io:format("Process Manager: ~p is about to kill -> ~p (~p)~n", [self(), PID, Key]),
    exit(PID, stop).

% usuwa cały kontener z PIDami
destroy() -> ets:delete(pids).
