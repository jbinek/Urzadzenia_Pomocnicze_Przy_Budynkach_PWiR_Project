-module(process_manager).
-export([register/2, kill/1, init/0, destroy/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Process manager
%% Stores PIDs to kill if needed
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function: init
%% Purpose: Initiates data container that stores PIDs.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init() -> 
    io:format("init process manager: ~p~n", [self()]),
    ets:new(pids, [set, named_table, public]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function: register
%% Purpose: Registers process of a given Key and PID.
%% Arguments: Key, PID.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
register(Key, PID) -> ets:insert(pids, {Key, PID}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function: kill
%% Purpose: Kills process of a given Key.
%% Arguments: Key
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
kill(Key) -> 
    PID = element(2, hd(ets:lookup(pids, Key))),
    io:format("Process Manager: ~p is about to kill -> ~p (~p)~n", [self(), PID, Key]),
    exit(PID, stop).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function: destroy
%% Purpose: Destroys data container that stores PIDs.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
destroy() -> ets:delete(pids).
