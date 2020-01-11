-module(alarm).
-export([start/0, stop/0]).

%%%%%%%%%%%%%%%%%%%%%%
%% alarm simulates behavior of house's alarm.
%% Functions: start, stop, listen
%%%%%%%%%%%%%%%%%%%%%%

port() -> 8084.
id() -> alarm.

%%%%%%%%%%%%%%%%%%%%%%
%% Function: start
%% Registers alarm on the server and sends data to show in the dialog box.
%%%%%%%%%%%%%%%%%%%%%%

start() ->
    try
        io:format("Starting alarm with Id: ~p...~n", [id()]),
        emitter_utils:register(controller:address(), controller:port(), id(), port()),
        Wx=wx:new(),
        Frame=wxFrame:new(Wx, -1, "Alarm Frame"),
        %wxFrame:show(Frame),
        process_manager:register(id(), self()),
        listen(Frame),
        start
    catch
        _:_ -> io:format("Single process may handle only one alarm!~n", []),
        error
    end.

%%%%%%%%%%%%%%%%%%%%%%
%% Function: stop
%% Stops the alarm.
%%%%%%%%%%%%%%%%%%%%%%

stop() ->
    try
        emitter_utils:unregister(controller:address(), controller:port(), id()),
        io:format("Alarm which Id is ~p is being turned off ~n", [id()]),
        process_manager:kill(id())
    catch
        error:Reason -> io:format("Error while terminating ~p, alarm: ~p!~n", [self(), Reason]),
        error
    end.

%%%%%%%%%%%%%%%%%%%%%%
%% Function: listen
%% Waits for information to show in dialog box.
%%%%%%%%%%%%%%%%%%%%%%

listen(Frame) ->
    case consumer_utils:listen(port()) of
        {_, _, Message} ->
            io:format("Show alarm dialog: ~p ~n", [Message]),
            D = wxMessageDialog:new (Frame, "ALARM: " ++ Message),
            wxMessageDialog:showModal (D);
        _ ->
            nil
    end,
    listen(Frame).
