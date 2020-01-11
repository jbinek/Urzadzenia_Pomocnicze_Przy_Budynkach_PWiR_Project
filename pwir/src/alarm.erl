-module(alarm).
-export([start/0, stop/0]).

% alarm symuluje zachowanie alarmu

port() -> 8084.
id() -> alarm.

% START
% Rejestruje id alarmu w centrum kontroli na serwerze i wysyła dane do okienka w GUI

start() ->
    try
        io:format("Alarm uruchamia się, ID = ~p...~n", [id()]),
        emitter_utils:register(controller:address(), controller:port(), id(), port()),
        Wx=wx:new(),
        Frame=wxFrame:new(Wx, -1, "ALARM!"),
        %wxFrame:show(Frame),
        process_manager:register(id(), self()),
        listen(Frame),
        start
    catch
        _:_ -> io:format("Za dużo procesów przypisanych do jednego czujnika!~n", []),
        error
    end.

% STOP
% Zatrzymuje alarm

stop() ->
    try
        emitter_utils:unregister(controller:address(), controller:port(), id()),
        io:format("Alarm o ID = ~p kończy pracę ~n", [id()]),
        process_manager:kill(id())
    catch
        _:_ -> io:format("Alarm nie jest uruchomiony!~n"),
        error
    end.

% LISTEN
% Czeka na informację o pokazaniu okienka w GUI

listen(Frame) ->
    case consumer_utils:listen(port()) of
        {_, _, Message} ->
            io:format("Pokazanie okienka: ~p ~n", [Message]),
            D = wxMessageDialog:new (Frame, "ALARM!: " ++ Message),
            wxMessageDialog:showModal (D);
        _ ->
            nil
    end,
    listen(Frame).
