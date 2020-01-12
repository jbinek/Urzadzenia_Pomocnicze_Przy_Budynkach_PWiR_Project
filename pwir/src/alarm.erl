-module(alarm).
-export([start/0, stop/0]).

% alarm symuluje zachowanie alarmu

port() -> 8084.
id() -> alarm.

% START
% Rejestruje id alarmu w centrum kontroli na serwerze i wysyła dane do okienka w GUI

start() ->
    try
        io:format("Alarm uruchamia sie, ID = ~p...~n", [id()]),
        czujniki_UDP:register(centrum_kontroli:address(), centrum_kontroli:port(), id(), port()),
        Wx=wx:new(),
        Frame=wxFrame:new(Wx, -1, "ALARM!"),
        %wxFrame:show(Frame),
        kontroler_pid:register(id(), self()),
        listen(Frame),
        start
    catch
        _:_ -> io:format("Za duzo procesow przypisanych do jednego czujnika!~n", []),
        error
    end.

% STOP
% Zatrzymuje alarm

stop() ->
    try
        czujniki_UDP:unregister(centrum_kontroli:address(), centrum_kontroli:port(), id()),
        io:format("Alarm o ID = ~p konczy prace ~n", [id()]),
        kontroler_pid:kill(id())
    catch
        _:_ -> io:format("Alarm nie jest uruchomiony!~n"),
        error
    end.

% LISTEN
% Czeka na informację o pokazaniu okienka w GUI

listen(Frame) ->
    case klient_UDP:listen(port()) of
        {_, _, Message} ->
            io:format("Pokazanie okienka: ~p ~n", [Message]),
            D = wxMessageDialog:new (Frame, "ALARM!: " ++ Message),
            wxMessageDialog:showModal (D);
        _ ->
            nil
    end,
    listen(Frame).
