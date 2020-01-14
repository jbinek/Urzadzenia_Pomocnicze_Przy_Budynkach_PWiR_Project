-module(alarm).
-export([start/0, stop/0]).

% alarm symuluje zachowanie alarmu, powiadamia o wlamaniu kiedy czujnik wykrywa ruch w momencie kiedy drzwi sa zakmniete (18-6)
% powiadamia o falszywm alarmie kiedy wykrycie nastapi w godzinach pracy (7-17)

port() -> 8084.
id() -> alarm.

% START
% Rejestruje id alarmu w centrum kontroli na serwerze i wysyła dane do okienka w GUI

start() ->
    try
        io:format("Alarm uruchamia sie, ID = ~p ~n", [id()]),
        czujniki_UDP:register(centrum_kontroli:address(), centrum_kontroli:port(), id(), port()),
        Wx=wx:new(),
        Frame=wxFrame:new(Wx, -1, "POWIADOMIENIE!"),
        wxFrame:setBackgroundColour(Frame, {30,144,255}),
        %wxFrame:show(Frame),
        kontroler_pid:register(id(), self()),
        nasluchuj(Frame),
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

% NASLUCHUJ
% Czeka na informację o pokazaniu okienka w GUI

nasluchuj(Frame) ->
    case klient_UDP:nasluchuj(port()) of
        {_, _, Message} ->
            %io:format("Pokazanie okienka: ~p ~n", [Message]),
            io:format("Pojawienie sie okienka z POWIADOMIENIEM: ~n"),
            D = wxMessageDialog:new (Frame, "POWIADOMIENIE: " ++ Message),
            wxMessageDialog:showModal (D);
        _ ->
            nil
    end,
    nasluchuj(Frame).
