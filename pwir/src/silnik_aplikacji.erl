-module(silnik_aplikacji).
-export([start/0, stop/0, gui/0]).

% silnik aplikacji

czasUruchomienia() -> 2.
czasZatrzymania() -> 1.

% tworzy wszystkie potrzebne zasoby i uruchamia aplikację

start() ->
    kontroler_pid:init(),

    %centrum kontroli
    CentrumPID = spawn(fun () -> centrum_kontroli:start() end),
    io:format("Run process [Centrum Kontroli]: ~p~n", [CentrumPID]),
    timer:sleep(timer:seconds(czasUruchomienia())),


    %klienci
    AlarmPID = spawn(fun () -> alarm:start() end),
    io:format("Run process [Alarm]: ~p~n", [AlarmPID]),
    timer:sleep(timer:seconds(czasUruchomienia())),

    KlimaPID = spawn(fun () -> klimatyzacja:start() end),
    io:format("Run process [Klimatyzacja]: ~p~n", [KlimaPID]),
    timer:sleep(timer:seconds(czasUruchomienia())),

    RoletyPID = spawn(fun () -> rolety:start() end),
    io:format("Run process [Rolety]: ~p~n", [RoletyPID]),
    timer:sleep(timer:seconds(czasUruchomienia())),

    DrzwiPID = spawn(fun () -> drzwi:start() end),
    io:format("Run process [Drzwi]: ~p~n", [DrzwiPID]),
    timer:sleep(timer:seconds(czasUruchomienia())),

    ZraszaczPID = spawn(fun () -> zraszacz_przeciwpozarowy:start() end),
    io:format("Run process [Zraszacz przeciwpozarowy]: ~p~n", [ZraszaczPID]),
    timer:sleep(timer:seconds(czasUruchomienia())),




    %urzadzenia
    AntywlamPID = spawn(fun () -> czujnik_antywlamaniowy:start() end),
    io:format("Run process [Czujnik antywlamaniowy]: ~p~n", [AntywlamPID]),
    timer:sleep(timer:seconds(czasUruchomienia())),

    TempPID = spawn(fun () -> miernik_temperatury:start() end),
    io:format("Run process [Miernik temperatury]: ~p~n", [TempPID]),
    timer:sleep(timer:seconds(czasUruchomienia())),

    KontrolDrzwiPID = spawn(fun () -> kontroler_drzwi:start() end),
    io:format("Run process [Kontroler drzwi]: ~p~n", [KontrolDrzwiPID]),
    timer:sleep(timer:seconds(czasUruchomienia())),

    DymPID = spawn(fun () -> detektor_dymu:start() end),
    io:format("Run process [Czujnik dymu]: ~p~n", [DymPID]).

%zatrzymuje działanie aplikacji
stop() ->

    %urządzenia
    detektor_dymu:stop(),
    timer:sleep(timer:seconds(czasZatrzymania())),

    miernik_temperatury:stop(),
    timer:sleep(timer:seconds(czasZatrzymania())),

    czujnik_antywlamaniowy:stop(),
    timer:sleep(timer:seconds(czasZatrzymania())),

    kontroler_drzwi:stop(),
    timer:sleep(timer:seconds(czasZatrzymania())),


  %klienci
    alarm:stop(),
    timer:sleep(timer:seconds(czasZatrzymania())),

    klimatyzacja:stop(),
    timer:sleep(timer:seconds(czasZatrzymania())),

    rolety:stop(),
    timer:sleep(timer:seconds(czasZatrzymania())),

    drzwi:stop(),
    timer:sleep(timer:seconds(czasZatrzymania())),

    zraszacz_przeciwpozarowy:stop(),
    timer:sleep(timer:seconds(czasZatrzymania())),



    %centurm kontroli
    centrum_kontroli:stop(),

    kontroler_pid:destroy().


gui() ->
    P_PID = self(),
    Wx=wx:new(),
    Frame=wxFrame:new(Wx, -1, "Urządzenia pomocnicze przy budynkach",[{size,{400, 200}}]),
    wxFrame:setBackgroundColour(Frame,{218,165,32}),
    Panel = wxPanel:new(Frame, [{size, {10, 0}}]),

    StartButton = wxButton:new(Panel, 12, [{label,"URUCHOM"}, {pos, {50, 50}}, {size, {100,100}}]),
    wxButton:connect(StartButton, command_button_clicked, [{callback,
        fun(_, _) -> P_PID ! start end }]),
    wxButton:setBackgroundColour(StartButton,{240,230,140}),

    StopButton = wxButton:new(Panel, 12, [{label,"ZATRZYMAJ"}, {pos, {210, 50}}, {size, {100,100}}]),
    wxButton:connect(StopButton, command_button_clicked, [{callback,
        fun(_, _) -> P_PID ! stop end }]),
   % wxButton:setBackgroundColour(StopButton,{240,230,141}),

    wxFrame:show(Frame),

    awaitStart().

%kontroler GUI
awaitStart() ->
    receive
        start -> start()
    end,
    awaitStop().

%kontroler GUI
awaitStop() ->
    receive
        stop -> stop()
    end,
    awaitStart().