%% -------------------------------------------------------------
%% Simulation of Consensus Problem in Erlang
%% Protocol: Phase King (original version) from
%%           "Towards Optimal Distributed Consensus, FoCS, 1989"
%%           by <Piotr Berman, Juan A. Garay, Kenneth J. Perry>
%% ---
%% Nick Korasidis    <renelvon@?GMail.com>
%% Panagiotis Aronis <panaro32@hotmail.com>
%% ---
%% Version: 0.0.2
%% -------------------------------------------------------------

-module(simul).
-export([game/0]).
-define(GM, game_master).
-define(DELAY, 5).

%% Swap comments in following lines to enable DEBUG mode.
%-define(DEBUG(A, B), ok).
-define(DEBUG(A, B), io:format(A, B)).

%% -- The Game
game() ->
    register(?GM, self()), % An impartial Game Master
    {A1, A2, A3} = now(),
    random:seed(A1, A2, A3),
    {NumOfGenerals, NumOfTraitors, InitValues} = parse_input(),
    Generals = create_generals(NumOfGenerals),
    {Traitors, Loyals} = assign_roles(Generals, NumOfGenerals, NumOfTraitors),
    GMRef = make_ref(), % ?GM has unforgeable identity and cannot be impersonated by a traitor
    inform_loyal_generals(Loyals, Generals, InitValues, GMRef),
    inform_traitor_generals(Traitors, Generals, Traitors),
    MaxTraitors = max_inadequate_traitors(NumOfGenerals),
    sync_round({1, 1}, Loyals, Traitors, MaxTraitors, GMRef),
    wait_entrants(terminating, Loyals, GMRef),
    ?DEBUG("Game Master: Loyals Terminated~n", []),
    wait_entrants(terminating, Traitors),
    ?DEBUG("Game Master: Traitors Terminated~n", []),
    io:format("Game Master: Protocol Terminated~n").

%% -- Input Parser
parse_input() ->
    NumOfGenerals = read_int(),
    NumOfTraitors = read_int(),
    InitValues = [read_int() || _I <- lists:seq(1, NumOfGenerals - NumOfTraitors)],
    {NumOfGenerals, NumOfTraitors, InitValues}.

read_int() -> 
    {ok, [Int]} = io:fread("", "~d"),
    Int.

%% -- Preparatory Phase
create_generals(N) -> unfold(N, fun () -> spawn(fun setup_general/0) end).

assign_roles(Generals, NumOfGenerals, NumOfTraitors) ->
    assign_roles(Generals, NumOfGenerals, NumOfTraitors, []).

assign_roles(Gs, _NGs, 0, Acc) -> {Acc, Gs};
assign_roles(Gs, NGs, NTs, Acc) ->
    NewTraitorIdx = random:uniform(NGs),
    G = lists:nth(NewTraitorIdx, Gs),
    assign_roles(Gs -- [G], NGs - 1, NTs - 1, [G | Acc]).

inform_loyal_generals([], _Generals, [], _GMRef) -> ok;
inform_loyal_generals([G | Gs], Generals, [I | Is], GMRef) ->
    G ! {loyal, GMRef, Generals, I},
    inform_loyal_generals(Gs, Generals, Is, GMRef).

inform_traitor_generals([], _Generals, _Traitors) -> ok;
inform_traitor_generals([T | Ts], Generals, Traitors) ->
    T ! {traitor, Generals, Traitors},
    inform_traitor_generals(Ts, Generals, Traitors).

setup_general() ->
    receive
        {loyal, GMRef, Generals, InitValue} ->
            init_loyal(InitValue, Generals, GMRef);
        {traitor, Generals, Traitors} ->
            init_traitor(Generals, Traitors)
    end.

%% -- Round Coordination Phase
sync_round(Round, Loyals, Traitors, MaxTraitors, GMRef) ->
    ?DEBUG("Game Master: @ Barrier before Round ~w, Phase ~w~n", [element(1, Round), element(2, Round)]),
    wait_entrants({enter_round, Round}, Traitors),
    wait_entrants({enter_round, Round}, Loyals, GMRef),
    ?DEBUG("Game Master: Starting Round ~w, Phase ~w~n", [element(1, Round), element(2, Round)]),
    start_round(Traitors),
    start_round(Loyals, GMRef),
    NextRound = next_round(Round, MaxTraitors),
    case NextRound of
        none -> ok;
        _ -> sync_round(NextRound, Loyals, Traitors, MaxTraitors, GMRef)
    end.

wait_entrants(_Msg, []) ->
    ?DEBUG("All Traitors reported ~w~n", [_Msg]);
wait_entrants(Msg, [T | Ts]) ->
    receive
        {T, Msg} ->
            wait_entrants(Msg, Ts)
    % after clause ...
    end.
          
wait_entrants(_Msg, [], _GMRef) ->
    ?DEBUG("All Loyals reported ~w~n", [_Msg]);
wait_entrants(Msg, [L | Ls], GMRef) ->
    receive
        {L, Msg, GMRef} -> case Msg of
                terminating ->
                    ?DEBUG("Game Master: Loyal General ~w requested termination~n", [L]);
                {enter_round, {_R, _E}} ->
                    ?DEBUG("Game Master: Loyal General ~w requested entrance to Round ~w, Phase ~w~n", [L, _R, _E])
        end
            % after clause ...
    end,
    wait_entrants(Msg, Ls, GMRef).

start_round([]) -> ok;
start_round([T | Ts]) ->
    T ! start_round,
    start_round(Ts).

start_round([], _GMRef) -> ok;
start_round([L | Ls], GMRef) ->
    L ! {start_round, GMRef},
    start_round(Ls, GMRef).

next_round({R, 3}, MaxTraitors) when R =< MaxTraitors -> {R + 1, 1};
next_round({R, E}, _MaxTraitors) when E < 3 -> {R, E + 1};
next_round(_R, _MaxTraitors) -> none.

%% -- Player AI
init_loyal(InitValue, Generals, GMRef) ->
    NumOfGenerals = length(Generals),
    MaxTraitors = max_inadequate_traitors(NumOfGenerals),
    play_loyal(InitValue, {1, 1}, Generals, NumOfGenerals, MaxTraitors, GMRef).

play_loyal(V, {R, _E}, _Generals, _NumOfGenerals, MaxTraitors, GMRef)
    when R > MaxTraitors + 1 ->
    if
        V == 0 -> io:format("Loyal General ~w reporting: Today we fight! :(~n", [self()]);
        V == 1 -> io:format("Loyal General ~w reporting: Today we play Tichu!!! :D~n", [self()])
    end,
    whereis(?GM) ! {self(), terminating, GMRef};

play_loyal(V, RR = {R, 1}, Generals, NumOfGenerals, MaxTraitors, GMRef) ->
    ?DEBUG("~w: Awaiting entrance to Round ~w, phase ~w~n", [self(), R, 1]),
    sync(RR, GMRef),
    send_all(V, Generals),
    Votes = collect_all(Generals, 1),
    NewV = decide(Votes, NumOfGenerals - MaxTraitors),
    play_loyal(NewV, {R, 2}, Generals, NumOfGenerals, MaxTraitors, GMRef);

play_loyal(V, RR = {R, 2}, Generals, NumOfGenerals, MaxTraitors, GMRef) ->
    ?DEBUG("~w: Awaiting entrance to Round ~w, phase ~w~n", [self(), R, 2]),
    sync(RR, GMRef),
    send_all(V, Generals),
    Votes = collect_all(Generals, 2),
    NewV = decide(Votes, MaxTraitors + 1),
    play_loyal({NewV, Votes}, {R, 3}, Generals, NumOfGenerals, MaxTraitors, GMRef);

play_loyal({V, Votes}, RR = {R, 3}, Generals, NumOfGenerals, MaxTraitors, GMRef) ->
    ?DEBUG("~w: Awaiting entrance to Round ~w, phase ~w~n", [self(), R, 3]),
    sync(RR, GMRef),
    King = lists:nth(R,Generals),
    case King == self() of
        true -> send_all(V, Generals);
        false -> ok
    end,
    receive
        {King, KingV} -> ok
    after 1000 * ?DELAY ->
            King = 0,
            KingV = 2
    end,
    NewV = case V == 2 orelse element(V + 1, Votes) < NumOfGenerals - MaxTraitors of
        true -> min(1, KingV);
        false -> V
    end,
    play_loyal(NewV, {R + 1, 1}, Generals, NumOfGenerals, MaxTraitors, GMRef).

sync(R) ->
    whereis(?GM) ! {self(), {enter_round, R}},
    receive
        start_round -> ok
    end.
sync(R, GMRef) ->
    whereis(?GM) ! {self(), {enter_round, R}, GMRef},
    receive
        {start_round, GMRef} -> ok
    end.

send_all(_Msg, []) -> ok;
send_all(Msg, [G | Gs]) ->
    G ! {self(), Msg},
    send_all(Msg, Gs).

collect_all(Generals, Default) ->
    collect_all(Generals, Default, 0, 0, 0, 0).

collect_all([], Default, C0, C1, C2, CD) ->
    Votes = {C0, C1, C2},
    DefVotes = element(Default + 1, Votes),
    setelement(Default + 1, Votes, DefVotes + CD);
collect_all([G |Gs], Default, C0, C1, C2, CD) ->
    receive
        {G, 0} ->
            collect_all(Gs, Default, C0 + 1, C1, C2, CD);
        {G, 1} ->
            collect_all(Gs, Default, C0, C1 + 1, C2, CD);
        {G, 2} ->
            collect_all(Gs, Default, C0, C1, C2 + 1, CD)
    after 1000 * ?DELAY ->
            collect_all(Gs, Default, C0, C1, C2, CD + 1)
    end.

decide({C0, C1, _C2}, Bound) ->
    if
        C0 >= Bound -> 0;
        C1 >= Bound -> 1;
        true -> 2
    end.
    
init_traitor(Generals, Traitors) -> 
    Self = self(),
    Loyals = Generals -- Traitors,
    MaxTraitors = max_inadequate_traitors(length(Generals)),
    MyTraitorIdx = length(lists:takewhile(fun (E) -> E =/= Self end, Traitors)),
    play_traitor(1, Generals, Loyals, Traitors, MaxTraitors, MyTraitorIdx).

play_traitor(Round, _Generals, _Loyals, _Traitors, MaxTraitors, _MyTraitorIdx)
    when Round > MaxTraitors + 1 ->
    whereis(?GM) ! {self(), terminating};
    
play_traitor(Round, Generals, Loyals, Traitors, MaxTraitors, MyTraitorIdx) ->
    ?DEBUG("Round: ~w~n Generals: ~w~n Traitors: ~w~n", [Round, Generals, Traitors]),
    case traitor_is_king(Round, Generals, Traitors) of
        true -> play_traitor_king(Round, Generals, Loyals, Traitors, MaxTraitors, MyTraitorIdx);
        false -> play_traitor_nonking(Round, Generals, Loyals, Traitors, MaxTraitors, MyTraitorIdx)
    end.
    
traitor_is_king(Round, Generals, Traitors) ->
    lists:member(lists:nth(Round, Generals), Traitors).

play_traitor_king(Round, Generals, Loyals, Traitors, MaxTraitors, MyTraitorIdx) ->
    sync({Round, 1}),
    {C0, C1, _C2} = collect_all(Loyals, 1), % This 1 is dummy, cause all loyals follow protocol.
    V = vote_no_super(C0, C1, MyTraitorIdx),
    send_all(V, Loyals),
    sync({Round, 2}),
    send_all(2, Loyals),
    sync({Round, 3}),
    case lists:nth(Round, Generals) =:= self() of
        true ->
            % Confuse them anyway you like!
            ?DEBUG("Traitor General ~w reporting: You fools, you should never trust the King!~n", [self()]),
            send_all(0, tl(Loyals)),
            hd(Loyals) ! {self(), 1};
        false -> ok
    end,
    play_traitor(Round + 1, Generals, Loyals, Traitors, MaxTraitors, MyTraitorIdx). 

play_traitor_nonking(Round, Generals, Loyals, Traitors, MaxTraitors, MyTraitorIdx) ->
    King = lists:nth(Round, Generals),
    Bots = lists:nthtail(MaxTraitors, Loyals -- [King]), % There may be not enough bots...
    Rest = Loyals -- Bots,
    sync({Round, 1}),
    {C0, C1, _C2} = collect_all(Loyals, 1), % This 1 is dummy, cause all loyals follow protocol.
    V1 = case C1 >= C0 of true -> 1; false -> 0 end,
    V2 = vote_no_super(C0, C1, MyTraitorIdx),
    send_all(V1, Bots),
    send_all(V2, Rest),
    sync({Round, 2}),
    send_all(V1, Bots),
    send_all(1 - V1, Rest),
    sync({Round, 3}),
    ?DEBUG("Traitor General ~w reporting: Not my fault!~n", [self()]),
    play_traitor(Round + 1, Generals, Loyals, Traitors, MaxTraitors, MyTraitorIdx). 

vote_no_super(C0, C1, Idx) ->
    if
        C0 > C1 andalso Idx + C1 =< C0 -> 1;
        C1 > C0 andalso Idx + C0 =< C1 -> 0;
        true -> Idx rem 2
    end.

%% -- Miscellaneous stuff
max_inadequate_traitors(NumOfGenerals) ->
    (NumOfGenerals + 2) div 3 - 1.

unfold(N, F) -> unfold(N, F, []).

unfold(0, _F, Acc) -> Acc;
unfold(N, F, Acc) -> unfold(N - 1, F, [F() | Acc]).
