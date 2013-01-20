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
    {NumOfGenerals, NumOfTraitors, InitValues} = parse_input(),
    Generals = create_generals(NumOfGenerals),
    {Traitors, Loyals} = assign_roles(Generals, NumOfGenerals, NumOfTraitors),
    GMRef = make_ref(), % ?GM has unforgeable identity and cannot be impersonated by a traitor
    inform_loyal_generals(Loyals, Loyals, InitValues, GMRef),
    inform_traitor_generals(Traitors, Generals, Traitors),
    MaxTraitors = max_inadequate_traitors(NumOfGenerals),
    sync_round({1, 1}, Loyals, Traitors, MaxTraitors, GMRef),
    wait_entrants(terminating, Loyals, GMRef),
   % ?DEBUG("Game Master: Loyals Terminated~n", []),
    wait_entrants(terminating, Traitors),
   % ?DEBUG("Game Master: Traitors Terminated~n", []),
    io:format("Game Master: Protocol Terminated~n").

%% -- Input Parser
parse_input() ->
    NumOfGenerals = read_int(),
    NumOfTraitors = read_int(),
    InitValues = [read_int() || _I <- lists:seq(1, NumOfGenerals)],
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

wait_entrants(_Msg, []) -> ok;
wait_entrants(Msg, [T | Ts]) ->
    receive
        {T, Msg} ->
            wait_entrants(Msg, Ts)
    % after clause ...
    end.
          
wait_entrants(Msg, [], _GMRef) ->
    ?DEBUG("All Loyals reported ~w~n", [Msg]);
wait_entrants(Msg, [L | Ls], GMRef) ->
    receive
        {L, Msg, GMRef} -> case Msg of
                terminating ->
                    ?DEBUG("Game Master: Loyal General ~w requested termination~n", [L]);
                {enter_round, {R, E}} ->
                    ?DEBUG("Game Master: Loyal General ~w requested entrance to Round ~w, Phase ~w~n", [L, R, E])
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
    
init_traitor(Generals, Traitors) -> play_traitor({1, 1}, Generals, Traitors).

play_traitor(NextRound, Generals, Traitors) -> stub.
    
%% -- Miscellaneous stuff
max_inadequate_traitors(NumOfGenerals) ->
    (NumOfGenerals + 2) div 3 - 1.

unfold(N, F) -> unfold(N, F, []).

unfold(0, _F, Acc) -> Acc;
unfold(N, F, Acc) -> unfold(N - 1, F, [F() | Acc]).
