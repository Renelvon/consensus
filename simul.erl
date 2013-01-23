%% -------------------------------------------------------------
%% Simulation of Consensus Problem in Erlang
%% Protocol: Phase King (original version) from
%%           "Towards Optimal Distributed Consensus, FoCS, 1989"
%%           by <Piotr Berman, Juan A. Garay, Kenneth J. Perry>
%% ---
%% Nick Korasidis    <renelvon@?GMail.com>
%% Panagiotis Aronis <panaro32@hotmail.com>
%% ---
%% Version: 1.1.42
%% -------------------------------------------------------------

-module(simul).
-export([game/0]).
-define(GM, game_master).
-define(DELAY, 5).

%% Swap comments in following lines to enable DEBUG mode.
-define(DEBUG(A, B), ok).
%-define(DEBUG(A, B), io:format(A, B)).

-define(GMText,      "\e[01;36mGM:\e[00m").
-define(LoyalText,   "\e[01;32mLG:\e[00m").
-define(TraitorText, "\e[01;31mTG:\e[00m").

%% -- The Game
game() ->
    register(?GM, self()), % An impartial Game Master
    {A1, A2, A3} = now(),
    random:seed(A1, A2, A3),
    {NumOfGenerals, NumOfTraitors, InitValues} = parse_input(),
    Generals = create_generals(NumOfGenerals),
    ?DEBUG("~s Spawned ~w Generals~n", [?GMText, NumOfGenerals]),
    {Traitors, Loyals} = assign_roles(Generals, NumOfGenerals, NumOfTraitors),
    GMRef = make_ref(), % ?GM has unforgeable identity. No traitor can impersonate him.
    inform_loyal_generals(Loyals, Generals, InitValues, GMRef),
    ?DEBUG("~s All Loyals Informed -- Awaiting Handshakes...~n", [?GMText]),
    ack_loyal_generals(Loyals, GMRef),
    ?DEBUG("~s ... Handshakes Successful~n", [?GMText]),
    inform_traitor_generals(Traitors, Generals, Traitors),
    ?DEBUG("~s All Traitors Informed -- No Handshakes Initiated~n", [?GMText]),
    Rounds = max_inadequate_traitors(NumOfGenerals),
    ?DEBUG("~s Starting Consensus Protocol~n", [?GMText]),
    sync_round({1, 1}, Loyals, Traitors, Rounds, GMRef),
    ?DEBUG("~s All Rounds Completed~n", [?GMText]),
    wait_entrants(terminating, Loyals, GMRef),
    ?DEBUG("~s All Loyals Terminated~n", [?GMText]),
    wait_entrants(terminating, Traitors),
    ?DEBUG("~s All Traitors Terminated~n", [?GMText]),
    io:format("~s Consensus Protocol Terminated~n", [?GMText]).

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
inform_loyal_generals([L | Ls], Generals, [I | Is], GMRef) ->
    L ! {loyal, GMRef, Generals, I},
    inform_loyal_generals(Ls, Generals, Is, GMRef).

ack_loyal_generals([], _GMRef) -> ok;
ack_loyal_generals([L | Ls], GMRef) ->
    receive
        {L, GMRef} -> 
            L ! {ack, GMRef},
            ack_loyal_generals(Ls, GMRef)
    end.

inform_traitor_generals([], _Generals, _Traitors) -> ok;
inform_traitor_generals([T | Ts], Generals, Traitors) ->
    T ! {traitor, Generals, Traitors},
    inform_traitor_generals(Ts, Generals, Traitors).

setup_general() ->
    receive
        {loyal, GMRef, Generals, InitValue} ->
            whereis(?GM) ! {self(), GMRef},
            receive
                {ack, GMRef} ->
                    init_loyal(InitValue, Generals, GMRef)
            end;
        {traitor, Generals, Traitors} ->
            init_traitor(Generals, Traitors)
    end.

%% -- Round Coordination Phase
sync_round(Round, Loyals, Traitors, MaxTraitors, GMRef) ->
    ?DEBUG("~s Syncing Traitors for Round ~w, Phase ~w...~n", [?GMText, element(1, Round), element(2, Round)]),
    wait_entrants({enter_round, Round}, Traitors),
    ?DEBUG("~s ... Traitors synced. Syncing Loyals for Round ~w, Phase ~w...~n", [?GMText, element(1, Round), element(2, Round)]),
    wait_entrants({enter_round, Round}, Loyals, GMRef),
    ?DEBUG("~s ... Loyals synced. Starting Round ~w, Phase ~w~n", [?GMText, element(1, Round), element(2, Round)]),
    start_round(Traitors),
    start_round(Loyals, GMRef),
    NextRound = next_round(Round, MaxTraitors),
    case NextRound of
        none -> ok;
        _ -> sync_round(NextRound, Loyals, Traitors, MaxTraitors, GMRef)
    end.

wait_entrants(_Msg, []) ->
    ?DEBUG("~s All Traitors reported ~w~n", [?GMText, _Msg]);
wait_entrants(Msg, [T | Ts]) ->
    receive
        {T, Msg} ->
            wait_entrants(Msg, Ts)
    % No after clause.
    end.
          
wait_entrants(_Msg, [], _GMRef) ->
    ?DEBUG("~s All Loyals reported ~w~n", [?GMText, _Msg]);
wait_entrants(Msg, [L | Ls], GMRef) ->
    receive
        {L, Msg, GMRef} -> case Msg of
                terminating ->
                    ?DEBUG("~s Loyal General ~w requested termination~n", [?GMText, L]);
                {enter_round, {_R, _E}} ->
                    ?DEBUG("~s Loyal General ~w requested entrance to Round ~w, Phase ~w~n", [?GMText, L, _R, _E])
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
        V == 0 -> io:format("~s [~w] REPORTING: Today we fight! :(~n", [?LoyalText, self()]);
        V == 1 -> io:format("~s [~w] REPORTING: Today we play Tichu!!! :D~n", [?LoyalText, self()])
    end,
    whereis(?GM) ! {self(), terminating, GMRef};

play_loyal(V, RR = {R, 1}, Generals, NumOfGenerals, MaxTraitors, GMRef) ->
    sync(RR, GMRef),
    ?DEBUG("~s [~w] Entered Round ~w, Phase ~w~n", [?LoyalText, self(), R, 1]),
    send_all(V, Generals),
    Votes = collect_all(Generals, 1),
    NewV = decide(Votes, NumOfGenerals - MaxTraitors),
    play_loyal(NewV, {R, 2}, Generals, NumOfGenerals, MaxTraitors, GMRef);

play_loyal(V, RR = {R, 2}, Generals, NumOfGenerals, MaxTraitors, GMRef) ->
    sync(RR, GMRef),
    ?DEBUG("~s [~w] Entered Round ~w, Phase ~w~n", [?LoyalText, self(), R, 2]),
    send_all(V, Generals),
    Votes = collect_all(Generals, 2),
    NewV = decide(Votes, MaxTraitors + 1),
    play_loyal({NewV, Votes}, {R, 3}, Generals, NumOfGenerals, MaxTraitors, GMRef);

play_loyal({V, Votes}, RR = {R, 3}, Generals, NumOfGenerals, MaxTraitors, GMRef) ->
    sync(RR, GMRef),
    ?DEBUG("~s [~w] Entered Round ~w, Phase ~w~n", [?LoyalText, self(), R, 3]),
    King = lists:nth(R,Generals),
    case King == self() of
        true ->
            ?DEBUG("~s [~w] REPORTING: Trust the King and you will never walk alone!~n", [?LoyalText, self()]),
            send_all(V, Generals);
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
    {_D0, _D1, _D2} = collect_all(Loyals, 1), % Empty mailbox.
    send_all(2, Loyals),
    sync({Round, 3}),
    case lists:nth(Round, Generals) =:= self() of
        true ->
            % Confuse them anyway you like!
            ?DEBUG("~s [~w] REPORTING: You fools, you should never trust the King!~n", [?TraitorText, self()]),
            send_all(1, tl(Loyals)),
            hd(Loyals) ! {self(), 0},
            send_all(0, Traitors); % For stupid reasons
        false -> ok
    end,
    receive
        {_King, _KingV} -> ok
    end,
    play_traitor(Round + 1, Generals, Loyals, Traitors, MaxTraitors, MyTraitorIdx). 

play_traitor_nonking(Round, Generals, Loyals, Traitors, MaxTraitors, MyTraitorIdx) ->
    NumOfGenerals = length(Generals),
    sync({Round, 1}),
    {C0, C1, _C2} = collect_all(Loyals, 1), % This 1 is dummy, cause all loyals follow protocol.
    case C1 >= majority(NumOfGenerals) of
        true ->
            King = lists:nth(Round, Generals),
            NBots = super_majority(NumOfGenerals) - length(Traitors),
            Bottable = Loyals -- [King],
            CanHazBots = min(length(Bottable), NBots), % Avoid drawing from empty deck
            {Bots, TempRest} = lists:split(CanHazBots, Loyals -- [King]), % There may be not enough bots...
            Rest = [King | TempRest],
            VRest = vote_no_super(C0, C1, MyTraitorIdx),
            send_all(1, Bots),
            send_all(VRest, Rest),
            sync({Round, 2}),
            {_D0, _D1, _D2} = collect_all(Loyals, 1), % Empty mailbox
            send_all(1, Bots),
            send_all(0, Rest);
        false ->
            V = vote_no_super(C0, C1, MyTraitorIdx),
            send_all(V, Loyals),
            sync({Round, 2}),
            {_D0, _D1, _D2} = collect_all(Loyals, 1), % Empty mailbox
            send_all(2, Loyals) % ... and hope
    end,
    sync({Round, 3}),
    receive
        {_King, _KingV} -> ok % Empty mailbox
    end,
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

super_majority(NumOfGenerals) ->
    NumOfGenerals - max_inadequate_traitors(NumOfGenerals).

majority(NumOfGenerals) ->
    max_inadequate_traitors(NumOfGenerals) + 1.

unfold(N, F) -> unfold(N, F, []).

unfold(0, _F, Acc) -> Acc;
unfold(N, F, Acc) -> unfold(N - 1, F, [F() | Acc]).
