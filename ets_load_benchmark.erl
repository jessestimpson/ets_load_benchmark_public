-module(ets_load_benchmark).

-export([start/0]).

-define(CollectTime, timer:seconds(5)).

start() ->
    io:format("n,f,r,t~n"),
    Baseline = start(1, undefined),
    Runs = [ trunc(math:pow(2, X)) || X <- lists:seq(1, 8) ],
    [ start(X, Baseline) || X <- Runs ].

start(N, Baseline) ->
    ets:new(?MODULE, [public, named_table]),
    ets:insert(?MODULE, {c, 0}),
    Pids = [ spawn_link(fun() -> worker_loop(idle) end) || _ <- lists:seq(1, N) ],
    M = [ erlang:monitor(process, Pid) || Pid <- Pids ],
    [ erlang:send(Pid, start) || Pid <- Pids ],
    T1 = erlang:monotonic_time(millisecond),
    timer:sleep(?CollectTime),
    [ erlang:send(Pid, stop) || Pid <- Pids],
    T2 = erlang:monotonic_time(millisecond),
    [{c, C}] = ets:lookup(?MODULE, c),
    receive_downs(M),
    Rate = C / ((T2-T1) / 1000),
    Baseline2 = if is_number(Baseline) -> Baseline; true -> Rate end,
    io:format("~p,~p,~p,~p~n", [N, Rate/Baseline2, trunc(Rate), (T2-T1)/1000]),
    ets:delete(?MODULE),
    Rate.

worker_loop(idle) ->
    receive
        start ->
            worker_loop(start);
        stop ->
            ok
    end;
worker_loop(start) ->
    ets:update_counter(?MODULE, c, [{2, 1}]),
    receive
        stop ->
            ok
    after 0 ->
              worker_loop(start)
    end.

receive_downs([]) ->
    ok;
receive_downs([M|Rest]) ->
    receive
        {'DOWN', M, process, _Pid, _Reason} ->
            receive_downs(Rest)
    end.

