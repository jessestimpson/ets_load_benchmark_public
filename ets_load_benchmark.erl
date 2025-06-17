-module(ets_load_benchmark).

-export([start/0]).

-define(CollectTime, timer:seconds(5)).

start() ->
    Runs = [ trunc(math:pow(2, X)) || X <- lists:seq(1, 8) ],
    [ start(X) || X <- Runs ].

start(N) ->
    ets:new(?MODULE, [public, named_table]),
    ets:insert(?MODULE, {c, 0}),
    Pids = [ spawn_link(fun() -> worker_loop(idle) end) || _ <- lists:seq(1, N) ],
    M = [ erlang:monitor(process, Pid) || Pid <- Pids ],
    [ erlang:send(Pid, start) || Pid <- Pids ],
    timer:sleep(?CollectTime),
    [ erlang:send(Pid, stop) || Pid <- Pids],
    [{c, C}] = ets:lookup(?MODULE, c),
    receive_downs(M),
    io:format("~p => ~p /s~n", [N, C / (?CollectTime / 1000)]),
    ets:delete(?MODULE).

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

