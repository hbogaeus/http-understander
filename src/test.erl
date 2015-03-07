-module(test).
-author("Henry").

-export([bench/3]).

bench(N, Host, Port) ->
  Start = now(),
  run(N, Host, Port),
  Finish = now(),
  Time = timer:now_diff(Finish, Start),
  io:format("Test: Bench with ~w requests took ~w ms.~n", [N, Time div 1000]).

run(N, Host, Port) when N >= 0 ->
  request(Host, Port),
  run(N - 1, Host, Port);
run(_, _, _) ->
  ok.

request(Host, Port) ->
  Options = [list, {active, false}, {reuseaddr, true}],
  {ok, Server} = gen_tcp:connect(Host, Port, Options),
  gen_tcp:send(Server, http:get("foo")),
  Recv = gen_tcp:recv(Server, 0),
  case Recv of
    {ok, _} ->
      ok;
    {error, Error} ->
      io:format("Test: Error: ~w~n", [Error])
  end,
  gen_tcp:close(Server).