-module(rudy).
-author("Henry Bogaeus & Simon Carlson").

%% API
-export([start/1, stop/0]).

start(Port) ->
  register(rudy, spawn(fun() -> init(Port) end)).

stop() ->
  exit(whereis(rudy), "time to die").

init(Port) ->
  Opt = [list, {active, false}, {reuseaddr, true}],
  case gen_tcp:listen(Port, Opt) of
    {ok, Listen} ->
      handler(Listen),
      gen_tcp:close(Listen),
      ok;
    {error, _Error} ->
      error
  end.

handler(Listen) ->
  io:format("Rudy: Listening...~n"),
  case gen_tcp:accept(Listen) of
    {ok, Client} ->
      io:format("Rudy: Client ~w found!~n", [Client]),
      request(Client),
      handler(Listen);
    {error, Error} ->
      io:format("Rudy: Error: ~w~n", [Error]),
      error
  end.

request(Client) ->
  Recv = gen_tcp:recv(Client, 0),
  case Recv of
    {ok, Str} ->
      Request = http:parse_request(Str),
      Response = reply(Request),
      gen_tcp:send(Client, Response);
    {error, Error} ->
      io:format("Rudy: Error: ~w~n", [Error])
  end,
  gen_tcp:close(Client),
  io:format("Rudy: Connection closed.~n").

reply({{get, URI, _}, _, _}) ->
  http:ok(URI).
