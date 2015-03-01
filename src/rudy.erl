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
  log("Rudy: Listening...~n", []),
  case gen_tcp:accept(Listen) of
    {ok, Client} ->
      {ok, {Addr, Port}} = inet:peername(Client),
      log("Rudy: Client ~w on port ~w found!~n", [Addr, Port]),
      request(Client),
      handler(Listen);
    {error, Error} ->
      log("Rudy: Error: ~w~n", [Error]),
      error
  end.

request(Client) ->
  Recv = gen_tcp:recv(Client, 0),
  case Recv of
    {ok, Str} ->
      Request = http:parse_request(Str),
      %{_, Headers, _} = Request,
      %lists:foreach(fun(H) -> io:format("     ~s~n", [H]) end, Headers),
      Response = reply(Request),
      gen_tcp:send(Client, Response);
    {error, Error} ->
      log("Rudy: Error: ~w~n", [Error])
  end,
  gen_tcp:close(Client),
  log("Rudy: Connection closed.~n").

reply({{get, URI, _}, _, _}) ->
  io:format(URI),
  case URI of
    _ -> http:ok(URI)
  end.

log(String) ->
  log(String, []).
log(String, Data) ->
  Timestamp = os:timestamp(),
  {_, {HH, MM, SS}} = calendar:now_to_local_time(Timestamp),
  NewString = "[~w:~w:~w] " ++ String,
  NewData = [HH, MM, SS | Data],
  io:format(NewString, NewData).