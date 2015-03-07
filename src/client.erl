-module(client).
-author("Henry").

-export([test/0]).

test() ->
  HTTP_Request = "GET /index.html HTTP/1.1\r\nfoo 34\r\nbar 36\r\n\r\nHello~n",
  Server = localhost,
  io:format("Client: Connecting to ~w...~n", [Server]),
  case gen_tcp:connect(Server, 8080, [list]) of
    {ok, Socket} ->
      io:format("Client: Connection successful!~n"),
      gen_tcp:send(Socket, HTTP_Request),
      gen_tcp:close(Socket);
    {error, Error} ->
      ok = io:format("Client: ~w~n", [Error])
  end.