-module(http).
-author("Henry Bogaeus & Simon Carlson").

-export([test/0,parse_request/1, ok/1, get/1, not_found/0]).

parse_request(R0) ->
  {Request, R1} = request_line(R0),
  {Headers, R2} = headers(R1),
  {Body, _} = message_body(R2),
  {Request, Headers, Body}.

request_line([$G, $E, $T, 32 | R0]) ->
  {URI, R1} = request_uri(R0),
  {Ver, R2} = http_version(R1),
  [13, 10 | R3] = R2,
  {{get, URI, Ver}, R3}.

request_uri(String) ->
  request_uri(String, []).

request_uri([32 | R0], URI) ->
  {lists:reverse(URI), R0};
request_uri([C | R0], URI) ->
  request_uri(R0, [C | URI]).

http_version([$H, $T, $T, $P, $/, $1, $., $1 | R0]) ->
  {v11, R0};
http_version([$H, $T, $T, $P, $/, $1, $., $0 | R0]) ->
  {v10, R0}.

headers(String) ->
  headers(String, []).

headers([13, 10 | R0], Headers) ->
  {lists:reverse(Headers), R0};
headers(R0, Headers) ->
  {Header, R1} = header(R0, []),
  headers(R1, [Header | Headers]).

header([13, 10 | R0], Header) ->
  {lists:reverse(Header), R0};
header([C | R0], Header) ->
  header(R0, [C | Header]).


message_body(R) ->
  {R, []}.

ok(URI) ->
  {ok, Binary}  = file:read_file("html/hello.html"),
  HTLM = binary_to_list(Binary),
  "HTTP/1.1 200 OK\r\n" ++ "\r\n" ++ HTLM.

not_found() ->
  "HTTP/1.1 404 Not Found\r\n" ++ "\r\n" ++ "Not found...".

get(URI) ->
  "GET " ++ URI ++ " HTTP/1.1\r\n" ++ "\r\n".

test() ->
  %Request = "GET /index.html HTTP/1.1\r\nfoo 34\r\nbar 36\r\n\r\nHello~n",
  %io:format(Request),
  %parse_request(Request).
  %File = file:open("html/hello.html", [read]),
  {ok, Binary}  = file:read_file("html/hello.html"),
  HTLM = binary_to_list(Binary),
  io:format("~s~n", [HTLM]).