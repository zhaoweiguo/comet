%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc Web server for comet.

-module(comet_web).
-author("Mochi Media <dev@mochimedia.com>").

-include("comet_common.hrl").

-export([start/1, stop/0, loop/2]).

%% External API
start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot)
           end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req, DocRoot) ->
    "/" ++ Path = Req:get(path),
    try
        case Req:get(method) of
            Method when Method =:= 'GET'; Method =:= 'HEAD' ->
                case Path of
                    "hello" ->  %get請求 /hello?username=<UserName>
                        hello('GET', Req);
                    "test/" ++ Id ->
                        comet_test(Id, Req);
                    _ ->
                        ?P2("~p", [Path]),
                        Req:serve_file(Path, DocRoot)
                end;
            'POST' ->
                case Path of
                    "hello" ->
                        hello('POST', Req);
                    _ ->
                        Req:not_found()
                end;
            _ ->
                Req:respond({501, [], []})
        end
    catch
        Type:What ->
            Report = ["web request failed",
                      {path, Path},
                      {type, Type}, {what, What},
                      {trace, erlang:get_stacktrace()}],
            error_logger:error_report(Report),
            %% NOTE: mustache templates need \ because they are not awesome.
            Req:respond({500, [{"Content-Type", "text/plain"}],
                         "request failed, sorry\n"})
    end.

%% Internal API

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.



hello('GET', Req) ->
    QueryStringData = Req:parse_qs(),  %得到包含query string参数的proplist
    ?P2("~p", [QueryStringData]),
    Username = proplists:get_value("username", QueryStringData, "Anonymous"),
                                                % 返回http請求, 格式: {HTTP状态码、头信息proplist、主体信息}
    Req:respond({200, [{"Content-Type", "text/plain"}],
                 "Hello " ++ Username ++ "!\n"});
hello('POST', Req) ->
    PostData = Req:parse_post(),  %得到包含query string参数的proplist
    Username = proplists:get_value("username", PostData, "Anonymous"),
    Req:respond({200, [{"Content-Type", "text/html"}],
                 "Hello " ++ Username ++ "!\n"}).

% 测试最基本的comet类型
comet_test(Id, Req) ->
    Response = Req:ok({"text/html; charset=utf-8",
                       [{"Server","Mochiweb-Test"}],
                       chunked}),
    Response:write_chunk("Mochiconntest welcomes you! Your Id: " ++ Id ++ "\n"),
    %% router:login(list_to_atom(Id), self()),
    feed(Response, Id, 1).


feed(Response, Path, N) ->
    receive
         %{router_msg, Msg} ->
         %    Html = io_lib:format("Recvd msg #~w: '~s'<br/>", [N, Msg]),
         %    Response:write_chunk(Html);
    after 10000 ->
            Msg = io_lib:format("Chunk ~w for id ~s\n", [N, Path]),
            Response:write_chunk(Msg)
    end,
    feed(Response, Path, N+1).




%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

you_should_write_a_test() ->
    ?assertEqual(
       "No, but I will!",
       "Have you written any tests?"),
    ok.

-endif.
