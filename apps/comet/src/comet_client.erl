-module(comet_client).

-export([start/2, timer/2, recv/1]).

start(Filename, Wait) ->
    inets:start(),
    spawn(?MODULE, timer, [10000, self()]),
    This = self(),
    spawn(fun()->
         loadurls(
           Filename,
           fun(U)->
               This ! {loadurl, U} 
           end,
           Wait
          )
    end),
    recv({0,0,0}).


recv(Stats) ->
    {Active, Closed, Chunks} = Stats,
    receive
        {stats} -> io:format("Stats: ~w\n",[Stats])
        after 0 -> noop
    end,
    receive
        {http,{_Ref,stream_start,_X}} -> recv({Active+1,Closed,Chunks});
        {http,{_Ref,stream,_X}}       -> recv({Active, Closed, Chunks+1});
        {http,{_Ref,stream_end,_X}}   -> recv({Active-1, Closed+1, Chunks});
        {http,{_Ref,{error,Why}}}     -> io:format("Closed: ~w\n",[Why]),
                                         recv({Active-1, Closed+1, Chunks});
        {loadurl, Url}                ->
            httpc:request(
              get, 
              {Url, []}, 
              [], 
              [
               {sync, false}, 
               {stream, self}, 
%               {version, "HTTP/1.1"}, 
               {body_format, binary}
              ]
             ),
            recv(Stats)
    end.

% 每T秒钟后给Who发送一个信息{stats}
timer(T, Who) ->
    receive
    after T ->
        Who ! {stats}
    end,
        timer(T, Who).

% 打开文件，调用for_each_line/3 函数
% Read lines from a file with a specified delay between lines:
for_each_line_in_file(Name, Proc, Mode, Accum0) ->
    {ok, Device} = file:open(Name, Mode),
    for_each_line(Device, Proc, Accum0).

% 对文件里的内容一行行读, 并执行回调函数Proc
for_each_line(Device, Proc, Accum) ->
    case io:get_line(Device, "") of
        eof  ->
            file:close(Device),
            Accum;
        Line ->
            NewAccum = Proc(Line, Accum),
            for_each_line(Device, Proc, NewAccum)
    end.

% 对文件Filename里的内容，每一行处理，执行回调函数Callback, 然后等Wait秒
loadurls(Filename, Callback, Wait) ->
    for_each_line_in_file(
        Filename,
        fun(Line, List) ->
            Callback(string:strip(Line, right, $\n)),  % 得到以\n为分隔符的左面的字串,并执行回调函数Callback/1
            receive
            after Wait ->
                noop
            end,
            List
        end,
        [read],
        []
     ).
