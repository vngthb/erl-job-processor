-module(job_processor_server).

-export([
    start/1
]).

start(Port) ->
    {ok, ListenSocket} = gen_tcp:listen(Port, [{reuseaddr, true}]),
    spawn(fun() -> accept(ListenSocket) end),
    receive
        stop -> gen_tcp:close(ListenSocket)
    end.

accept(ListenSocket) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    inet:setopts(Socket, [{packet, http_bin}, {active, false}]),
    spawn(fun() -> accept(ListenSocket) end),
    RequestDetails = read(Socket, [], []),
    handle(Socket, RequestDetails).

read(Socket, Request0, Headers0) ->
    case gen_tcp:recv(Socket, 0, 5000) of
        {ok, {http_request, Method, AbsolutePath, _}} ->
            Request = [{method, Method}, {path, AbsolutePath}] ++ Request0,
            read(Socket, Request, Headers0);
        {ok, {http_header, _, Header, _, Value}} ->
            Headers = [{Header, Value}] ++ Headers0,
            read(Socket, Request0, Headers);
        {ok, http_eoh} ->
            Path = proplists:get_value(path, Request0),
            Method = proplists:get_value(method, Request0),
            Body = fetch_content(Socket, Headers0),
            {Path, Method, Headers0, Body}
    end.

fetch_content(Socket, Headers) ->
    ContentLength = proplists:get_value('Content-Length', Headers),
    case ContentLength of
        undefined ->
            "";
        _ ->
            ContentLengthInteger = binary_to_integer(ContentLength),
            inet:setopts(Socket, [{packet, raw}]),
            {ok, Body} = gen_tcp:recv(Socket, ContentLengthInteger),
            Body
    end.

handle(Socket, {{abs_path, <<"/sort-job-tasks">>}, Method, _Headers, Body0}) ->
    case Method of
        'POST' ->
            Job = jason:decode(Body0),
            {Status, ResponseBody} =
                case job_processor_lib:sort_job_tasks(Job) of
                    {error, _, _} = Error ->
                        {"400", jason:encode(error_to_proplist(Error))};
                    Result ->
                        {"200", jason:encode(Result)}
                end,
            reply(Socket, Status, [], ResponseBody);
        _ ->
            reply(Socket, "405", [], "")
    end;
handle(Socket, {{abs_path, <<"/job-to-bash">>}, Method, _Headers, Body0}) ->
    case Method of
        'POST' ->
            Job = jason:decode(Body0),
            {Status, ResponseBody} =
                case job_processor_lib:job_to_bash(Job) of
                    {error, _, _} = Error->
                        {"400", jason:encode(error_to_proplist(Error))};
                    Result ->
                        {"200", Result}
                end,
            reply(Socket, Status, [], ResponseBody);
        _ ->
            reply(Socket, "405", [], "")
    end;
handle(Socket, {_Path, _Method, _Headers, _Body}) ->
    reply(Socket, "404", [], "").

reply(Socket, Status, Headers, Body) ->
    Response = ["HTTP/1.0 ", Status, "\r\n", Headers, "\r\n", Body],
    gen_tcp:send(Socket, Response),
    gen_tcp:close(Socket).

error_to_proplist({Type, Reason, Details}) ->
    [{Type, Reason}, {details, Details}].
