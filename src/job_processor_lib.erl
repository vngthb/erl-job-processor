-module(job_processor_lib).

-export([
    sort_job_tasks/1,
    job_to_bash/1
]).

sort_job_tasks(Job) ->
    Tasks = proplists:get_value(<<"tasks">>, Job),
    [{<<"tasks">>, [sort_tasks(Tasks, [], [])]}].

job_to_bash(Job) ->
    Tasks = proplists:get_value(<<"tasks">>, Job),
    SortedTasks = sort_tasks(Tasks, [], []),
    Commands =
        [[<<"#!/usr/bin/env bash">>, 10]] ++
            [[proplists:get_value(<<"command">>, Task), $\n] || Task <- SortedTasks],
    iolist_to_binary(Commands).

sort_tasks([], Sorted, _SortedTaskNames) ->
    Sorted;
sort_tasks([H | T], Sorted, SortedTaskNames) ->
    case proplists:get_value(<<"requires">>, H) of
        undefined ->
            sort_tasks(T, Sorted ++ [H], SortedTaskNames ++ [proplists:get_value(<<"name">>, H)]);
        Requirements ->
            case Requirements -- SortedTaskNames of
                [] ->
                    sort_tasks(
                        T, Sorted ++ [H], SortedTaskNames ++ [proplists:get_value(<<"name">>, H)]
                    );
                _ ->
                    sort_tasks(T ++ [H], Sorted, SortedTaskNames)
            end
    end.

% job() ->
%     [
%         {<<"tasks">>, [
%             [
%                 {<<"name">>, <<"task-1">>},
%                 {<<"command">>, <<"touch /tmp/file1">>}
%             ],
%             [
%                 {<<"name">>, <<"task-2">>},
%                 {<<"command">>, <<"cat /tmp/file1">>},
%                 {<<"requires">>, [<<"task-3">>]}
%             ],
%             [
%                 {<<"name">>, <<"task-3">>},
%                 {<<"command">>, <<"echo 'Hello World!' > /tmp/file1">>},
%                 {<<"requires">>, [<<"task-1">>]}
%             ],
%             [
%                 {<<"name">>, <<"task-4">>},
%                 {<<"command">>, <<"rm /tmp/file1">>},
%                 {<<"requires">>, [<<"task-2">>, <<"task-3">>]}
%             ]
%         ]}
%     ].
