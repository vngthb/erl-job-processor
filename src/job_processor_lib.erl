-module(job_processor_lib).

-export([
    sort_job_tasks/1,
    job_to_bash/1
]).

sort_job_tasks(Job) ->
    Tasks = proplists:get_value(<<"tasks">>, Job),
    [{<<"tasks">>, [sort_tasks(Tasks)]}].

job_to_bash(Job) ->
    Tasks = proplists:get_value(<<"tasks">>, Job),
    SortedTasks = sort_tasks(Tasks),
    Commands =
        [[<<"#!/usr/bin/env bash">>, 10]] ++
            [[proplists:get_value(<<"command">>, Task), $\n] || Task <- SortedTasks],
    iolist_to_binary(Commands).

sort_tasks(Tasks) ->
    Vertices = find_vertices(Tasks),
    Edges = lists:flatten([find_edges(Task) || Task <- Tasks]),
    Digraph = make_digraph(Vertices, Edges),
    CyclicRequirements = digraph_utils:cyclic_strong_components(Digraph),
    AllRequirements = find_all_requirements(Tasks),
    OrderedTasks = digraph_utils:postorder(Digraph),
    MissingRequirements = AllRequirements -- OrderedTasks,
    Result =
        if
            CyclicRequirements /= [] ->
                {error, cyclic_requirements, CyclicRequirements};
            MissingRequirements /= [] ->
                {error, missing_requirements, MissingRequirements};
            true ->
                OrderedTasks
        end,
    delete_digraph(Digraph),
    Result.

find_all_requirements(Tasks) ->
    AllRequirements = lists:flatten([proplists:get_value(<<"requires">>, Task, []) || Task <- Tasks]),
    lists:usort(AllRequirements).

find_vertices(Tasks) ->
    [proplists:get_value(<<"name">>, Task) || Task <- Tasks].

find_edges(Task) ->
    Name = proplists:get_value(<<"name">>, Task),
    case proplists:get_value(<<"requires">>, Task) of
        undefined -> [];
        Requirements -> [{Name, Requirement} || Requirement <- Requirements]
    end.

make_digraph(Vertices, Edges) ->
    Digraph = digraph:new(),
    lists:foreach(fun(Vertex) -> digraph:add_vertex(Digraph, Vertex) end, Vertices),
    lists:foreach(
        fun({Vertex1, Vertex2}) -> digraph:add_edge(Digraph, Vertex1, Vertex2) end, Edges
    ),
    Digraph.

delete_digraph(Digraph) ->
    digraph:delete(Digraph).

job() ->
    [
        {<<"tasks">>, [
            [
                {<<"name">>, <<"task-1">>},
                {<<"command">>, <<"touch /tmp/file1">>}
            ],
            [
                {<<"name">>, <<"task-2">>},
                {<<"command">>, <<"cat /tmp/file1">>},
                {<<"requires">>, [<<"task-3">>]}
            ],
            [
                {<<"name">>, <<"task-3">>},
                {<<"command">>, <<"echo 'Hello World!' > /tmp/file1">>},
                {<<"requires">>, [<<"task-1">>]}
            ],
            [
                {<<"name">>, <<"task-4">>},
                {<<"command">>, <<"rm /tmp/file1">>},
                {<<"requires">>, [<<"task-2">>, <<"task-3">>]}
            ]
        ]}
    ].