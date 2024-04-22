%%%-------------------------------------------------------------------
%% @doc erl-job-processor public API
%% @end
%%%-------------------------------------------------------------------

-module(erl-job-processor_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    erl-job-processor_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
