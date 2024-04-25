-module(job_processor_sup).

-behaviour(supervisor).

-export([start_link/1]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link(Args) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Args).

init(Args) ->
    SupFlags = #{
        strategy => one_for_all,
        intensity => 0,
        period => 1
    },
    ChildSpecs = [
        #{
            id => jps_1,
            start => {job_processor_server, start, [Args]}
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.
