-module(rc_example_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    VMaster =
        {rc_example_vnode_master,
         {riak_core_vnode_master, start_link, [rc_example_vnode]},
         permanent,
         5000,
         worker,
         [riak_core_vnode_master]},

    CoverageFSM =
        {rc_example_coverage_fsm_sup,
         {rc_example_coverage_fsm_sup, start_link, []},
         permanent,
         infinity,
         supervisor,
         [rc_example_coverage_fsm_sup]},

    {ok, {{one_for_one, 5, 10}, [VMaster, CoverageFSM]}}.
