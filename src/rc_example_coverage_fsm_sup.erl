-module(rc_example_coverage_fsm_sup).

-behavior(supervisor).

-export([start_link/0,
         start_fsm/1,
         init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  CoverageFSM = {undefined,
                 {rc_example_coverage_fsm, start_link, []},
                 temporary, 5000, worker, [rc_example_coverage_fsm]},

  {ok, {{simple_one_for_one, 10, 10}, [CoverageFSM]}}.

start_fsm(Args) ->
  supervisor:start_child(?MODULE, Args).
