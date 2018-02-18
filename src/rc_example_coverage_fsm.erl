-module(rc_example_coverage_fsm).

-behaviour(riak_core_coverage_fsm).

-export([start_link/4,
         init/2,
         process_results/2,
         finish/2]).

start_link(ReqId, ClientPid, Request, Timeout) ->
  riak_core_coverage_fsm:start_link(?MODULE, {pid, ReqId, ClientPid}, [Request, Timeout]).

%% riak_core_coverage_fsm API

init({pid, ReqId, ClientPid}, [Request, Timeout]) ->
  lager:info("Starting coverage request ~p ~p", [ReqId, Request]),

  State = #{req_id => ReqId,
            from => ClientPid,
            request => Request,
            accum => []},

  %% See https://github.com/Kyorai/riak_core/blob/3.0.9/src/riak_core_coverage_fsm.erl#L45-L63
  %% for details on each of these elements
  {Request, %% An opaque data structure that is used by the VNode to implement the specific coverage request.
   allup,  %% 'all' for all VNodes or 'allup' for all reachable.
   1, %% replication factor
   1, %%  number of primary VNodes from the preference list to use
   rc_example, %% service to use to check for available nodes
   rc_example_vnode_master, %% The atom to use to reach the vnode master module
   Timeout,
   %% coverage plan was added in the _ng fork
   %% https://github.com/Kyorai/riak_core/commit/3826e3335ab3fe0008b418c4ece17845bcf1d4dc#diff-638fdfff08e818d2858d8b9d8d290c5f
   riak_core_coverage_plan, %%  The module which defines create_plan
   State}.

process_results({{_ReqId, {_Partition, _Node}}, []}, State ) ->
  {done, State};

process_results({{_ReqId, {Partition, Node}}, Data},
                State = #{accum := Accum}) ->
  NewAccum = [{Partition, Node, Data} | Accum],
  {done, State#{accum => NewAccum}}.

finish(clean, State = #{req_id := ReqId, from := From, accum := Accum}) ->
  lager:info("Finished coverage request ~p", [ReqId]),

  %% send the result back to the caller
  From ! {ReqId, {ok, Accum}},
  {stop, normal, State};

finish({error, Reason}, State = #{req_id := ReqId, from := From, accum := Accum}) ->
  lager:warning("Coverage query failed! Reason: ~p", [Reason]),
  From ! {ReqId, {partial, Reason, Accum}},
  {stop, normal, State}.
