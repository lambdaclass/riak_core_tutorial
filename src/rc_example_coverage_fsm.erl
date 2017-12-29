-module(rc_example_coverage_fsm).

-behaviour(riak_core_coverage_fsm).

-export([run/2,
         start_link/4,
         init/2,
         process_results/2,
         finish/2]).

run(Request, Timeout) ->
  ReqId = erlang:phash2(erlang:monotonic_time()),
  {ok, _} = rc_example_coverage_fsm_sup:start_fsm([ReqId, self(), Request, Timeout]),
  receive
    {ReqId, Val} -> Val
  end.

start_link(ReqId, From, Request, Timeout) ->
  riak_core_coverage_fsm:start_link(?MODULE, {pid, ReqId, From},
                                    [ReqId, From, Request, Timeout]).

%% riak_core_coverage_fsm API

%% TODO log both From and drop extra argument if its the same
init(_From, [ReqId, From, Request, Timeout]) ->
  lager:info("Starting coverage request ~p ~p", [ReqId, Request]),

  State = #{req_id => ReqId,
            from => From,
            request => Request,
            accum => []},

  %% TODO explain each value
  {Request,
   allup,
   1,
   1,
   rc_example,
   rc_example_vnode_master,
   Timeout,
   %% coverage plan was added in the _ng fork
   %% https://github.com/Kyorai/riak_core/commit/3826e3335ab3fe0008b418c4ece17845bcf1d4dc#diff-638fdfff08e818d2858d8b9d8d290c5f
   riak_core_coverage_plan,
   State}.

process_results({{_ReqId, {Partition, Node}}, Data},
                State = #{accum := Accum}) ->
  NewAccum = [{Partition, Node, Data} | Accum],
  {done, State#{accum => NewAccum}}.

finish(clean, State = #{req_id := ReqId, from := From, accum := Accum}) ->
  lager:info("Finished coverate request ~p", [ReqId]),

  %% send the result back to the caller
  From ! {ReqId, {ok, Accum}},
  {stop, normal, State};

finish({error, Reason}, State = #{req_id := ReqId, from := From, accum := Accum}) ->
  lager:warning("Coverage query failed! Reason: ~p", [Reason]),
  From ! {ReqId, {partial, Reason, Accum}},
  {stop, normal, State}.
