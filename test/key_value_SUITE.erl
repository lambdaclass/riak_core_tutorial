-module(key_value_SUITE).

-include_lib("common_test/include/ct.hrl").

-compile(export_all).

all() ->
  [ping_test,
   key_value_test,
   coverage_test].

init_per_suite(Config) ->
  Node1 = 'node1@127.0.0.1',
  Node2 = 'node2@127.0.0.1',
  Node3 = 'node3@127.0.0.1',
  start_node(Node1, 8198, 8199),
  start_node(Node2, 8298, 8299),
  start_node(Node3, 8398, 8399),

  build_cluster(Node1, Node2, Node3),

  [{node1, Node1},
   {node2, Node2},
   {node3, Node3} | Config].

end_per_suite(Config) ->
  Node1 = ?config(node1, Config),
  Node2 = ?config(node2, Config),
  Node3 = ?config(node3, Config),
  stop_node(Node1),
  stop_node(Node2),
  stop_node(Node3),
  ok.

ping_test(Config) ->
  Node1 = ?config(node1, Config),
  Node2 = ?config(node2, Config),
  Node3 = ?config(node3, Config),

  {pong, _Partition1} = rc_command(Node1, ping),
  {pong, _Partition2} = rc_command(Node2, ping),
  {pong, _Partition3} = rc_command(Node3, ping),

  ok.

key_value_test(Config) ->
  Node1 = ?config(node1, Config),
  Node2 = ?config(node2, Config),
  Node3 = ?config(node3, Config),

  ok = rc_command(Node1, put, [k1, v1]),
  ok = rc_command(Node1, put, [k2, v2]),
  ok = rc_command(Node1, put, [k3, v3]),

  %% get from any of the nodes
  v1 = rc_command(Node1, get, [k1]),
  v2 = rc_command(Node1, get, [k2]),
  v3 = rc_command(Node1, get, [k3]),
  not_found = rc_command(Node1, get, [k10]),

  v1 = rc_command(Node2, get, [k1]),
  v2 = rc_command(Node2, get, [k2]),
  v3 = rc_command(Node2, get, [k3]),
  not_found = rc_command(Node2, get, [k10]),

  v1 = rc_command(Node3, get, [k1]),
  v2 = rc_command(Node3, get, [k2]),
  v3 = rc_command(Node3, get, [k3]),
  not_found = rc_command(Node3, get, [k10]),

  %% test reset and delete
  ok = rc_command(Node1, put, [k1, v_new]),
  v_new = rc_command(Node1, get, [k1]),

  v_new = rc_command(Node1, delete, [k1]),
  not_found = rc_command(Node1, get, [k1]),

  ok = rc_command(Node1, put, [k1, v_new]),
  v_new = rc_command(Node1, get, [k1]),

  ok.

coverage_test(Config) ->
  Node1 = ?config(node1, Config),
  Node2 = ?config(node2, Config),

  %% clear, should contain no keys and no values
  {ok, []} = rc_command(Node1, clear),
  {ok, []} = rc_command(Node1, keys),
  {ok, []} = rc_command(Node1, values),

  ToKey = fun (N) -> "key" ++ integer_to_list(N) end,
  ToValue = fun (N) -> "value" ++ integer_to_list(N) end,
  Range = lists:seq(1, 100),
  lists:foreach(fun(N) ->
                    ok = rc_command(Node1, put, [ToKey(N), ToValue(N)])
                end, Range),

  ActualKeys = rc_coverage(Node2, keys),
  ActualValues = rc_coverage(Node2, values),

  100 = length(ActualKeys),
  100 = length(ActualValues),

  true = have_same_elements(ActualKeys, lists:map(ToKey, Range)),
  true = have_same_elements(ActualValues, lists:map(ToValue, Range)),

  ok.

%%% internal
start_node(NodeName, WebPort, HandoffPort) ->
  %% need to set the code path so the same modules are available in the slave
  CodePath = code:get_path(),
  PathFlag = "-pa " ++ lists:concat(lists:join(" ", CodePath)),
  {ok, _} = ct_slave:start(NodeName, [{erl_flags, PathFlag}]),

  %% set the required environment for riak core
  DataDir = "./data/" ++ atom_to_list(NodeName),
  rpc:call(NodeName, application, load, [riak_core]),
  rpc:call(NodeName, application, set_env, [riak_core, ring_state_dir, DataDir]),
  rpc:call(NodeName, application, set_env, [riak_core, platform_data_dir, DataDir]),
  rpc:call(NodeName, application, set_env, [riak_core, web_port, WebPort]),
  rpc:call(NodeName, application, set_env, [riak_core, handoff_port, HandoffPort]),
  rpc:call(NodeName, application, set_env, [riak_core, schema_dirs, ["../../lib/rc_example/priv"]]),

  %% start the rc_example app
  {ok, _} = rpc:call(NodeName, application, ensure_all_started, [rc_example]),

  ok.

stop_node(NodeName) ->
  ct_slave:stop(NodeName).

build_cluster(Node1, Node2, Node3) ->
  rpc:call(Node2, riak_core, join, [Node1]),
  rpc:call(Node3, riak_core, join, [Node1]),
  ok.

rc_command(Node, Command) ->
  rc_command(Node, Command, []).
rc_command(Node, Command, Arguments) ->
  rpc:call(Node, rc_example, Command, Arguments).

rc_coverage(Node, Command) ->
  {ok, List} = rc_command(Node, Command),
  %% convert the coverage result to a plain list
  lists:foldl(fun({_Partition, _Node, Values}, Accum) ->
                  lists:append(Accum, Values)
              end, [], List).

have_same_elements(List1, List2) ->
  S1 = sets:from_list(List1),
  S2 = sets:from_list(List2),
  sets:is_subset(S1, S2) andalso sets:is_subset(S2, S1).
