-module(rc_example_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).


start(_StartType, _StartArgs) ->
  ok = riak_core:register([{vnode_module, rc_example_vnode}]),
  ok = riak_core_node_watcher:service_up(rc_example, self()),
  rc_example_sup:start_link().

stop(_State) ->
    ok.
