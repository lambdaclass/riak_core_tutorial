-module(rc_example).

-include_lib("riak_core/include/riak_core_vnode.hrl").

-export([ping/0]).

%% @doc Pings a random vnode to make sure communication is functional
ping() ->
  DocIdx = riak_core_util:chash_key({<<"ping">>, term_to_binary(os:timestamp())}),
  PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, rc_example),
  [{IndexNode, _Type}] = PrefList,
  riak_core_vnode_master:sync_spawn_command(IndexNode, ping, rc_example_vnode_master).
