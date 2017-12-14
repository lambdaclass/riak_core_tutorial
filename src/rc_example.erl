-module(rc_example).

-include_lib("riak_core/include/riak_core_vnode.hrl").

-export([ping/0,
         ping/1,
         ring_status/0]).

%% @doc Pings a random vnode to make sure communication is functional
ping()->
  ping(os:timestamp()).

ping(Term) ->
  DocIdx = riak_core_util:chash_key({<<"ping">>, term_to_binary(Term)}),
  PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, rc_example),
  [{IndexNode, _Type}] = PrefList,
  riak_core_vnode_master:sync_spawn_command(IndexNode, ping, rc_example_vnode_master).

ring_status() ->
  {ok, Ring} = riak_core_ring_manager:get_my_ring(),
  riak_core_ring:pretty_print(Ring, [legend]).
