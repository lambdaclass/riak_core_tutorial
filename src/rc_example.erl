-module(rc_example).

-include_lib("riak_core/include/riak_core_vnode.hrl").

-export([ping/0,
         ping/1,
         ring_status/0,
         put/2,
         get/1,
         delete/1,
         keys/0,
         values/0
        ]).

%% @doc Pings a random vnode to make sure communication is functional
ping()->
  ping(os:timestamp()).

ping(Key) ->
  sync_command(Key, ping).

ring_status() ->
  {ok, Ring} = riak_core_ring_manager:get_my_ring(),
  riak_core_ring:pretty_print(Ring, [legend]).

put(Key, Value) ->
  sync_command(Key, {put, Key, Value}).

get(Key) ->
  sync_command(Key, {get, Key}).

delete(Key) ->
  sync_command(Key, {delete, Key}).

keys() ->
  coverage_command(keys).

values() ->
  coverage_command(values).

%% internal
hash_key(Key) ->
  riak_core_util:chash_key({<<"rc_example">>, term_to_binary(Key)}).

sync_command(Key, Command) ->
  DocIdx = hash_key(Key),
  PrefList = riak_core_apl:get_apl(DocIdx, 1, rc_example),
  [IndexNode] = PrefList,
  riak_core_vnode_master:sync_spawn_command(IndexNode, Command, rc_example_vnode_master).

coverage_command(Command) ->
  Timeout = 5000,
  ReqId = erlang:phash2(erlang:monotonic_time()),
  {ok, _} = rc_example_coverage_fsm_sup:start_fsm([ReqId, self(), Command, Timeout]),

  receive
    {ReqId, Val} -> Val
  end.
