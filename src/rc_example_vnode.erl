-module(rc_example_vnode).

-behaviour(riak_core_vnode).

-include_lib("riak_core/include/riak_core_vnode.hrl").

-export([start_vnode/1,
         init/1,
         terminate/2,
         handle_command/3,
         is_empty/1,
         delete/1,
         handle_handoff_command/3,
         handoff_starting/2,
         handoff_cancelled/1,
         handoff_finished/2,
         handle_handoff_data/2,
         encode_handoff_item/2,
         handle_coverage/4,
         handle_exit/3]).

%% API
start_vnode(I) ->
  riak_core_vnode_master:get_vnode_pid(I, ?MODULE).

init([Partition]) ->
  {ok, #{partition => Partition, data => #{}}}.

%% Sample command: respond to a ping
handle_command(ping, _Sender, State = #{partition := Partition}) ->
  log("Reived ping comand", State),
  {reply, {pong, Partition}, State};

handle_command({put, Key, Value}, _Sender, State = #{data := Data}) ->
  log("PUT ~p:~p", [Key, Value], State),
  NewData = Data#{Key => Value},
  {reply, ok, State#{data => NewData}};

handle_command({get, Key}, _Sender, State = #{data := Data}) ->
  log("GET ~p", [Key], State),
  {reply, maps:get(Key, Data, not_found), State};

handle_command({delete, Key}, _Sender, State = #{data := Data}) ->
  log("DELETE ~p", [Key], State),
  NewData = maps:remove(Key, Data),
  {reply, maps:get(Key, Data, not_found), State#{data => NewData}};

handle_command(Message, _Sender, State) ->
  logger:warning("unhandled_commanod ~p", [Message]),
  {noreply, State}.

%% handle_handoff_command will be called when commands come in during
%% handoff. These can be regular commands (i.e. get, put, delete) or the
%% special ?FOLD_REQ which contains the fold function that's to be used to
%% to pass this vnode's data to the target vnode.
%%
%% See for reference:
%% https://github.com/Kyorai/riak_core/blob/faf04f4820aff5bc876f79609fa838e1c86c0fb0/src/riak_core_vnode.erl#L312-L339
%% https://github.com/basho/riak_kv/blob/d5cfe62d8f0ff36ead2019bde7a08cdd33fd3764/src/riak_kv_vnode.erl#L974-L984
handle_handoff_command(?FOLD_REQ{foldfun=FoldFun, acc0=Acc0}, _Sender,
                       State = #{data := Data}) ->
  %% this fold is synchronous, so even if a command can come in during the entire
  %% handoff process, we can safely assume that the state won't be modified
  %% concurrently with the fold
  log("Received fold request for handoff", State),
  Result = maps:fold(FoldFun, Acc0, Data),
  {reply, Result, State};

handle_handoff_command({get, Key}, Sender, State) ->
  %% if this vnode receives the get, means the new target didn't take over yet
  %% i.e. this vnode still has the most up to date data, and cand handle locally
  log("GET during handoff, handling locally ~p", [Key], State),
  handle_command({get, Key}, Sender, State);

handle_handoff_command(Message, Sender, State) ->
  %% write requests (put and delete) should be forwarded to the new partition
  %% owner vnode, but also applied locally, so we can keep serving gets
  {reply, _Result, NewState} = handle_command(Message, Sender, State),
  {forward, NewState}.

handoff_starting(_TargetNode, State) ->
  log("starting handoff", State),
  {true, State}.

is_empty(State = #{data := Data}) ->
  IsEmpty = maps:size(Data) == 0,
  {IsEmpty, State}.

handoff_cancelled(State) ->
  log("handoff cancelled", State),
  {ok, State}.

encode_handoff_item(Key, Value) ->
  erlang:term_to_binary({Key, Value}).

handle_handoff_data(BinData, State = #{data := Data}) ->
  {Key, Value} = erlang:binary_to_term(BinData),
  log("received handoff data ~p", [{Key, Value}], State),
  NewData = Data#{Key => Value},
  {reply, ok, State#{data => NewData}}.

handoff_finished(_TargetNode, State) ->
  log("finished handoff", State),
  {ok, State}.

delete(State) ->
  log("deleting the vnode data", State),
  {ok, State#{data => #{}}}.

handle_coverage(keys, _KeySpaces, {_, ReqId, _}, State = #{data := Data}) ->
  log("Received keys coverage", State),
  Keys = maps:keys(Data),
  {reply, {ReqId, Keys}, State};

handle_coverage(values, _KeySpaces, {_, ReqId, _}, State = #{data := Data}) ->
  log("Received values coverage", State),
  Values = maps:values(Data),
  {reply, {ReqId, Values}, State};

handle_coverage(clear, _KeySpaces, {_, ReqId, _}, State) ->
  log("Received clear coverage", State),
  NewState = State#{data => #{}},
  {reply, {ReqId, []}, NewState}.

handle_exit(_Pid, _Reason, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

%%% internal
log(String, State) ->
  log(String, [], State).

%% same as logger:info but prepends the partition
log(String, Args, #{partition := Partition}) ->
  String2 = "[~.36B] " ++ String,
  Args2 = [Partition | Args],
  logger:info(String2, Args2),
  ok.
