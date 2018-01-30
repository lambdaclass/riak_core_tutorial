# Riak Core Tutorial

A basic example of a riak_core application, using the most
recent version of the [riak_core_ng fork](https://hex.pm/packages/riak_core_ng)
and running on Erlang/OTP 20 with rebar3.

This example was largely based on the
[Little Riak Core Book](https://marianoguerra.github.io/little-riak-core-book/)
and the
[Create a riak_core application in Elixir](https://medium.com/@GPad/create-a-riak-core-application-in-elixir-part-1-41354c1f26c3)
series.

The code on this repository can be used directly by cloning it; see [Usage](/#usage)
for example commands. Alternatively,
the [Tutorial](/#riak-core-tutorial) explains the step-by-step process to
produce the same code base from scratch.

* [Example application usage](#example-application-usage)
* [Riak Core Tutorial](#riak-core-tutorial-1)
  * [When to use Riak Core](#when-to-use-riak-core)
  * [About this tutorial](#about-this-tutorial)
  * [Useful links](#useful-links)
  * [0. Riak Core overview](#0-riak-core-overview)
  * [1. Setup](#1-setup)
  * [2. The vnode](#2-the-vnode)
    * [The riak_vnode behavior](#the-riak_vnode-behavior)
    * [Application and supervisor setup](#application-and-supervisor-setup)
    * [Sending commands to the vnode](#sending-commands-to-the-vnode)
  * [3. Setting up the cluster](#3-setting-up-the-cluster)
  * [4. Building a distributed Key/Value store](#4-building-a-distributed-keyvalue-store)
  * [5. Testing](#5-testing)
  * [6. Coverage commands](#6-coverage-commands)
  * [7. Handoff](#7-handoff)
  * [8. Redundancy and fault-tolerance](#8-redundancy-and-fault-tolerance)

## Example application usage
Run on three separate terminals:

``` shell
make dev1
make dev2
make dev3
```

Join the nodes and ping:

``` erlang
(rc_example1@127.0.0.1)1> riak_core:join('rc_example2@127.0.0.1').
(rc_example1@127.0.0.1)2> riak_core:join('rc_example3@127.0.0.1').
(rc_example1@127.0.0.1)3> rc_example:ping().
```

Check the ring status:

``` erlang
(rc_example3@127.0.0.1)4> rc_example:ring_status().
```

Try the key/value commands:

``` erlang
(rc_example1@127.0.0.1)1> rc_example:put(k1, v1).
ok
(rc_example1@127.0.0.1)2> rc_example:put(k2, v2).
ok
(rc_example2@127.0.0.1)1> rc_example:get(k2).
v2
```

## Riak Core Tutorial

[Riak Core](https://github.com/basho/riak_core) is the distributed
systems framework used by the [Riak data store](https://github.com/basho/riak)
to distribute data and scale. More generally, it can be thought of as
a toolkit for building distributed, scalable, fault-tolerant
applications. In practical terms, Riak Core is an Erlang/OTP application, and most
of the user defined work is done in the `riak_core_vnode` behavior.

### When to use Riak Core

What makes Riak Core so interesting and useful is that it implements
the ideas of the
[Amazon's Dyamo](https://en.wikipedia.org/wiki/Dynamo_(storage_system))
architecture and exposes its infrastructure as
a reusable library, allowing to easily apply them in any context that
can benefit from decentralized distribution of work (including but not
limited to data stores).

As you will see, it provides the basic blocks to build distributed services, consistent hashing, routing, support for sharding and replicating, distributed queries, etc. They need not all be used. For example, a game server which handles requests from players could partition players to handle load, and ensure that players requests are always handled on the same vnode to ensure data locality.

A distributed batch job handling system could also use consistent hashing and routing to ensure jobs from the same batch are always handled by the same node, or distribute the jobs across several partitions and then use the distributed map-reduce queries to gather results.

### About this tutorial

Basho, the company that originally developed Riak and Riak Core was
put into receivership in 2017. This introduces some uncertainty about the
future of these products, although the community has shown interest in
taking over their maintenance. At the moment of writing,
the [riak_core_ng](https://github.com/Kyorai/riak_core) fork seems to
be the most actively maintained fork of Riak Core and hopefully the work being
done there will eventually be merged back to the canonical repository.

As part of our interest in this technology and our intention to use it
in new projects we had to struggle a bit with scarce and outdated
documenatation, stale dependencies, etc. The intention is thus to
provide a tutorial on how to use Riak Core today, on an Erlang 20
and rebar3 project, with minimal dependencies and operational
sugar. You'll notice the structure borrows heavily from
the
[Little Riak Core Book](https://marianoguerra.github.io/little-riak-core-book/)
and the
[riak_core in Elixir](https://medium.com/@GPad/create-a-riak-core-application-in-elixir-part-1-41354c1f26c3)
series, which were our main references.

### Useful links
* [Introducing Riak Core](http://basho.com/posts/business/introducing-riak-core/)
* [Riak Core Wiki](https://github.com/basho/riak_core/wiki)
* [Masterless Distributed Computing with Riak Core](http://www.erlang-factory.com/upload/presentations/294/MasterlessDistributedComputingwithRiakCore-RKlophaus.pdf)
* Ryan Zezeski's "working" blog:
  [First, multinode](https://github.com/rzezeski/try-try-try/tree/master/2011/riak-core-first-multinode) and
  [The vnode](https://github.com/rzezeski/try-try-try/tree/master/2011/riak-core-the-vnode)
* [Little Riak Core Book](https://marianoguerra.github.io/little-riak-core-book/)
* riak_core in Elixir:
  [Part I](https://medium.com/@GPad/create-a-riak-core-application-in-elixir-part-1-41354c1f26c3),
  [Part II](https://medium.com/@GPad/create-a-riak-core-application-in-elixir-part-2-88bdec73f368),
  [Part III](https://medium.com/@GPad/create-a-riak-core-application-in-elixir-part-3-8bac36632be0),
  [Part IV](https://medium.com/@GPad/create-a-riak-core-application-in-elixir-part-4-728512ece224) and
  [Part V](https://medium.com/@GPad/create-a-riak-core-application-in-elixir-part-5-86cd9d2c6b92)
* [A Gentle Introduction to Riak Core](http://efcasado.github.io/riak-core_intro/)
* Understanding Riak Core:
  [Handoff](http://basho.com/posts/technical/understanding-riak_core-handoff/),
  [The visit fun](http://basho.com/posts/technical/understanding-riak_core-visitfun/) and
  [Building Handoff](http://basho.com/posts/technical/understanding-riak_core-building-handoff/)

### 0. Riak Core overview

Riak Core is based on the Dynamo architecture, meaning it
scales and distributes the work in a decentralized manner, using
[Consistent Hashing](https://en.wikipedia.org/wiki/Consistent_hashing).

Most operations are applied to an object which is identified by some
data value. In the context of a Key/Value store, for example, the
identifier is the Key used in get, put and delete operations.

Before performing the operation, a hashing function is applied to
the key. The key hash will be used to decide which node in the
cluster should be responsible for executing the operation. The range of
possible values the key hash can take (the keyspace, usually
depicted as a ring), is partitioned in equally sized buckets, which
are assigned to virtual nodes, also known as vnodes.

![The Ring](ring.png)

The number of vnodes is fixed at cluster creation and a given hash value will
always belong to the same partition (i.e. the same vnode). The vnodes in
turn are evenly distributed across all available physical nodes.
Note this distribution isn't fixed as the keyspace partitioning
is: the vnode distribution can change if a physical node is added
to the cluster or goes down.

### 1. Setup

In this tutorial we'll build an in-memory, distributed key/value
store. Let's start by creating a new project with rebar3:

``` shell
$ rebar3 new app rc_example
===> Writing rc_example/src/rc_example_app.erl
===> Writing rc_example/src/rc_example_sup.erl
===> Writing rc_example/src/rc_example.app.src
===> Writing rc_example/rebar.config
===> Writing rc_example/.gitignore
===> Writing rc_example/LICENSE
===> Writing rc_example/README.md
```

Note there's a
[rebar3 template for riak core](https://github.com/marianoguerra/rebar3_template_riak_core). The
reason we don't use
it here is that it's outdated and it generates a lot of operational
code that would take a lot of effort to figure out and fix. Instead,
we'll start with an empty project and build our way up, although the
code generated by the template can serve as a good reference along the
way.

Next up we'll fill up some of the rebar.config file. We'll add the
riak_core dependency and lager, which we'll use for logging:

``` erlang
{erl_opts, [debug_info, {parse_transform, lager_transform}]}.
{deps, [{riak_core, "3.0.9", {pkg, riak_core_ng}}, {lager, "3.5.1"}]}.
```

Note we're using
the [`riak_core_ng` fork](https://hex.pm/packages/riak_core_ng), which is
more up to date so it's easier to make it work with Erlang 20. If you
go ahead and try to `rebar3 compile` your project, you'll notice it
fails with this message:

``` shell
===> Compiling _build/default/lib/riak_ensemble/src/riak_ensemble_test.erl failed
_build/default/lib/riak_ensemble/src/riak_ensemble_test.erl:21: export_all flag enabled - all functions will be exported
```

The issue here is that some of the dependencies of riak_core use
the [warnings_as_errors option](http://erlang.org/doc/man/compile.html),
and their code contains stuff
that produces warnings in recent Erlang versions (namely, they use
`export_all` or `gen_fsm`). To fix this we need to override their
configuration in our rebar.config file, removing the `warnings_as_errors` option:

``` erlang
{overrides, [{override, riak_ensemble,
              [{erl_opts, [debug_info,
                           warn_untyped_record,
                           {parse_transform, lager_transform}]}]},

             {override, riak_core,
              [{erl_opts, [{parse_transform, lager_transform},
                           debug_info, {platform_define, "^[0-9]+", namespaced_types},
                           {platform_define, "18", old_rand},
                           {platform_define, "17", old_rand},
                           {platform_define, "^R15", old_hash}]}]},

             {override, poolboy,
              [{erl_opts, [debug_info,
                           {platform_define, "^[0-9]+", namespaced_types}]}]}]}
```

Now that the project compiles, let's try to build and run a
release. First we need to add lager and riak_core to
`src/rc_example.app.src`, so they're started along with our
application. We also need to add cuttlefish, which is a system riak uses
for its internal configuration:

``` erlang
  {applications,
   [kernel,
    stdlib,
    lager,
    cuttlefish,
    riak_core
   ]}
```

Then, add the [release configuration](https://www.rebar3.org/docs/releases) for
development in `rebar.config`:

``` erlang
{relx, [{release, {rc_example, "0.1.0"}, [rc_example]},
        {dev_mode, true},
        {include_erts, false},
        {sys_config, "conf/sys.config"},
        {vm_args, "conf/vm.args"},
        {extended_start_script, false}]}.
```

Note we won't be using the rebar3 shell command, which doesn't play
along nicely with riak_core; we need a proper release instead (although we can
use dev_mode). Thus, we can build and run the release with:

    $ rebar3 release && _build/default/rel/rc_example/bin/rc_example

If you go ahead and run that you'll see an error like `Failed to load
ring file: "no such file or directory"`. We need to add some configuration to
`conf/sys.config` and `conf/vm.args` to properly start riak_core:

``` erlang
%% vm.args
-name rc_example@127.0.0.1

%% conf/sys.config
[{riak_core,
  [{ring_state_dir, "./data/ring"},
   {web_port, 8098},
   {handoff_port, 8099},
   {schema_dirs, ["priv"]}
  ]}].
```

`vm.args` just sets the node name; in `sys.config` we set a data
directory for riak core (`ring_state_dir`) and a
couple of ports; we also need to point riak to its schema (by setting
`schema_dirs`). For this to work we have to copy
[this file](https://github.com/Kyorai/riak_core/blob/fifo-merge/priv/riak_core.schema)
to `priv/riak_core.schema`.

At this point we should have a runnable release (if you see errors,
try removing the _build directory):

    $ rebar3 release && _build/default/rel/rc_example/bin/rc_example

### 2. The vnode

So far we've got a single Erlang node running a release with riak_core
in it, but we didn't really write any code to test it. So, before
getting into the distributed aspects of riak_core, let's add the
simplest possible functionality: a ping command.

Recall from the [overview](/#0-riak-core-overview), that the keyspace (the range of all possible
results of hashing a key) is partitioned, and each partition is assigned to a
virtual node. The vnode is a worker process which
handles incoming requests known as commands and is implemented as
an OTP behavior. In our initial example
we'll create an empty vnode that only knows how to handle a ping
command. A detailed explanation of vnodes can be found [here](https://github.com/rzezeski/try-try-try/tree/master/2011/riak-core-the-vnode).

#### The riak_vnode behavior

Let's add a `src/rc_example_vnode.erl` module that will implement the
`riak_core_vnode` behavior:

``` erlang
-module(rc_example_vnode).
-behaviour(riak_core_vnode).

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

start_vnode(I) ->
    riak_core_vnode_master:get_vnode_pid(I, ?MODULE).

init([Partition]) ->
    {ok, #{partition => Partition}}.

handle_command(ping, _Sender, State = #{partition := Partition}) ->
  lager:info("Received ping command ~p", [Partition]),
  {reply, {pong, Partition}, State};

handle_command(Message, _Sender, State) ->
    lager:warning("unhandled_command ~p", [Message]),
    {noreply, State}.

```

First off, the `start_vnode` function. This is not a riak_vnode
behavior callback, but it's nevertheless required for the vnode to work.
This function isn't documented, and to my knowledge it will always
have the same implementation: `riak_core_vnode_master:get_vnode_pid(I,
?MODULE).`, so it could probably be handled internally by
riak\_core. Since it isn't, we copy paste that line everytime ¯\\\_(ツ)_/¯

The `init` callback initializes the state of the vnode, much like in a
gen_server. In the code above we intialize a state map that only
contains the id of the partition assigned to the vnode.

The next interesting callback is `handle_command`, which as you may
expect handles the requests that are assigned to the vnode. The nature
of the command will be defined by the Message parameter. In the case of
our simple ping command, we add a new `handle_command` clause that
just replies with the partition id of the vnode.

That's all we need to get started, the rest of the `riak_vnode`
callbacks will have dummy implementations. We'll get back at those in the
following sections.

``` erlang
handle_handoff_command(_Message, _Sender, State) ->
    {noreply, State}.

handoff_starting(_TargetNode, State) ->
    {true, State}.

handoff_cancelled(State) ->
    {ok, State}.

handoff_finished(_TargetNode, State) ->
    {ok, State}.

handle_handoff_data(_Data, State) ->
    {reply, ok, State}.

encode_handoff_item(_ObjectName, _ObjectValue) ->
    <<>>.

is_empty(State) ->
    {true, State}.

delete(State) ->
    {ok, State}.

handle_coverage(_Req, _KeySpaces, _Sender, State) ->
    {stop, not_implemented, State}.

handle_exit(_Pid, _Reason, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
```

#### Application and supervisor setup
Before moving on we need to add some boilerplate code for riak_core to
find and manage our example vnode. Update the `start` callback in
`src/rc_example_app.erl`:

``` erlang
start(_StartType, _StartArgs) ->
  ok = riak_core:register([{vnode_module, rc_example_vnode}]),
  ok = riak_core_node_watcher:service_up(rc_example, self()),

  rc_example_sup:start_link().
```

The first line initialises the ring telling riak_core to use
`rc_example_vnode` as a vnode module. The second one starts the
node_watcher, a process responsible for tracking the status of nodes within a riak_core cluster.

We also need to update the supervisor in `src/rc_example_sup.erl`, to
start the vnode_master, the process that coordinates the
distribution of work within the physical node: it starts all the
worker vnodes, receives all the requests on that particular physical
node and routes each of them to the vnode that should handle it.

``` erlang
init([]) ->
  VMaster = {rc_example_vnode_master,
             {riak_core_vnode_master, start_link, [rc_example_vnode]},
             permanent, 5000, worker, [riak_core_vnode_master]},

  {ok, {{one_for_one, 5, 10}, [VMaster]}}.
```

#### Sending commands to the vnode

So far we have a vnode that knows how to respond to an incoming ping
request, but we still need an API to be able to send that
request. We'll add a `src/rc_example.erl` file that will contain the
public interface to our application:

``` erlang
-module(rc_example).

-export([ping/0]).

ping()->
  Key = os:timestamp(),
  DocIdx = hash_key(Key),
  PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, rc_example),
  [{IndexNode, _Type}] = PrefList,
  Command = ping,
  riak_core_vnode_master:sync_spawn_command(IndexNode, Command, rc_example_vnode_master).

%% internal

hash_key(Key) ->
  riak_core_util:chash_key({<<"rc_example">>, term_to_binary(Key)}).
```

Let's go over the `ping()` implementation line by line. As stated
before, most operations will be performed over a single object (with the
exception of aggregation operations, like listing all available keys
in a key/value store). That object is usually identified by some key,
which will be hashed to decide what partition (that is what vnode at
what physical node) should receive
the request. In the case of `ping`, there isn't any actual object
involved, and thus no key, but we make a random one by using
`os:timestamp()`. The nature of the hashing algorithm
distributes values uniformly over the ring, so each new timestamp
should be assigned to a random partition of the ring.

The `hash_key` helper calls `riak_core_util:chash_key` to produce the
hash of the key. Note `chash_key` receives a tuple of two binaries;
the first element is called the bucket, a value
riak_core will use to namespace your keys; you can choose to have a
single one per application, or many, according to your needs.

The result of the hash is passed to `riak_core_apl:get_primary_apl`
which returns an Active Preference List (APL) for the given key, this
is a list of active vnodes that can handle that request. The amount of
offered vnodes will be determined by the second argument of the
function. We can try these functions in the release shell to get a
better sense of how they work:

``` erlang
(rc_example@127.0.0.1)1> riak_core_util:chash_key({<<"rc_example">>, term_to_binary(os:timestamp())}).
<<233,235,224,243,192,63,109,102,255,125,189,206,164,247,
  117,34,94,199,14,184>>
(rc_example@127.0.0.1)2> K1 = riak_core_util:chash_key({<<"rc_example">>, term_to_binary(os:timestamp())}).
<<190,175,151,200,144,123,229,205,94,16,209,140,252,108,
  247,20,238,31,6,82>>
(rc_example1@127.0.0.1)3> riak_core_apl:get_primary_apl(K1, 1, rc_example).
[{{1096126227998177188652763624537212264741949407232,
   'rc_example@127.0.0.1'},
  primary}]
(rc_example1@127.0.0.1)4> K2 = riak_core_util:chash_key({<<"rc_example">>, term_to_binary(os:timestamp())}).
<<113,53,13,80,4,131,62,95,63,164,211,74,145,83,189,77,
  254,224,190,198>>
(rc_example@127.0.0.1)5> riak_core_apl:get_primary_apl(K2, 1, rc_example).
[{{662242929415565384811044689824565743281594433536,
   'rc_example@127.0.0.1'},
  primary}]
(rc_example@127.0.0.1)6> riak_core_apl:get_primary_apl(K2, 3, rc_example).
[{{662242929415565384811044689824565743281594433536,
   'rc_example@127.0.0.1'},
  primary},
 {{685078892498860742907977265335757665463718379520,
   'rc_example@127.0.0.1'},
  primary},
 {{707914855582156101004909840846949587645842325504,
   'rc_example@127.0.0.1'},
  primary}]
```

We get different partitions every time, always on the same physical
node (because we're still running a single one).

The last line of `ping/0` sends the `ping` command to the selected
vnode through the `riak_core_vnode_master`. The function used to do so
is `sync_spawn_command`, which acts a bit like a `gen_server:call` in
the sense that it blocks the calling process waiting for the
response. There are other functions to send commands to a vnode:
`riak_core_vnode_master:command/3` (which works asynchronously like
`gen_server:cast`) and `riak_core_vnode_master:sync_command/3` (which
is like `sync_spawn_command` but blocks the vnode_master process).

You can find more details of the functions used in this
section [here](http://efcasado.github.io/riak-core_intro/). To wrap up
let's run our `ping` function from the shell:

``` erlang
(rc_example@127.0.0.1)1> rc_example:ping().
10:19:00.903 [info] Received ping command 479555224749202520035584085735030365824602865664
{pong,479555224749202520035584085735030365824602865664}
(rc_example@127.0.0.1)2> rc_example:ping().
10:19:01.503 [info] Received ping command 479555224749202520035584085735030365824602865664
{pong,502391187832497878132516661246222288006726811648}
```

### 3. Setting up the cluster

At this point we can execute a simple command, but none of the previous
effort would make any sense if we keep running stuff on a single
node. The whole point of riak_core is to distribute work in a
fault-tolerant and decetralized manner. In this section we'll update
our configuration so we can run our
project in a three-node Erlang cluster. For practical reasons all of
the nodes will reside on our local machine, but moving them to separate
servers should fairly simple.

If you review our codebase you'll note that the one spot that
has fixed node configuration is `conf/vm.args`, where we set the
node name to `rc_example@127.0.0.1`. We want to have `rc_example1`,
`rc_example2` and `rc_example3` instead. We'll be running our
nodes in the same machine so we also need to use different ports for
riak_core in each node (the `web_port` and `handoff_port` tuples in `conf/sys.config`).

Since we'll have an almost identical configuration in all of the nodes,
we'll use the overalys feature that rebar3 inherits from relx. You can
read about
it [here](https://www.rebar3.org/docs/releases#section-overlays),
although it's not strictly necessary for the
purposes of this tutorial. First we tell rebar3 that `conf/sys.config`
and `conf/vm.args` should be treated as templates by adding an
`overlay` tuple in the `relx` configuration:

``` erlang
{overlay, [{template, "conf/sys.config", "releases/{{default_release_version}}/sys.config"},
           {template, "conf/vm.args", "releases/{{default_release_version}}/vm.args"}]}
```

The template variables' values will be taken from `overlay_vars` files. We will
define three
different [rebar profiles](https://www.rebar3.org/docs/profiles) in
`rebar.config`, each pointing to a different `overaly_vars` file:

``` erlang
{profiles, [{dev1, [{relx, [{overlay_vars, "conf/vars_dev1.config"}]}]},
            {dev2, [{relx, [{overlay_vars, "conf/vars_dev2.config"}]}]},
            {dev3, [{relx, [{overlay_vars, "conf/vars_dev3.config"}]}]}]}
```

 Now create
`vars_dev1.config`, `vars_dev2.config` and `vars_dev3.config` in the
`conf` directory as follows:

``` erlang
%% conf/vars_dev1.config
{node, "rc_example1@127.0.0.1"}.

{web_port,          8198}.
{handoff_port,      8199}.

%% conf/vars_dev2.config
{node, "rc_example2@127.0.0.1"}.

{web_port,          8298}.
{handoff_port,      8299}.

%% conf/vars_dev3.config
{node, "rc_example3@127.0.0.1"}.

{web_port,          8398}.
{handoff_port,      8399}.
```

Lastly, update `sys.config` and `vm.args` to refer to template
variables instead of concrete values:

``` erlang
%% conf/sys.config
[{riak_core,
  [{ring_state_dir, "./data/ring"},
   {web_port, {{web_port}}},
   {handoff_port, {{handoff_port}}},
   {schema_dirs, ["lib/rc_example-0.1.0/priv"]}]}].

%% conf/vm.args
-name {{node}}
```

To run the release we need to tell rebar which profile to use, for
example:

``` shell
rebar3 as dev1 release && _build/dev1/rel/rc_example/bin/rc_example
```

Let's add a Makefile to easily run any of the nodes:

``` makefile
.PHONY: dev1 dev2 dev3 clean_data

dev1:
	rebar3 as dev1 release && _build/dev1/rel/rc_example/bin/rc_example

dev2:
	rebar3 as dev2 release && _build/dev2/rel/rc_example/bin/rc_example

dev3:
	rebar3 as dev3 release && _build/dev3/rel/rc_example/bin/rc_example

clean_data:
	rm -rf _build/dev1/rel/rc_example/data* ; rm -rf _build/dev2/rel/rc_example/data* ; rm -rf _build/dev3/rel/rc_example/data*
```

We also include a `clean_data` target, for the cases when we want to start
with a fresh cluster (riak_core persists cluster
information between runs, so you may need to remove it when you make
changes to your configuration).

Before testing our cluster, let's add a function to inspect its status
in `src/rc_example.erl`:

``` erlang
-export([ping/0,
         ring_status/0]).

ring_status() ->
  {ok, Ring} = riak_core_ring_manager:get_my_ring(),
  riak_core_ring:pretty_print(Ring, [legend]).
```

Now open three terminals and run one of these commands on each:

``` shell
$ make dev1
$ make dev2
$ make dev3
```

If you try the `ring_status` function, you'll see something like:

```
(rc_example1@127.0.0.1)1> rc_example:ring_status().
==================================== Nodes ====================================
Node a: 64 (100.0%) rc_example1@127.0.0.1
==================================== Ring =====================================
aaaa|aaaa|aaaa|aaaa|aaaa|aaaa|aaaa|aaaa|aaaa|aaaa|aaaa|aaaa|aaaa|aaaa|aaaa|aaaa|
ok
```

Each node only knows about itself. We can fix that by making node 2 and
3 join node 1. `riak_core:join` is used for a single node to join a
cluster:

``` erlang
%% node 2
(rc_example2@127.0.0.1)1> riak_core:join('rc_example1@127.0.0.1').
18:46:45.409 [info] 'rc_example2@127.0.0.1' changed from 'joining' to 'valid'

%% node 3
(rc_example3@127.0.0.1)1> riak_core:join('rc_example1@127.0.0.1').
18:46:47.120 [info] 'rc_example3@127.0.0.1' changed from 'joining' to 'valid'
```

Now `ring_status()` should show the three nodes with a third of the
keyspace each (it may take some seconds for the percentages to settle):

``` erlang
(rc_example1@127.0.0.1)2> rc_example:ring_status().
==================================== Nodes ====================================
Node a: 21 ( 32.8%) rc_example1@127.0.0.1
Node b: 22 ( 34.4%) rc_example2@127.0.0.1
Node c: 21 ( 32.8%) rc_example3@127.0.0.1
==================================== Ring =====================================
abcc|abcc|abcc|abcc|abcc|abcc|abcc|abcc|abcc|abcc|abbc|abba|abba|abba|abba|abba|
ok
```

If you call `rc_example:ping()` a couple of times, you should see that
the log output (`received ping command`) is printed in a different
terminal every time, because vnodes from any of the physical nodes can
receive the command.

### 4. Building a distributed Key/Value store

Now that we have the project layout and and distribution setup we can
start working on our in-memory Key/Value store. As you may imagine,
this means modifying our worker vnode to support a new set of
commands: `put`, `get` and `delete`. Here are the relevant parts:

TODO

* add state to vnode
* add new handle_command clauses

* add new functions to rc_example api
* test the new functions in the shell

As you can see, even if the initial setup can be a little burdensome,
you can get distribution of work and fault-tolerance with little
effort, by just adding your application-specific logic to a vnode module.

### 5. Testing

TODO #7

### 6. Coverage commands

keys, values
explain the gotcha of breaking change in the coverage fsm

add a test

### 7. Handoff

explain what we've learned digging the code comments

### 8. Redundancy and fault-tolerance

TODO #6
