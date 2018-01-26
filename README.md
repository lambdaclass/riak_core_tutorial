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

## Usage
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
applications. What makes Riak Core so interesting and useful is that it implements
the ideas of the
[Amazon's Dyamo](https://en.wikipedia.org/wiki/Dynamo_(storage_system))
architecture and exposes its infrastructure as
a reusable library, allowing to easily apply them in any context that
can benefit from decentralized distribution of work (including but not
limited to data stores).

In practical terms, Riak Core is an Erlang/OTP application, and most
of the user defined work is done in the `riak_core_vnode` behavior.

### About this tutorial

Basho, the company that originally developed Riak and Riak Core was
put into receivership in 2017. This introduces some uncertainty about the
future of these products, although the community has shown interest in
taking over their maintenance. At the moment of writing,
that [riak_core_ng](https://github.com/Kyorai/riak_core) fork seems to
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
value. In the context of a Key/Value store, for example, the
identifier is the Key used in get, put and delete operations.

Before performing the operation, a hashing function is applied to
the key. The hashed value will be used to decide what node of the
cluster should be responsible of executing the operation. The range of
all possible values the hashed key can take (the keyspace, usually
depicted as a ring), is partitioned in equally sized buckets, which
are called virtual nodes (vnodes).

![The Ring](ring.png)

The amount of vnodes is fixed and a given hash value will always
belong to the same partition (i.e. the same vnode). The vnodes, in
turn, are evenly distributed across all available physycal
nodes. Note this distribution isn't fixed as the keyspace partitioning
is: the vnode distribution can change if a physical node is added
to the cluster or a one goes down.

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
{deps, [{riak_core, "3.0.9", {pkg, riak_core_ng}},
        {lager, "3.5.1"}]}.
```

Note we're using
the [riak_core_ng fork](https://hex.pm/packages/riak_core_ng), which is
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
export_all or gen_fsm). To fix this we need to override their
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
development in rebar.config:

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

`vm.args` just sets the node name; in `sys.confg` we set a data
directory for riak core (`ring_state_dir`) and a
couple of ports; we also need to point riak to its schema (by setting
`schema_dirs`). For this to work we have to copy
[this file](https://github.com/Kyorai/riak_core/blob/fifo-merge/priv/riak_core.schema)
to `priv/riak_core.schema`.

At this point we should have a runnable release (if you see errors,
try removing the _build directory):

    $ rebar3 release && _build/default/rel/rc_example/bin/rc_example

### 2. vnode

link to an explanation of the vnode

start with an empty one, add a ping command

### 3. setup the cluster, basic console commands

add overlays and configuration for different nodes

explain what the makefile does, (no magic!)

basic console/cluster status commands
TODO #4

### 4. in memory key value store

put, get, delete

### 5. Testing

TODO #7

### 6. coverage commands

keys, values
explain the gotcha of breaking change in the coverage fsm

add a test

### 7. handoff

explain what we've learned digging the code comments

### 8. redundancy and fault tolerance

TODO #6
