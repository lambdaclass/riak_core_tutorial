# Using riak_core in 2018

## Background

- explain what is riak core and why it's useful
- mention basho in receivership, current
- mention that documetation is scarce, include useful links: basho
  where to start, rc wiki,
  trytrytry, understanding rc, little rc book, elixir tutorial, slides links.
- explain the rationale for this tutorial: little rc book and rebar3
  template have a lot of dependencies and doesn't work with E20;
  closer to elixir tutorial but for erlang; document details we've
  learned along the way
- explain the general structure is directly borrowed from those two
  tutorials and will link to them when appropriate

## 1. setup

specially detail patches required to make it run on Erlang 20
(warnings as errors, etc.)

general project structure (no magic, maybe suggest using rebar3
template as a reference)

## 2. vnode

link to an explanation of the vnode

start with an empty one, add a ping command

## 3. setup the cluster, basic

explain what the makefile does, (no magic!)

basic console/cluster status commands
TODO #4

## 4. in memory key value store

put, get, delete

## 5. Testing

TODO #7

## 5. coverage commands

keys, values
explain the gotcha of breaking change in the coverage fsm

add a test

## 6. handoff

explain what we've learned digging the code comments

## 7. redundancy and fault tolerance

TODO #6
