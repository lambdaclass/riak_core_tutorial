# riak_core example

A basic example of a riak_core application, using with the most
recent version of the [riak_core_ng fork](https://hex.pm/packages/riak_core_ng)
and running on Erlang/OTP 20 with rebar3.

This example was largely based in the
[Little Riak Core Book](https://marianoguerra.github.io/little-riak-core-book/)
and the
[Create a riak_core application in Elixir](https://medium.com/@GPad/create-a-riak-core-application-in-elixir-part-1-41354c1f26c3)
series.

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
