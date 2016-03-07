-module(etcd_test_helper).

-export([
  unique_key/0
]).

unique_key() ->
  iolist_to_binary([
        "/tests/",
        lists:flatten([[io_lib:format("~2.16.0B",[X]) || <<X:8>> <= crypto:rand_bytes(20)]])
      ]).