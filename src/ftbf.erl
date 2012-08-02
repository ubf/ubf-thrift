
-module(ftbf).
-behaviour(contract_proto).

-include_lib("ubf/include/ubf.hrl").

-export([proto_vsn/0, proto_driver/0, proto_packet_type/0]).
-export([encode/1, encode/2, encode/3]).
-export([decode_init/0, decode_init/1, decode_init/2, decode/1, decode/2, decode/3]).

-export([atom_to_binary/1]).
-export([binary_to_atom/1, binary_to_existing_atom/1]).

%% Dummy hack/kludge.
-export([contract_records/0]).

contract_records() ->
    [].


%%
%%---------------------------------------------------------------------
%%
proto_vsn()         -> 'ftbf1.0'.
proto_driver()      -> ftbf_driver.
proto_packet_type() -> 4.


%%
%%---------------------------------------------------------------------
%%
encode(X) ->
    tbf:encode(X).

encode(X, Mod) ->
    tbf:encode(X, Mod).

encode(X, Mod, VSN) ->
    tbf:encode(X, Mod, VSN).

decode(X) ->
    tbf:decode(X).

decode(X, Mod) ->
    tbf:decode(X, Mod).

decode(X, Mod, Cont) ->
    tbf:decode(X, Mod, Cont).

decode_init() ->
    tbf:decode_init().

decode_init(Safe) ->
    tbf:decode_init(Safe).

decode_init(Safe, Binary) ->
    tbf:decode_init(Safe, Binary).


%%
%%---------------------------------------------------------------------
%%
atom_to_binary(X) ->
    tbf:atom_to_binary(X).

binary_to_atom(X) ->
    tbf:binary_to_atom(X).

binary_to_existing_atom(X) ->
    tbf:binary_to_existing_atom(X).
