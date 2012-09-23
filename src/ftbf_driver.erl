%% @doc Protocol driver process for FTBF (Framed Thrift Binary Format)
%% protocol sessions.

-module(ftbf_driver).
-behaviour(contract_driver).

-export([start/1, start/2, init/1, init/2, encode/3, decode/4]).

-define(VSN_1, 16#80010000).

start(Contract) ->
    start(Contract, []).

start(Contract, Options) ->
    proc_utils:spawn_link_debug(fun() -> contract_driver:start(?MODULE, Contract, Options) end, ftbf_client_driver).

init(Contract) ->
    init(Contract, []).

init(_Contract, Options) ->
    Safe = safe(Options),
    {Safe, ftbf:decode_init(Safe)}.

encode(Contract, _Safe, Term) ->
    case get(?MODULE) of
        undefined ->
            ftbf:encode(Term, Contract, ?VSN_1);
        Vsn ->
            ftbf:encode(Term, Contract, Vsn)
    end.

decode(Contract, Safe, {init, Rest, Vsn}, Binary) ->
    put(?MODULE, Vsn),
    Cont = ftbf:decode_init(Safe, Rest),
    ftbf:decode(Binary, Contract, Cont);
decode(Contract, _Safe, Cont, Binary) ->
    ftbf:decode(Binary, Contract, Cont).

safe(Options) ->
    proplists:get_bool(safe, Options).
