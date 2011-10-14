%% @doc Protocol driver process for TBF (Thrift Binary Format)
%% protocol sessions.

-module(tbf_driver).
-behaviour(contract_driver).

-export([start/1, start/2, init/1, init/2, encode/3, decode/5]).

-define(VSN_1, 16#80010000).

start(Contract) ->
    start(Contract, []).

start(Contract, Options) ->
    proc_utils:spawn_link_debug(fun() -> contract_driver:start(?MODULE, Contract, Options) end, tbf_client_driver).

init(Contract) ->
    init(Contract, []).

init(_Contract, Options) ->
    Safe = safe(Options),
    {Safe, tbf:decode_init(Safe)}.

encode(Contract, _Safe, Term) ->
    case get(?MODULE) of
        undefined ->
            tbf:encode(Term, Contract, ?VSN_1);
        Vsn ->
            tbf:encode(Term, Contract, Vsn)
    end.

decode(Contract, Safe, Cont, Binary, CallBack) ->
    Cont1 = tbf:decode(Binary, Contract, Cont),
    decode(Contract, Safe, Cont1, CallBack).

decode(Contract, Safe, {ok, Term, Binary, VSN}=_Cont, CallBack) ->
    put(?MODULE, VSN),
    CallBack(Term),
    Cont1 = tbf:decode_init(Safe, Binary),
    decode(Contract, Safe, Cont1, CallBack);
decode(_Contract, _Safe, Cont, _CallBack) ->
    Cont.

safe(Options) ->
    proplists:get_bool(safe, Options).
