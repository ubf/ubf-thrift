%%% -*- mode: erlang -*-
%%% $Id$
%%% @doc Sample Thrift contract.
%%%
%%%

-module(ubf_thrift_plugin).

%% Required callback API for all UBF contract implementations.
-export([info/0, description/0, keepalive/0]).
-export([handlerStart/1, handlerStop/3, handlerRpc/1]).

-compile({parse_transform,contract_parser}).
-add_contract("ubf_thrift_plugin").

-include("ubf.hrl").

info() ->
    "I am a Thrift server".

description() ->
    "A Thrift server programmed by UBF".

keepalive() ->
    ok.


%% @spec handlerStart(Args::list(any())) ->
%%          {accept, Reply::any(), StateName::atom(), StateData::term()} | {reject, Reason::any()}
%% @doc start handler
handlerStart(_Args) ->
    {accept,ok,none,unused}.

%% @spec handlerStop(Pid::pid(), Reason::any(), StateData::term()) -> void()
%% @doc stop handler
handlerStop(_Pid, _Reason, _StateData) ->
    unused.


%% @spec handlerRpc(Event::any()) ->
%%          Reply::any()
%% @doc rpc handler
handlerRpc({Method, SeqId, {'struct',_,_}=Arg})
  when is_binary(Method), is_integer(SeqId) ->
    %% @TODO add your own implementation here
    io:format("call method=~p seqid=~p struct=~p~n", [Method, SeqId, Arg]),
    Reply = Arg, %% Let's fake it and echo the request
    {'T-REPLY', Reply}; %% or {'T-EXCEPTION', Reply}.
handlerRpc(Event)
  when Event==info; Event==description ->
    ?S(?MODULE:Event());
handlerRpc(Event)
  when Event==keepalive ->
    ?MODULE:Event();
handlerRpc(Event) ->
    {Event, not_implemented}.
