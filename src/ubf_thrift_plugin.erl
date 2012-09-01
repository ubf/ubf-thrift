%%% -*- mode: erlang -*-
%%% @doc Sample Thrift contract.
%%%
%%%

-module(ubf_thrift_plugin).
-behaviour(ubf_plugin_stateless).

%% Required callback API for all UBF contract implementations.
-export([info/0, description/0, keepalive/0]).
-export([moduleStart/1, moduleRestart/1]).
-export([handlerStart/1, handlerStop/3, handlerRpc/1, handlerEvent/1]).

-import(ubf_plugin_handler, [sendEvent/2, install_handler/2]).

-compile({parse_transform,contract_parser}).
-add_contract("src/ubf_thrift_plugin").

-include_lib("ubf/include/ubf.hrl").

info() ->
    "I am a Thrift server".

description() ->
    "A Thrift server programmed by UBF".

keepalive() ->
    ok.

%% @doc start module
moduleStart(_Args) ->
    unused.

%% @doc restart module
moduleRestart(Args) ->
    moduleStart(Args).

%% @spec handlerStart(Args::list(any())) ->
%%          {accept, Reply::any(), StateName::atom(), StateData::term()} | {reject, Reason::any()}
%% @doc start handler
handlerStart(_Args) ->
    ack = install_handler(self(), fun handlerEvent/1),
    {accept,ok,none,unused}.

%% @spec handlerStop(Pid::pid(), Reason::any(), StateData::term()) -> none()
%% @doc stop handler
handlerStop(_Pid, _Reason, _StateData) ->
    unused.


%% @spec handlerRpc(Event::any()) ->
%%          Reply::any()
%% @doc rpc handler
handlerRpc({'message', Name, 'T-CALL', SeqId, {'struct',_,_}=Arg})
  when is_binary(Name), is_integer(SeqId) ->
    %% io:format("call method=~p seqid=~p struct=~p~n", [Name, SeqId, Arg]),
    %% @TODO add your own implementation here
    Reply = Arg, %% Let's fake it and echo the request
    {'message', Name, 'T-REPLY', SeqId, Reply}; %% or 'T-EXCEPTION'
handlerRpc(Event)
  when Event==info; Event==description ->
    ?S(?MODULE:Event());
handlerRpc(Event)
  when Event==keepalive ->
    ?MODULE:Event();
handlerRpc(Event) ->
    {Event, not_implemented}.

handlerEvent(Event) ->
    %% @TODO add your own implementation here
    %% Let's fake it and echo the request
    sendEvent(self(), Event),
    fun handlerEvent/1.

