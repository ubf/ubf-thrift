%%% Description: thrift contract parser
%%%-------------------------------------------------------------------

-module(thrift_contract_parser).
-include("ubf_impl.hrl").
-include("thrift_impl.hrl").

-export([parse_transform/2]).

%%====================================================================
%% External Parse Transform
%%====================================================================

parse_transform(In, _Opts) ->
    %% io:format("In:~p~n   Opts: ~p~n",[In, _Opts]),
    Name = case [X || {attribute, _, module, X} <- In] of [M] -> atom_to_list(M) end,
    VSN = case [X || {attribute, _, vsn, X} <- In] of [V] -> V; _ -> "" end,
    Imports = [X || {attribute, _, add_types, X} <- In],
    Out = case [X || {attribute, _, add_contract, X} <- In] of
              [File] ->
                  case file(Name, VSN, Imports, File ++ infileExtension()) of
                      {ok, Contract, _Header} ->
                          %% io:format("Contract added: ~p~n", [Contract]),
                          contract_parser:parse_transform_contract(In, Contract);
                      {error, Why} ->
                          io:format("Error in contract:~p~n", [Why]),
                          erlang:error(Why)
                  end;
              [] ->
                  In
          end,
    Out.


%%====================================================================
%% External API
%%====================================================================


%%====================================================================
%% Internal functions
%%====================================================================

infileExtension()  -> ".thrift".

file(_Name, _VSN, _Imports, F) ->
    %% io:format("~n~p ~p~n~p~n~p~n", [Name, VSN, Imports, F]),
    case file:read_file(F) of
        {ok, _Bin} ->
            exit(notimplemented);
            %% case thriftc:parse(binary_to_list(Bin), []) of
            %%     {ok, AST, _Rest} ->
            %%         Types = ast2ubf(thriftc_ast:ast_to_int_form(AST)),
            %%         %% io:format("~n~p~n", [Types]),
            %%         contract_parser:tags([{name,Name}, {vsn,VSN}, {types,Types}], Imports);
            %%     Err ->
            %%         Err
            %% end;
        Err ->
            Err
    end.
