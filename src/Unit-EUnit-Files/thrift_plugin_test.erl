-module(thrift_plugin_test).

-compile(export_all).
-include("eunit.hrl").
-include("ubf.hrl").

do_eunit() ->
    case eunit:test({timeout,120,?MODULE}) of
        ok -> ok;
        _ -> erlang:halt(1)
    end.

-define(APPLICATION, thrift_plugin).
-define(TBF_PORT, server_port(test_tbf_tcp_port)).

-define(SLEEP, 50).

-record(args, {host, port}).


%%%----------------------------------------------------------------------
%%% TESTS
%%%----------------------------------------------------------------------

all_tests_test_() ->
    all_tests_(fun () -> test_setup(?APPLICATION) end,
               fun (X) -> test_teardown(X) end
              ).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

all_tests_(Setup,Teardown) ->
    {setup,
     Setup,
     Teardown,
     (all_actual_tests_())(not_used)
    }.

all_actual_tests_() ->
    all_actual_tests_("localhost",fun() -> ?TBF_PORT end).

all_actual_tests_(Host,Port) ->
    fun(_) ->
            [?_test(test_001(#args{host=Host,port=Port()}))
             , ?_test(test_002(#args{host=Host,port=Port()}))
             , ?_test(test_003(#args{host=Host,port=Port()}))
             , ?_test(test_004(#args{host=Host,port=Port()}))
            ]
    end.

%%%----------------------------------------------------------------------
%%% Internal
%%%----------------------------------------------------------------------

test_setup(App) ->
    application:start(sasl),
    application:stop(App),
    ok = application:start(App),
    App.

test_teardown(App) ->
    application:stop(App),
    ok.

%% connect -> close
test_001(#args{host=Host,port=Port}) ->
    {ok,Sock} = gen_tcp:connect(Host,Port,[]),
    ok = gen_tcp:close(Sock).

%% connect -> shutdown(X) -> close
test_002(Args) ->
    test_002(Args,read),
    test_002(Args,write),
    test_002(Args,read_write).

test_002(#args{host=Host,port=Port},How) ->
    {ok,Sock} = gen_tcp:connect(Host,Port,[]),
    ok = gen_tcp:shutdown(Sock, How),
    ok = gen_tcp:close(Sock).

%% connect -> close
test_003(#args{}=Args) ->
    {ok,Pid1} = client_connect(Args),
    client_stop(Pid1),
    {ok,Pid2} = client_connect(Args),
    client_stop(Pid1),
    {reply,ok} = client_rpc(Pid2,keepalive),
    client_stop(Pid2).

%% connect -> call -> echo reply -> close
test_004(#args{}=Args) ->
    {ok,Pid1} = client_connect(Args),

    Struct = {'struct', <<"bool">>, [{'field', <<>>, 'T-BOOL', 1, true}]},
    Call = {'message', <<"test_004">>, 'T-CALL', 1, Struct},
    {reply,Reply} = client_rpc(Pid1,Call),
    Reply = {'message', <<"test_004">>, 'T-REPLY', 1, Struct},

    client_stop(Pid1).


%%%----------------------------------------------------------------------
%%% Helpers
%%%----------------------------------------------------------------------

server_port(Name) ->
    case proc_socket_server:server_port(Name) of
        Port when is_integer(Port) ->
            Port;
        _ ->
            timer:sleep(10),
            server_port(Name)
    end.


client_connect(#args{host=Host,port=Port}) ->
    Options = [{proto,tbf},{serverlessrpc,true},{serverhello,undefined},{simplerpc,true}],
    {ok,Pid,undefined} = ubf_client:connect(Host,Port,Options,infinity),
    {ok,Pid}.


client_rpc(X,Y) ->
    client_rpc(X,Y,infinity).


client_rpc(Pid,Args,Timeout) ->
    ubf_client:rpc(Pid,Args,Timeout).


client_stop(Pid) ->
    ubf_client:stop(Pid).
