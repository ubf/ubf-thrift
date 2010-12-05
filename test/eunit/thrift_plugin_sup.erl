%%%----------------------------------------------------------------------
%%% File    : thrift_plugin_sup.erl
%%% Purpose : test UBF top-level supervisor
%%%----------------------------------------------------------------------

-module(thrift_plugin_sup).

-behaviour(supervisor).

%% External exports
-export([start_link/1]).

%% supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link(_Args) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%----------------------------------------------------------------------
%%% Callback functions from supervisor
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}
%%----------------------------------------------------------------------
%% @spec(Args::term()) -> {ok, {supervisor_flags(), child_spec_list()}}
%% @doc The main TEST UBF supervisor.

init(Args) ->
    %% seq_trace:set_token(send, true), seq_trace:set_token('receive', true),

    %% Child_spec = [Name, {M, F, A},
    %%               Restart, Shutdown_time, Type, Modules_used]

    DefaultMaxConn = 10000,
    DefaultTimeout = 60000,
    DefaultPlugins = proplists:get_value(plugins, Args, [ubf_thrift_plugin]),

    CTBF = case proplists:get_value(test_tbf_tcp_port, Args, 0) of
               undefined ->
                   [];
               TBFPort ->
                   TBFMaxConn = proplists:get_value(test_tbf_maxconn, Args, DefaultMaxConn),
                   TBFIdleTimer = proplists:get_value(test_tbf_timeout, Args, DefaultTimeout),
                   TBFOptions = [{statelessrpc,true}               %% mandatory for thrift
                                 , {startplugin,ubf_thrift_plugin} %%          "
                                 , {serverhello,undefined}         %%          "
                                 , {simplerpc,true}                %%          "
                                 , {proto,tbf}                     %%          "
                                 , {maxconn,TBFMaxConn}
                                 , {idletimer,TBFIdleTimer}
                                 , {registeredname,test_tbf_tcp_port}
                                ],
                   TBFServer =
                       {tbf_server, {ubf_server, start_link, [test_tbf, DefaultPlugins, TBFPort, TBFOptions]},
                        permanent, 2000, worker, [tbf_server]},

                   [TBFServer]
           end,

    {ok, {{one_for_one, 2, 60}, CTBF}}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------
