%%% -*- mode: erlang -*-
%%%

{application, thrift_plugin,
 [
  {description, "THRIFT_PLUGIN"},
  {vsn, "0.01"},
  {id, "THRIFT_PLUGIN"},
  {modules, [
             %% TODO: fill in this list, perhaps
            ]
  },
  {registered, [ ] },
  %% NOTE: do not list applications which are load-only!
  {applications, [ kernel, stdlib, sasl ] },
  {mod, {thrift_plugin_app, []} }
 ]
}.
